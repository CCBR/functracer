#' Analyze Direct and Transitive R Function Dependencies
#'
#' Build a dependency map starting from an entry script and tracing into a
#' target package source directory.
#'
#' @param entry_script Path to the entry R script.
#' @param package_dir Path to a package root directory containing `R/` and
#'   `NAMESPACE`.
#' @param package_name Optional package name. If `NULL`, inferred from
#'   `DESCRIPTION`.
#' @param output_dir Directory where output artifacts are written.
#' @param output_prefix Prefix for output files. Defaults to entry script stem.
#'
#' @return A list containing paths to output files and in-memory data frames.
#' @export
analyze_dependencies <- function(
  entry_script,
  package_dir,
  package_name = NULL,
  output_dir = ".",
  output_prefix = NULL
) {
  entry_script <- normalizePath(entry_script, winslash = "/", mustWork = FALSE)
  package_dir <- normalizePath(package_dir, winslash = "/", mustWork = FALSE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)

  package_r_dir <- file.path(package_dir, "R")
  namespace_file <- file.path(package_dir, "NAMESPACE")
  description_file <- file.path(package_dir, "DESCRIPTION")

  if (!file.exists(entry_script)) {
    stop("Cannot find entry script: ", entry_script)
  }
  entry_script <- normalizePath(entry_script, winslash = "/", mustWork = TRUE)
  package_dir <- normalizePath(package_dir, winslash = "/", mustWork = TRUE)

  if (!dir.exists(package_r_dir)) {
    stop("Cannot find package R directory: ", package_r_dir)
  }
  if (!file.exists(namespace_file)) {
    stop("Cannot find package NAMESPACE: ", namespace_file)
  }

  if (is.null(package_name)) {
    package_name <- infer_package_name(description_file)
  }

  if (is.null(output_prefix) || identical(output_prefix, "")) {
    output_prefix <- tools::file_path_sans_ext(basename(entry_script))
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  output_csv <- file.path(
    output_dir,
    paste0(output_prefix, "_dependencies.csv")
  )
  output_json <- file.path(
    output_dir,
    paste0(output_prefix, "_dependencies.json")
  )
  output_svg <- file.path(
    output_dir,
    paste0(output_prefix, "_dependencies.svg")
  )

  exported_functions <- extract_exported_functions(namespace_file)

  direct_calls_result <- extract_direct_calls_from_script(entry_script)
  direct_calls <- direct_calls_result$calls

  entry_parsed <- direct_calls_result$parsed_exprs
  entry_fn_map <- extract_function_definitions(entry_parsed)
  entry_local_functions <- names(entry_fn_map)

  pkg_map_result <- build_package_dependency_map(
    r_dir = package_r_dir,
    known_package_name = package_name
  )
  dep_map <- pkg_map_result$dep_map
  known_package_functions <- pkg_map_result$function_names

  for (local_fn in entry_local_functions) {
    local_deps <- character(0)
    local_bodies <- entry_fn_map[[local_fn]]
    for (body_expr in local_bodies) {
      call_df <- extract_calls_from_expr(body_expr)
      local_deps <- unique(c(
        local_deps,
        call_df$call_name[
          call_df$call_name %in%
            c(known_package_functions, entry_local_functions)
        ]
      ))
    }
    local_deps <- local_deps[local_deps != local_fn]
    dep_map[[local_fn]] <- local_deps
  }

  direct_pkg <- unique(direct_calls$function_name[
    direct_calls$package == package_name |
      (is.na(direct_calls$package) &
        direct_calls$function_name %in% known_package_functions)
  ])

  direct_entry_local <- unique(direct_calls$function_name[
    is.na(direct_calls$package) &
      direct_calls$function_name %in% entry_local_functions
  ])

  roots <- unique(c(direct_pkg, direct_entry_local))
  roots <- roots[roots %in% names(dep_map)]

  transitive_df <- compute_transitive_dependencies(
    roots = roots,
    dep_map = dep_map,
    entry_label = basename(entry_script)
  )

  if (nrow(transitive_df) > 0) {
    transitive_df$source <- ifelse(
      transitive_df[["function"]] %in% entry_local_functions,
      basename(entry_script),
      package_name
    )
    transitive_df$is_exported <- transitive_df[["function"]] %in%
      exported_functions
    transitive_df <- transitive_df[
      order(
        transitive_df$hop_depth,
        transitive_df$dep_type,
        transitive_df[["function"]]
      ),
    ]
    rownames(transitive_df) <- NULL
  }

  readr::write_csv(transitive_df, output_csv)

  json_payload <- list(
    metadata = list(
      generated_at = as.character(Sys.time()),
      parser = direct_calls_result$parser_used,
      entry_script = entry_script,
      package_dir = package_dir,
      package_name = package_name,
      roots = roots
    ),
    direct_calls = direct_calls,
    dependencies_by_depth = split(transitive_df, transitive_df$hop_depth)
  )

  jsonlite::write_json(
    json_payload,
    output_json,
    auto_unbox = TRUE,
    pretty = TRUE
  )

  create_dependency_graph(
    dep_rows = transitive_df,
    dep_map = dep_map,
    root_functions = roots,
    entry_label = basename(entry_script),
    output_path = output_svg
  )

  list(
    csv = output_csv,
    json = output_json,
    svg = output_svg,
    roots = roots,
    direct_calls = direct_calls,
    dependencies = transitive_df
  )
}

#' Trace Functions and Print Output Paths
#'
#' @inheritParams analyze_dependencies
#' @return Invisibly returns the same list as [analyze_dependencies()].
#' @export
trace_functions <- function(
  entry_script,
  package_dir,
  package_name = NULL,
  output_dir = ".",
  output_prefix = NULL
) {
  result <- analyze_dependencies(
    entry_script = entry_script,
    package_dir = package_dir,
    package_name = package_name,
    output_dir = output_dir,
    output_prefix = output_prefix
  )

  message("Dependency analysis complete")
  message("CSV: ", result$csv)
  message("JSON: ", result$json)
  message("SVG: ", result$svg)

  invisible(result)
}

infer_package_name <- function(description_file) {
  if (!file.exists(description_file)) {
    stop(
      "Cannot infer package name because DESCRIPTION is missing: ",
      description_file
    )
  }

  desc <- read.dcf(description_file)
  if (!"Package" %in% colnames(desc)) {
    stop("DESCRIPTION does not contain a Package field: ", description_file)
  }

  pkg <- desc[1, "Package"]
  if (!nzchar(pkg)) {
    stop("DESCRIPTION has an empty Package field: ", description_file)
  }

  pkg
}

get_treesitter_language <- function() {
  if (!requireNamespace("treesitter", quietly = TRUE)) {
    stop("Package 'treesitter' is required but not installed")
  }
  if (!requireNamespace("treesitter.r", quietly = TRUE)) {
    stop("Package 'treesitter.r' is required but not installed")
  }

  treesitter.r::language()
}

parse_file_with_treesitter <- function(path) {
  source_text <- readr::read_file(path)
  treesitter::text_parse(source_text, get_treesitter_language())
}

walk_named_nodes <- function(node, visitor) {
  visitor(node)
  children <- treesitter::node_named_children(node)
  if (length(children) == 0) {
    return(invisible(NULL))
  }
  for (child in children) {
    walk_named_nodes(child, visitor)
  }
  invisible(NULL)
}

get_assignment_operator <- function(node) {
  op_node <- treesitter::node_child_by_field_name(node, "operator")
  if (is.null(op_node)) {
    return(NA_character_)
  }
  treesitter::node_text(op_node)
}

get_identifier_text <- function(node) {
  if (is.null(node)) {
    return(NA_character_)
  }
  if (treesitter::node_type(node) != "identifier") {
    return(NA_character_)
  }
  treesitter::node_text(node)
}

namespace_call_parts <- function(node) {
  if (is.null(node) || treesitter::node_type(node) != "namespace_operator") {
    return(NULL)
  }
  lhs <- treesitter::node_child_by_field_name(node, "lhs")
  rhs <- treesitter::node_child_by_field_name(node, "rhs")
  op <- treesitter::node_child_by_field_name(node, "operator")
  list(
    pkg = get_identifier_text(lhs),
    fun = get_identifier_text(rhs),
    op = if (is.null(op)) NA_character_ else treesitter::node_text(op)
  )
}

extract_first_call_argument_identifier <- function(call_node) {
  args_node <- treesitter::node_child_by_field_name(call_node, "arguments")
  if (is.null(args_node)) {
    return(NA_character_)
  }
  arg_nodes <- treesitter::node_named_children(args_node)
  if (length(arg_nodes) == 0) {
    return(NA_character_)
  }
  arg_nodes <- Filter(
    function(x) treesitter::node_type(x) == "argument",
    arg_nodes
  )
  if (length(arg_nodes) == 0) {
    return(NA_character_)
  }
  first_value <- treesitter::node_child_by_field_name(arg_nodes[[1]], "value")
  get_identifier_text(first_value)
}

call_info_from_call_node <- function(call_node, include_line = FALSE) {
  fn_node <- treesitter::node_child_by_field_name(call_node, "function")
  if (is.null(fn_node)) {
    return(NULL)
  }

  fn_type <- treesitter::node_type(fn_node)
  if (fn_type == "identifier") {
    out <- list(
      call_name = treesitter::node_text(fn_node),
      pkg = NA_character_
    )
  } else if (fn_type == "namespace_operator") {
    ns <- namespace_call_parts(fn_node)
    if (is.null(ns) || is.na(ns$fun)) {
      return(NULL)
    }
    out <- list(
      call_name = ns$fun,
      pkg = ns$pkg
    )
  } else {
    return(NULL)
  }

  if (include_line) {
    out$line <- as.integer(
      treesitter::point_row(treesitter::node_start_point(call_node)) + 1
    )
  }

  out
}

extract_exported_functions <- function(namespace_path) {
  namespace_lines <- readLines(namespace_path, warn = FALSE)
  exported <- grep('^export\\(', namespace_lines, value = TRUE)
  exported <- sub('^export\\("?', "", exported)
  exported <- sub('"?\\)$', "", exported)
  unique(exported)
}

get_namespace_call <- function(call_expr) {
  if (!is.call(call_expr) || length(call_expr) < 3) {
    return(NULL)
  }

  op <- if (is.symbol(call_expr[[1]])) {
    as.character(call_expr[[1]])
  } else {
    NA_character_
  }
  if (!(op %in% c("::", ":::"))) {
    return(NULL)
  }

  pkg <- as.character(call_expr[[2]])
  fun <- as.character(call_expr[[3]])
  list(pkg = pkg, fun = fun)
}

extract_calls_from_expr <- function(expr) {
  if (is.null(expr)) {
    return(data.frame(
      call_name = character(),
      pkg = character(),
      stringsAsFactors = FALSE
    ))
  }

  calls <- list()

  walk_named_nodes(expr, function(node) {
    if (treesitter::node_type(node) != "call") {
      return(invisible(NULL))
    }
    info <- call_info_from_call_node(node)
    if (!is.null(info)) {
      calls[[length(calls) + 1]] <<- info
    }
    invisible(NULL)
  })

  if (length(calls) == 0) {
    return(data.frame(
      call_name = character(),
      pkg = character(),
      stringsAsFactors = FALSE
    ))
  }

  unique(as.data.frame(
    do.call(rbind, lapply(calls, as.data.frame)),
    stringsAsFactors = FALSE
  ))
}

extract_function_definitions <- function(parsed_exprs) {
  fn_map <- list()

  append_body <- function(fn_name, body_expr) {
    if (!fn_name %in% names(fn_map)) {
      fn_map[[fn_name]] <<- list()
    }
    fn_map[[fn_name]][[length(fn_map[[fn_name]]) + 1]] <<- body_expr
  }

  walk_named_nodes(parsed_exprs, function(node) {
    if (treesitter::node_type(node) != "binary_operator") {
      return(invisible(NULL))
    }

    op <- get_assignment_operator(node)
    if (!op %in% c("<-", "=")) {
      return(invisible(NULL))
    }

    lhs <- treesitter::node_child_by_field_name(node, "lhs")
    rhs <- treesitter::node_child_by_field_name(node, "rhs")
    if (is.null(lhs) || is.null(rhs)) {
      return(invisible(NULL))
    }

    rhs_type <- treesitter::node_type(rhs)

    if (
      treesitter::node_type(lhs) == "identifier" &&
        rhs_type == "function_definition"
    ) {
      fn_name <- treesitter::node_text(lhs)
      fn_body <- treesitter::node_child_by_field_name(rhs, "body")
      append_body(fn_name, fn_body)
      return(invisible(NULL))
    }

    if (treesitter::node_type(lhs) == "identifier" && rhs_type == "call") {
      rhs_fn <- treesitter::node_child_by_field_name(rhs, "function")
      ns <- namespace_call_parts(rhs_fn)
      if (!is.null(ns) && ns$pkg == "S7" && ns$fun == "new_generic") {
        append_body(treesitter::node_text(lhs), NULL)
      }
      return(invisible(NULL))
    }

    if (
      treesitter::node_type(lhs) == "call" && rhs_type == "function_definition"
    ) {
      lhs_fn <- treesitter::node_child_by_field_name(lhs, "function")
      is_method <- FALSE

      if (!is.null(lhs_fn) && treesitter::node_type(lhs_fn) == "identifier") {
        is_method <- identical(treesitter::node_text(lhs_fn), "method")
      } else {
        ns <- namespace_call_parts(lhs_fn)
        is_method <- !is.null(ns) && ns$fun == "method"
      }

      if (is_method) {
        method_name <- extract_first_call_argument_identifier(lhs)
        if (!is.na(method_name)) {
          fn_body <- treesitter::node_child_by_field_name(rhs, "body")
          append_body(method_name, fn_body)
        }
      }
    }

    invisible(NULL)
  })

  fn_map
}

extract_direct_calls_from_script <- function(script_path) {
  parsed_exprs <- parse_file_with_treesitter(script_path)

  calls <- list()
  walk_named_nodes(parsed_exprs, function(node) {
    if (treesitter::node_type(node) != "call") {
      return(invisible(NULL))
    }
    info <- call_info_from_call_node(node, include_line = TRUE)
    if (!is.null(info)) {
      calls[[length(calls) + 1]] <<- info
    }
    invisible(NULL)
  })

  if (length(calls) == 0) {
    direct_calls <- data.frame(
      line = integer(),
      function_name = character(),
      call_type = character(),
      package = character(),
      stringsAsFactors = FALSE
    )
  } else {
    direct_calls <- data.frame(
      line = as.integer(vapply(calls, function(x) x$line, integer(1))),
      function_name = vapply(calls, function(x) x$call_name, character(1)),
      package = vapply(
        calls,
        function(x) {
          if (is.null(x$pkg) || is.na(x$pkg)) {
            return(NA_character_)
          }
          x$pkg
        },
        character(1)
      ),
      stringsAsFactors = FALSE
    )

    direct_calls$call_type <- ifelse(
      is.na(direct_calls$package),
      "unqualified",
      "namespaced"
    )

    direct_calls <- direct_calls[
      order(direct_calls$line, direct_calls$function_name),
    ]
    rownames(direct_calls) <- NULL
  }

  list(
    calls = direct_calls,
    parser_used = "treesitter-r",
    parsed_exprs = parsed_exprs
  )
}

build_package_dependency_map <- function(
  r_dir,
  known_package_name
) {
  files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  files <- files[order(files)]

  all_functions <- list()
  function_source_file <- list()

  for (path in files) {
    parsed <- parse_file_with_treesitter(path)
    fn_map <- extract_function_definitions(parsed)
    if (length(fn_map) == 0) {
      next
    }
    for (name in names(fn_map)) {
      if (!name %in% names(all_functions)) {
        all_functions[[name]] <- list()
      }
      all_functions[[name]] <- c(all_functions[[name]], fn_map[[name]])
      if (!name %in% names(function_source_file)) {
        function_source_file[[name]] <- basename(path)
      }
    }
  }

  known_names <- names(all_functions)
  dep_map <- stats::setNames(vector("list", length(known_names)), known_names)

  for (fn_name in known_names) {
    fn_bodies <- all_functions[[fn_name]]
    if (length(fn_bodies) == 0) {
      dep_map[[fn_name]] <- character(0)
      next
    }

    deps <- character(0)
    for (fn_body in fn_bodies) {
      call_df <- extract_calls_from_expr(fn_body)
      if (nrow(call_df) == 0) {
        next
      }

      keep_unqualified <- call_df$call_name %in%
        known_names &
        (is.na(call_df$pkg) | call_df$pkg == "")
      keep_namespaced <- !is.na(call_df$pkg) &
        call_df$pkg == known_package_name &
        call_df$call_name %in% known_names
      deps <- unique(c(
        deps,
        call_df$call_name[keep_unqualified | keep_namespaced]
      ))
    }

    deps <- deps[deps != fn_name]
    dep_map[[fn_name]] <- deps
  }

  list(
    dep_map = dep_map,
    function_files = function_source_file,
    function_names = known_names
  )
}

compute_transitive_dependencies <- function(roots, dep_map, entry_label) {
  queue <- vector("list", 0)
  visited <- new.env(parent = emptyenv())
  rows <- vector("list", 0)

  for (root in unique(roots)) {
    queue[[length(queue) + 1]] <- list(
      fn = root,
      depth = 1L,
      path = c(entry_label, root)
    )
  }

  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]

    key <- paste(current$fn, paste(current$path, collapse = " -> "), sep = "||")
    if (exists(key, envir = visited, inherits = FALSE)) {
      next
    }
    assign(key, TRUE, envir = visited)

    dep_type <- if (current$depth == 1L) "direct" else "indirect"
    rows[[length(rows) + 1]] <- data.frame(
      "function" = current$fn,
      dep_type = dep_type,
      hop_depth = current$depth,
      call_path = paste(current$path, collapse = " -> "),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    next_deps <- dep_map[[current$fn]]
    if (is.null(next_deps) || length(next_deps) == 0) {
      next
    }

    for (dep in next_deps) {
      if (dep %in% current$path) {
        next
      }
      queue[[length(queue) + 1]] <- list(
        fn = dep,
        depth = current$depth + 1L,
        path = c(current$path, dep)
      )
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      "function" = character(),
      dep_type = character(),
      hop_depth = integer(),
      call_path = character(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  out <- do.call(rbind, rows)
  out <- out[order(out$hop_depth, out[["function"]], out$call_path), ]
  rownames(out) <- NULL
  out
}

create_dependency_graph <- function(
  dep_rows,
  dep_map,
  root_functions,
  entry_label,
  output_path
) {
  edge_df <- data.frame(
    from = character(),
    to = character(),
    stringsAsFactors = FALSE
  )

  for (fn in names(dep_map)) {
    deps <- dep_map[[fn]]
    if (length(deps) == 0) {
      next
    }
    edge_df <- rbind(
      edge_df,
      data.frame(from = fn, to = deps, stringsAsFactors = FALSE)
    )
  }

  reachable_nodes <- unique(c(entry_label, dep_rows[["function"]]))
  root_edges <- data.frame(
    from = rep(entry_label, length(root_functions)),
    to = root_functions,
    stringsAsFactors = FALSE
  )
  edge_df <- rbind(edge_df, root_edges)
  edge_df <- edge_df[
    edge_df$from %in% reachable_nodes | edge_df$from == entry_label,
    ,
    drop = FALSE
  ]
  edge_df <- edge_df[edge_df$to %in% reachable_nodes, , drop = FALSE]
  edge_df <- unique(edge_df)

  if (nrow(edge_df) == 0) {
    grDevices::svg(output_path, width = 10, height = 7)
    graphics::plot.new()
    graphics::text(0.5, 0.5, labels = "No package-scoped dependencies detected")
    grDevices::dev.off()
    return(invisible(NULL))
  }

  g <- igraph::graph_from_data_frame(edge_df, directed = TRUE)

  depth_lookup <- tapply(dep_rows$hop_depth, dep_rows[["function"]], min)
  depth_vals <- rep(Inf, length(igraph::V(g)))
  names(depth_vals) <- igraph::V(g)$name
  depth_vals[names(depth_lookup)] <- as.numeric(depth_lookup)
  depth_vals[entry_label] <- 0

  finite_depth <- depth_vals[is.finite(depth_vals)]
  palette <- grDevices::colorRampPalette(c(
    "#0b3954",
    "#087e8b",
    "#bfd7ea",
    "#ff5a5f"
  ))
  max_depth <- if (length(finite_depth) == 0) 1 else max(finite_depth)
  color_steps <- palette(max_depth + 1)

  vertex_colors <- rep("#cccccc", length(depth_vals))
  for (nm in names(depth_vals)) {
    d <- depth_vals[[nm]]
    if (is.finite(d)) {
      vertex_colors[which(names(depth_vals) == nm)] <- color_steps[d + 1]
    }
  }

  grDevices::svg(output_path, width = 14, height = 10)
  plot(
    g,
    layout = igraph::layout_with_fr(g),
    vertex.size = 16,
    vertex.label.cex = 0.7,
    vertex.label.family = "sans",
    vertex.color = vertex_colors,
    edge.arrow.size = 0.35,
    main = paste0("Transitive Dependencies from ", entry_label)
  )
  grDevices::dev.off()

  invisible(NULL)
}
