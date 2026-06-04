#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(igraph)
  library(jsonlite)
  library(readr)
})

project_root <- normalizePath(
  file.path(getwd(), "."),
  winslash = "/",
  mustWork = TRUE
)
code_dir <- file.path(project_root, "code")
main_script <- file.path(code_dir, "main.R")
mosuite_pkg_dir <- file.path(code_dir, "MOSuite")
mosuite_r_dir <- file.path(mosuite_pkg_dir, "R")
namespace_file <- file.path(mosuite_pkg_dir, "NAMESPACE")

output_csv <- file.path(project_root, "venn_diagram_dependencies.csv")
output_json <- file.path(project_root, "venn_diagram_dependencies.json")
output_svg <- file.path(project_root, "venn_diagram_dependencies.svg")

if (!file.exists(main_script)) {
  stop("Cannot find main script: ", main_script)
}
if (!dir.exists(mosuite_r_dir)) {
  stop("Cannot find MOSuite R directory: ", mosuite_r_dir)
}
if (!file.exists(namespace_file)) {
  stop("Cannot find MOSuite NAMESPACE: ", namespace_file)
}

#' Extract exported functions from a NAMESPACE file
#'
#' @param namespace_path Path to a package `NAMESPACE` file.
#'
#' @return A character vector of exported function names.
extract_exported_functions <- function(namespace_path) {
  namespace_lines <- readLines(namespace_path, warn = FALSE)
  exported <- grep("^export\\(", namespace_lines, value = TRUE)
  exported <- sub("^export\\(\"?", "", exported)
  exported <- sub("\"?\\)$", "", exported)
  unique(exported)
}

#' Get package and function names from a namespace call
#'
#' @param call_expr A parsed R call expression.
#'
#' @return A list with `pkg` and `fun` fields, or `NULL`.
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

#' Extract function calls from an expression
#'
#' @param expr A parsed R expression.
#'
#' @return A data frame of call names and optional package qualifiers.
extract_calls_from_expr <- function(expr) {
  calls <- list()

  walk_expr <- function(node) {
    if (is.call(node)) {
      head <- node[[1]]
      call_info <- NULL

      if (is.symbol(head)) {
        fn <- as.character(head)
        call_info <- list(call_name = fn, pkg = NA_character_)
      } else if (is.call(head)) {
        ns_call <- get_namespace_call(head)
        if (!is.null(ns_call)) {
          call_info <- list(call_name = ns_call$fun, pkg = ns_call$pkg)
        }
      }

      if (!is.null(call_info)) {
        calls[[length(calls) + 1]] <<- call_info
      }

      for (idx in seq_along(node)) {
        walk_expr(node[[idx]])
      }
    } else if (is.pairlist(node) || is.expression(node)) {
      for (idx in seq_along(node)) {
        walk_expr(node[[idx]])
      }
    }
  }

  walk_expr(expr)

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

#' Extract top-level function definitions from parsed expressions
#'
#' @param parsed_exprs Parsed R expressions.
#'
#' @return A named list mapping function names to body expressions.
extract_top_level_functions <- function(parsed_exprs) {
  fn_map <- list()

  append_body <- function(fn_name, body_expr) {
    if (!fn_name %in% names(fn_map)) {
      fn_map[[fn_name]] <<- list()
    }
    fn_map[[fn_name]][[length(fn_map[[fn_name]]) + 1]] <<- body_expr
  }

  get_ns_fun_name <- function(call_expr) {
    if (!is.call(call_expr) || length(call_expr) < 3) {
      return(NA_character_)
    }
    op <- if (is.symbol(call_expr[[1]])) {
      as.character(call_expr[[1]])
    } else {
      NA_character_
    }
    if (!(op %in% c("::", ":::"))) {
      return(NA_character_)
    }
    if (!is.symbol(call_expr[[3]])) {
      return(NA_character_)
    }
    as.character(call_expr[[3]])
  }

  walk_expr <- function(node) {
    if (is.call(node) && length(node) >= 3) {
      op <- if (is.symbol(node[[1]])) as.character(node[[1]]) else NA_character_
      if (op %in% c("<-", "=")) {
        lhs <- node[[2]]
        rhs <- node[[3]]

        rhs_head <- if (is.call(rhs) && is.symbol(rhs[[1]])) {
          as.character(rhs[[1]])
        } else {
          NA_character_
        }
        rhs_ns_head <- if (is.call(rhs)) {
          get_ns_fun_name(rhs[[1]])
        } else {
          NA_character_
        }

        if (is.symbol(lhs) && identical(rhs_head, "function")) {
          append_body(as.character(lhs), rhs[[3]])
        }

        if (
          is.symbol(lhs) && !is.na(rhs_ns_head) && rhs_ns_head == "new_generic"
        ) {
          append_body(as.character(lhs), NULL)
        }

        lhs_head <- if (is.call(lhs) && is.symbol(lhs[[1]])) {
          as.character(lhs[[1]])
        } else {
          NA_character_
        }
        lhs_ns_head <- if (is.call(lhs)) {
          get_ns_fun_name(lhs[[1]])
        } else {
          NA_character_
        }

        if (
          is.call(lhs) &&
            identical(rhs_head, "function") &&
            (lhs_head == "method" || lhs_ns_head == "method")
        ) {
          if (length(lhs) >= 2 && is.symbol(lhs[[2]])) {
            append_body(as.character(lhs[[2]]), rhs[[3]])
          }
        }
      }
    }

    if (is.call(node) || is.expression(node) || is.pairlist(node)) {
      for (i in seq_along(node)) {
        walk_expr(node[[i]])
      }
    }
  }

  walk_expr(parsed_exprs)
  fn_map
}

#' Extract direct calls from a script
#'
#' @param script_path Path to an R script.
#'
#' @return A list containing direct calls, parser metadata, and parsed
#'   expressions.
extract_script_calls <- function(script_path) {
  parser_used <- "base-parse"
  if (requireNamespace("treesitter", quietly = TRUE)) {
    ts_tree_parse <- get0(
      "tree_parse",
      asNamespace("treesitter"),
      inherits = FALSE
    )
    if (is.function(ts_tree_parse)) {
      parser_used <- "treesitter-available-fallback-to-base"
    }
  }

  parsed_exprs <- parse(file = script_path, keep.source = TRUE)
  pd <- getParseData(parsed_exprs)

  fn_tokens <- pd[
    pd$token == "SYMBOL_FUNCTION_CALL",
    c("line1", "text", "parent")
  ]
  if (nrow(fn_tokens) == 0) {
    direct_calls <- data.frame(
      line = integer(),
      function_name = character(),
      call_type = character(),
      package = character(),
      stringsAsFactors = FALSE
    )
  } else {
    direct_calls <- data.frame(
      line = fn_tokens$line1,
      function_name = fn_tokens$text,
      call_type = "unqualified",
      package = NA_character_,
      stringsAsFactors = FALSE
    )

    for (i in seq_len(nrow(fn_tokens))) {
      parent_id <- fn_tokens$parent[i]
      ns_row <- pd[
        pd$id == parent_id & pd$token %in% c("NS_GET", "NS_GET_INT"),
        ,
        drop = FALSE
      ]
      if (nrow(ns_row) > 0) {
        pkg_row <- pd[
          pd$parent == parent_id & pd$token == "SYMBOL_PACKAGE",
          ,
          drop = FALSE
        ]
        if (nrow(pkg_row) > 0) {
          direct_calls$call_type[i] <- "namespaced"
          direct_calls$package[i] <- pkg_row$text[1]
        }
      }
    }

    direct_calls <- direct_calls[
      order(direct_calls$line, direct_calls$function_name),
    ]
    rownames(direct_calls) <- NULL
  }

  list(
    calls = direct_calls,
    parser_used = parser_used,
    parsed_exprs = parsed_exprs
  )
}

#' Build a dependency map from a package R directory
#'
#' @param r_dir Path to a package `R/` directory.
#' @param known_package_name Package name used for namespaced self-calls.
#'
#' @return A list with dependency mappings, source files, and function names.
build_package_dependency_map <- function(
  r_dir,
  known_package_name = "MOSuite"
) {
  files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  files <- files[order(files)]

  all_functions <- list()
  function_source_file <- list()

  for (path in files) {
    parsed <- parse(file = path, keep.source = TRUE)
    fn_map <- extract_top_level_functions(parsed)
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
  dep_map <- setNames(vector("list", length(known_names)), known_names)

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

#' Trace transitive dependencies from root functions
#'
#' @param roots A character vector of root function names.
#' @param dep_map A named list mapping functions to dependencies.
#'
#' @return A data frame describing dependency depth and call paths.
trace_dependencies <- function(roots, dep_map) {
  queue <- vector("list", 0)
  visited <- new.env(parent = emptyenv())
  rows <- vector("list", 0)

  for (root in unique(roots)) {
    queue[[length(queue) + 1]] <- list(
      fn = root,
      depth = 1L,
      path = c("main.R", root)
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

#' Create an SVG dependency graph
#'
#' @param dep_rows A dependency data frame.
#' @param dep_map A named list mapping functions to dependencies.
#' @param root_functions Root functions connected to `main.R`.
#' @param output_path Path to the SVG output file.
#'
#' @return Invisibly creates the graph at `output_path`.
create_dependency_graph <- function(
  dep_rows,
  dep_map,
  root_functions,
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

  reachable_nodes <- unique(c("main.R", dep_rows[["function"]]))
  root_edges <- data.frame(
    from = rep("main.R", length(root_functions)),
    to = root_functions,
    stringsAsFactors = FALSE
  )
  edge_df <- rbind(edge_df, root_edges)
  edge_df <- edge_df[
    edge_df$from %in% reachable_nodes | edge_df$from == "main.R",
    ,
    drop = FALSE
  ]
  edge_df <- edge_df[edge_df$to %in% reachable_nodes, , drop = FALSE]
  edge_df <- unique(edge_df)

  g <- graph_from_data_frame(edge_df, directed = TRUE)

  depth_lookup <- tapply(dep_rows$hop_depth, dep_rows[["function"]], min)
  depth_vals <- rep(Inf, length(V(g)))
  names(depth_vals) <- V(g)$name
  depth_vals[names(depth_lookup)] <- as.numeric(depth_lookup)
  depth_vals["main.R"] <- 0

  finite_depth <- depth_vals[is.finite(depth_vals)]
  palette <- colorRampPalette(c("#0b3954", "#087e8b", "#bfd7ea", "#ff5a5f"))
  max_depth <- if (length(finite_depth) == 0) 1 else max(finite_depth)
  color_steps <- palette(max_depth + 1)

  vertex_colors <- rep("#cccccc", length(depth_vals))
  for (nm in names(depth_vals)) {
    d <- depth_vals[[nm]]
    if (is.finite(d)) {
      vertex_colors[which(names(depth_vals) == nm)] <- color_steps[d + 1]
    }
  }

  svg(output_path, width = 14, height = 10)
  plot(
    g,
    layout = layout_with_fr(g),
    vertex.size = 16,
    vertex.label.cex = 0.7,
    vertex.label.family = "sans",
    vertex.color = vertex_colors,
    edge.arrow.size = 0.35,
    main = "MOSuite Transitive Dependencies from main.R"
  )
  dev.off()
}

exported_functions <- extract_exported_functions(namespace_file)

direct_calls_result <- extract_script_calls(main_script)
direct_calls <- direct_calls_result$calls

main_parsed <- direct_calls_result$parsed_exprs
main_fn_map <- extract_top_level_functions(main_parsed)
main_local_functions <- names(main_fn_map)

pkg_map_result <- build_package_dependency_map(
  mosuite_r_dir,
  known_package_name = "MOSuite"
)
dep_map <- pkg_map_result$dep_map
known_mosuite_fns <- pkg_map_result$function_names

for (local_fn in main_local_functions) {
  local_deps <- character(0)
  local_bodies <- main_fn_map[[local_fn]]
  for (body_expr in local_bodies) {
    call_df <- extract_calls_from_expr(body_expr)
    local_deps <- unique(c(
      local_deps,
      call_df$call_name[
        call_df$call_name %in% c(known_mosuite_fns, main_local_functions)
      ]
    ))
  }
  local_deps <- local_deps[local_deps != local_fn]
  dep_map[[local_fn]] <- local_deps
}

direct_mosuite <- unique(direct_calls$function_name[
  direct_calls$package == "MOSuite" |
    (is.na(direct_calls$package) &
      direct_calls$function_name %in% known_mosuite_fns)
])

direct_main_local <- unique(direct_calls$function_name[
  is.na(direct_calls$package) &
    direct_calls$function_name %in% main_local_functions
])

roots <- unique(c(direct_mosuite, direct_main_local))
roots <- roots[roots %in% names(dep_map)]

transitive_df <- trace_dependencies(roots, dep_map)

if (nrow(transitive_df) > 0) {
  transitive_df$source <- ifelse(
    transitive_df[["function"]] %in% main_local_functions,
    "main.R",
    "MOSuite"
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

write_csv(transitive_df, output_csv)

json_payload <- list(
  metadata = list(
    generated_at = as.character(Sys.time()),
    parser = direct_calls_result$parser_used,
    entry_script = "code/main.R",
    package_dir = "code/MOSuite",
    roots = roots
  ),
  direct_calls = direct_calls,
  dependencies_by_depth = split(transitive_df, transitive_df$hop_depth)
)

write_json(json_payload, output_json, auto_unbox = TRUE, pretty = TRUE)

create_dependency_graph(
  dep_rows = transitive_df,
  dep_map = dep_map,
  root_functions = roots,
  output_path = output_svg
)

message("Dependency analysis complete")
message("CSV: ", output_csv)
message("JSON: ", output_json)
message("SVG: ", output_svg)
