#' Extract function calls from an expression tree
#'
#' @param expr A parsed treesitter expression.
#'
#' @return A data frame of detected call names and package qualifiers.
#' @keywords internal
#' @noRd
extract_calls_from_expr <- function(expr) {
  empty_calls <- data.frame(
    call_name = character(),
    pkg = character(),
    stringsAsFactors = FALSE
  )

  if (is.null(expr)) {
    call_df <- empty_calls
  } else {
    calls <- list()

    walk_named_nodes(expr, function(node) {
      if (treesitter::node_type(node) == "call") {
        info <- extract_call_info(node)
        if (!is.null(info)) {
          calls[[length(calls) + 1]] <<- info
        }
      }
      invisible(NULL)
    })

    if (length(calls) == 0) {
      call_df <- empty_calls
    } else {
      call_df <- unique(as.data.frame(
        do.call(rbind, lapply(calls, as.data.frame)),
        stringsAsFactors = FALSE
      ))
    }
  }

  call_df
}

#' Extract function definitions from parsed expressions
#'
#' @param parsed_exprs Parsed treesitter expressions.
#'
#' @return A named list mapping function names to body nodes.
#' @keywords internal
#' @noRd
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
      invisible(NULL)
    } else {
      op <- get_assignment_operator(node)
      if (!op %in% c("<-", "=")) {
        invisible(NULL)
      } else {
        lhs <- treesitter::node_child_by_field_name(node, "lhs")
        rhs <- treesitter::node_child_by_field_name(node, "rhs")
        if (is.null(lhs) || is.null(rhs)) {
          invisible(NULL)
        } else {
          rhs_type <- treesitter::node_type(rhs)

          if (
            treesitter::node_type(lhs) == "identifier" &&
              rhs_type == "function_definition"
          ) {
            fn_name <- treesitter::node_text(lhs)
            fn_body <- treesitter::node_child_by_field_name(rhs, "body")
            append_body(fn_name, fn_body)
          }

          if (
            treesitter::node_type(lhs) == "identifier" &&
              rhs_type == "call"
          ) {
            rhs_fn <- treesitter::node_child_by_field_name(rhs, "function")
            ns <- extract_namespace_parts(rhs_fn)
            if (!is.null(ns) && ns$pkg == "S7" && ns$fun == "new_generic") {
              append_body(treesitter::node_text(lhs), NULL)
            }
          }

          if (
            treesitter::node_type(lhs) == "call" &&
              rhs_type == "function_definition"
          ) {
            lhs_fn <- treesitter::node_child_by_field_name(lhs, "function")
            is_method <- FALSE

            if (
              !is.null(lhs_fn) &&
                treesitter::node_type(lhs_fn) == "identifier"
            ) {
              is_method <- identical(treesitter::node_text(lhs_fn), "method")
            } else {
              ns <- extract_namespace_parts(lhs_fn)
              is_method <- !is.null(ns) && ns$fun == "method"
            }

            if (is_method) {
              method_name <- extract_arg_identifier(lhs)
              if (!is.na(method_name)) {
                fn_body <- treesitter::node_child_by_field_name(rhs, "body")
                append_body(method_name, fn_body)
              }
            }
          }

          invisible(NULL)
        }
      }
    }
  })

  fn_map
}

#' Extract direct function calls from an entry script
#'
#' @param script_path Path to the entry script.
#'
#' @return A list with direct call rows, parser metadata, and parsed
#'   expressions.
#' @keywords internal
#' @noRd
extract_script_calls <- function(script_path) {
  parsed_exprs <- parse_file_with_treesitter(script_path)

  calls <- list()
  walk_named_nodes(parsed_exprs, function(node) {
    if (treesitter::node_type(node) == "call") {
      info <- extract_call_info(node, include_line = TRUE)
      if (!is.null(info)) {
        calls[[length(calls) + 1]] <<- info
      }
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
          package_name <- NA_character_
          if (!is.null(x$pkg) && !is.na(x$pkg)) {
            package_name <- x$pkg
          }

          package_name
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

#' Build a package dependency map from R source files
#'
#' @param r_dir Path to a package `R/` directory.
#' @param known_package_name Package name used for namespaced self-calls.
#'
#' @return A list containing dependency mappings and discovered function names.
#' @keywords internal
#' @noRd
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
