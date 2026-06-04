#' Load the treesitter language definition for R
#'
#' @return A treesitter language object.
#' @keywords internal
#' @noRd
get_treesitter_language <- function() {
  if (!requireNamespace("treesitter", quietly = TRUE)) {
    stop("Package 'treesitter' is required but not installed")
  }
  if (!requireNamespace("treesitter.r", quietly = TRUE)) {
    stop("Package 'treesitter.r' is required but not installed")
  }

  treesitter.r::language()
}

#' Parse an R file with treesitter
#'
#' @param path Path to an R source file.
#'
#' @return A parsed treesitter document.
#' @keywords internal
#' @noRd
parse_file_with_treesitter <- function(path) {
  source_text <- readr::read_file(path)
  treesitter::text_parse(source_text, get_treesitter_language())
}

#' Walk named treesitter nodes recursively
#'
#' @param node A treesitter node.
#' @param visitor A callback applied to each named node.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
walk_named_nodes <- function(node, visitor) {
  visitor(node)
  children <- treesitter::node_named_children(node)
  if (length(children) == 0) {
    invisible(NULL)
  } else {
    for (child in children) {
      walk_named_nodes(child, visitor)
    }

    invisible(NULL)
  }
}

#' Read an assignment operator from a node
#'
#' @param node A treesitter node.
#'
#' @return The assignment operator text or `NA_character_`.
#' @keywords internal
#' @noRd
get_assignment_operator <- function(node) {
  op_node <- treesitter::node_child_by_field_name(node, "operator")
  assignment_operator <- if (is.null(op_node)) {
    NA_character_
  } else {
    treesitter::node_text(op_node)
  }

  assignment_operator
}

#' Read identifier text from a node
#'
#' @param node A treesitter node.
#'
#' @return The identifier text or `NA_character_`.
#' @keywords internal
#' @noRd
get_identifier_text <- function(node) {
  identifier_text <- NA_character_

  if (is.null(node)) {
    identifier_text <- NA_character_
  } else if (treesitter::node_type(node) == "identifier") {
    identifier_text <- treesitter::node_text(node)
  }

  identifier_text
}

#' Extract namespace-qualified call parts
#'
#' @param node A treesitter namespace operator node.
#'
#' @return A list with package, function, and operator fields, or `NULL`.
#' @keywords internal
#' @noRd
extract_namespace_parts <- function(node) {
  parts <- NULL

  if (is.null(node) || treesitter::node_type(node) != "namespace_operator") {
    parts <- NULL
  } else {
    lhs <- treesitter::node_child_by_field_name(node, "lhs")
    rhs <- treesitter::node_child_by_field_name(node, "rhs")
    op <- treesitter::node_child_by_field_name(node, "operator")
    parts <- list(
      pkg = get_identifier_text(lhs),
      fun = get_identifier_text(rhs),
      op = if (is.null(op)) NA_character_ else treesitter::node_text(op)
    )
  }

  parts
}

#' Extract the first identifier from call arguments
#'
#' @param call_node A treesitter call node.
#'
#' @return The first identifier argument name or `NA_character_`.
#' @keywords internal
#' @noRd
extract_arg_identifier <- function(call_node) {
  first_identifier <- NA_character_
  args_node <- treesitter::node_child_by_field_name(call_node, "arguments")

  if (!is.null(args_node)) {
    arg_nodes <- treesitter::node_named_children(args_node)

    if (length(arg_nodes) > 0) {
      arg_nodes <- Filter(
        function(node_value) treesitter::node_type(node_value) == "argument",
        arg_nodes
      )

      if (length(arg_nodes) > 0) {
        first_value <- treesitter::node_child_by_field_name(
          arg_nodes[[1]],
          "value"
        )
        first_identifier <- get_identifier_text(first_value)
      }
    }
  }

  first_identifier
}

#' Extract call metadata from a call node
#'
#' @param call_node A treesitter call node.
#' @param include_line Whether to include the call line number.
#'
#' @return A list describing the call, or `NULL` if it cannot be resolved.
#' @keywords internal
#' @noRd
extract_call_info <- function(call_node, include_line = FALSE) {
  fn_node <- treesitter::node_child_by_field_name(call_node, "function")
  out <- NULL

  if (is.null(fn_node)) {
    out <- NULL
  } else {
    fn_type <- treesitter::node_type(fn_node)
    if (fn_type == "identifier") {
      out <- list(
        call_name = treesitter::node_text(fn_node),
        pkg = NA_character_
      )
    } else if (fn_type == "namespace_operator") {
      ns <- extract_namespace_parts(fn_node)
      if (!is.null(ns) && !is.na(ns$fun)) {
        out <- list(
          call_name = ns$fun,
          pkg = ns$pkg
        )
      }
    }
  }

  if (include_line && !is.null(out)) {
    out$line <- as.integer(
      treesitter::point_row(treesitter::node_start_point(call_node)) + 1
    )
  }

  out
}
