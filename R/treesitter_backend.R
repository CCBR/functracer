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
