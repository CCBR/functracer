#' Trace transitive dependencies from root functions
#'
#' @param roots Root function names.
#' @param dep_map Named list of direct dependencies.
#' @param entry_label Label used for the entry script node.
#'
#' @return A data frame describing dependency depth and call paths.
#' @keywords internal
#' @noRd
trace_dependencies <- function(roots, dep_map, entry_label) {
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
    out <- data.frame(
      "function" = character(),
      dep_type = character(),
      hop_depth = integer(),
      call_path = character(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    out <- do.call(rbind, rows)
    out <- out[order(out$hop_depth, out[["function"]], out$call_path), ]
    rownames(out) <- NULL
  }

  out
}

#' Render a dependency graph as SVG
#'
#' @param dep_rows Dependency rows returned by the traversal.
#' @param output_path Path to the output SVG file.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
build_graph_edges <- function(dep_rows) {
  edge_rows <- vector("list", 0)

  if (nrow(dep_rows) == 0) {
    edge_df <- data.frame(
      from = character(),
      to = character(),
      stringsAsFactors = FALSE
    )
    return(edge_df)
  }

  for (call_path in dep_rows$call_path) {
    path_nodes <- strsplit(call_path, " -> ", fixed = TRUE)[[1]]
    if (length(path_nodes) < 2) {
      next
    }

    for (idx in seq_len(length(path_nodes) - 1)) {
      edge_rows[[length(edge_rows) + 1]] <- data.frame(
        from = path_nodes[[idx]],
        to = path_nodes[[idx + 1]],
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(edge_rows) == 0) {
    edge_df <- data.frame(
      from = character(),
      to = character(),
      stringsAsFactors = FALSE
    )
  } else {
    edge_df <- unique(do.call(rbind, edge_rows))
  }

  edge_df
}

#' Render a dependency graph as SVG
#'
#' @param dep_rows Dependency rows returned by the traversal.
#' @param output_path Path to the output SVG file.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
create_dependency_graph <- function(dep_rows, output_path) {
  edge_df <- build_graph_edges(dep_rows)
  entry_label <- if (nrow(dep_rows) == 0) {
    "entry"
  } else {
    strsplit(dep_rows$call_path[[1]], " -> ", fixed = TRUE)[[1]][[1]]
  }

  if (nrow(edge_df) == 0) {
    grDevices::svg(output_path, width = 10, height = 7)
    graphics::plot.new()
    graphics::text(0.5, 0.5, labels = "No package-scoped dependencies detected")
    grDevices::dev.off()
    invisible(NULL)
  } else {
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
}
