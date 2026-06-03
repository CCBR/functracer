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
