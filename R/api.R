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

  direct_calls_result <- extract_script_calls(entry_script)
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

  transitive_df <- trace_dependencies(
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
