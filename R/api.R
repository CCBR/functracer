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
#'
#' @return A data frame describing direct and transitive dependencies.
#' @export
analyze_dependencies <- function(
  entry_script,
  package_dir,
  package_name = NULL
) {
  entry_script <- normalizePath(entry_script, winslash = "/", mustWork = FALSE)
  package_dir <- normalizePath(package_dir, winslash = "/", mustWork = FALSE)

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

  transitive_df
}

#' Write dependency data as CSV
#'
#' @param dependencies A dependency data frame from [analyze_dependencies()].
#' @param output_path Path to the output CSV file.
#'
#' @return The output path, invisibly.
#' @keywords internal
#' @noRd
write_dependencies_csv <- function(dependencies, output_path) {
  readr::write_csv(dependencies, output_path)
  invisible(output_path)
}

#' Write dependency data as JSON
#'
#' @param dependencies A dependency data frame from [analyze_dependencies()].
#' @param output_path Path to the output JSON file.
#'
#' @return The output path, invisibly.
#' @keywords internal
#' @noRd
write_dependencies_json <- function(dependencies, output_path) {
  jsonlite::write_json(
    dependencies,
    output_path,
    auto_unbox = TRUE,
    pretty = TRUE
  )

  invisible(output_path)
}

#' Write dependency data as SVG graph
#'
#' @param dependencies A dependency data frame from [analyze_dependencies()].
#' @param output_path Path to the output SVG file.
#'
#' @return The output path, invisibly.
#' @keywords internal
#' @noRd
write_dependencies_svg <- function(dependencies, output_path) {
  create_dependency_graph(dep_rows = dependencies, output_path = output_path)
  invisible(output_path)
}

#' Trace Functions and Write One Output Format
#'
#' @inheritParams analyze_dependencies
#' @param output_dir Directory where output artifacts are written.
#' @param output_prefix Prefix for output files. Defaults to entry script stem.
#' @param output_format Output file format. One of `"csv"`, `"json"`, or
#'   `"svg"`.
#'
#' @return Invisibly returns a list with output path and dependency data.
#' @export
trace_functions <- function(
  entry_script,
  package_dir,
  package_name = NULL,
  output_dir = ".",
  output_prefix = NULL,
  output_format = c("csv", "json", "svg")
) {
  output_format <- match.arg(output_format)

  dependencies <- analyze_dependencies(
    entry_script = entry_script,
    package_dir = package_dir,
    package_name = package_name
  )

  if (is.null(output_prefix) || identical(output_prefix, "")) {
    output_prefix <- tools::file_path_sans_ext(basename(entry_script))
  }

  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  extension <- switch(
    output_format,
    csv = "csv",
    json = "json",
    svg = "svg"
  )
  output_path <- file.path(
    output_dir,
    paste0(output_prefix, "_dependencies.", extension)
  )

  if (output_format == "csv") {
    write_dependencies_csv(dependencies, output_path)
  } else if (output_format == "json") {
    write_dependencies_json(dependencies, output_path)
  } else {
    write_dependencies_svg(dependencies, output_path)
  }

  message("Dependency analysis complete")
  message("Format: ", output_format)
  message("Output: ", output_path)

  invisible(list(
    output_path = output_path,
    output_format = output_format,
    dependencies = dependencies
  ))
}
