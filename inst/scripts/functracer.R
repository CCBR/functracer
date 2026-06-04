#!/usr/bin/env Rscript

#' Load package R sources into an execution environment
#'
#' @param pkg_root Path to the package root.
#'
#' @return An environment containing the sourced package functions.
load_package_functions <- function(pkg_root) {
  r_files <- list.files(
    file.path(pkg_root, "R"),
    pattern = "\\.R$",
    full.names = TRUE
  )
  r_files <- sort(r_files)

  for (r_file in r_files) {
    sys.source(r_file, envir = globalenv())
  }
}

#' Create the functracer command-line argument parser
#'
#' @return An `argparse::ArgumentParser` instance.
build_parser <- function() {
  parser <- argparse::ArgumentParser(
    description = "Trace direct and transitive R function dependencies."
  )

  parser$add_argument(
    "--entry",
    required = TRUE,
    help = "Entry R script to analyze."
  )
  parser$add_argument(
    "--package-dir",
    required = TRUE,
    help = "Target package root with R/ and NAMESPACE."
  )
  parser$add_argument(
    "--package-name",
    default = NULL,
    help = "Optional package name override."
  )
  parser$add_argument(
    "--output-dir",
    default = ".",
    help = "Directory for CSV, JSON, and SVG output."
  )
  parser$add_argument(
    "--prefix",
    default = NULL,
    help = "Optional output filename prefix."
  )

  parser
}

if (!requireNamespace("argparse", quietly = TRUE)) {
  stop("Package 'argparse' is required to run the functracer CLI")
}

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_arg) == 0) {
  stop("Unable to determine script path from commandArgs()")
}
script_path <- normalizePath(
  sub("^--file=", "", script_arg[1]),
  mustWork = TRUE
)
pkg_root <- normalizePath(
  file.path(dirname(script_path), "..", ".."),
  mustWork = TRUE
)
load_package_functions(pkg_root)

args <- build_parser()$parse_args()

trace_functions(
  entry_script = args$entry,
  package_dir = args[["package-dir"]],
  package_name = args[["package-name"]],
  output_dir = if (is.null(args[["output-dir"]])) "." else args[["output-dir"]],
  output_prefix = args$prefix
)
