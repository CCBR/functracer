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

  return(invisible(NULL))
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
    required = FALSE,
    help = "Target package root with R/ and NAMESPACE for local tracing."
  )
  parser$add_argument(
    "--repo-url",
    required = FALSE,
    help = "GitHub repository URL or local git repository for release analysis."
  )
  parser$add_argument(
    "--release-tag",
    required = FALSE,
    help = "Release tag to compare against the previous version."
  )
  parser$add_argument(
    "--previous-tag",
    required = FALSE,
    help = "Optional previous tag override for release analysis."
  )
  parser$add_argument(
    "--package-name",
    default = NULL,
    help = "Optional package name override."
  )
  parser$add_argument(
    "--output-dir",
    default = ".",
    help = "Directory for the output artifact."
  )
  parser$add_argument(
    "--prefix",
    default = NULL,
    help = "Optional output filename prefix."
  )
  parser$add_argument(
    "--output-format",
    default = "csv",
    choices = c("csv", "json", "svg"),
    help = "Output format to write."
  )

  return(parser)
}

#' Resolve a parsed CLI argument by underscore or dash key
#'
#' @param args Parsed argument list from `argparse`.
#' @param name Argument name as defined in CLI help (without leading `--`).
#'
#' @return The argument value, or `NULL` when not provided.
get_cli_argument <- function(args, name) {
  underscore_name <- gsub("-", "_", name)
  value <- args[[underscore_name]]

  if (is.null(value)) {
    value <- args[[name]]
  }

  return(value)
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

repo_url <- get_cli_argument(args, "repo-url")
release_tag <- get_cli_argument(args, "release-tag")
previous_tag <- get_cli_argument(args, "previous-tag")
package_name <- get_cli_argument(args, "package-name")
output_format <- get_cli_argument(args, "output-format")
output_dir <- get_cli_argument(args, "output-dir")
package_dir <- get_cli_argument(args, "package-dir")

if (!is.null(repo_url) || !is.null(release_tag)) {
  if (is.null(repo_url) || is.null(release_tag)) {
    stop("Both --repo-url and --release-tag are required for release analysis")
  }

  trace_release_impact(
    entry_script = args$entry,
    repository = repo_url,
    release_tag = release_tag,
    package_subdir = package_dir,
    previous_tag = previous_tag,
    package_name = package_name,
    output_format = output_format,
    output_dir = if (is.null(output_dir)) {
      "."
    } else {
      output_dir
    },
    output_prefix = args$prefix
  )
} else {
  if (is.null(package_dir)) {
    stop("--package-dir is required for local dependency tracing")
  }

  trace_functions(
    entry_script = args$entry,
    package_dir = package_dir,
    package_name = package_name,
    output_format = output_format,
    output_dir = if (is.null(output_dir)) {
      "."
    } else {
      output_dir
    },
    output_prefix = args$prefix
  )
}
