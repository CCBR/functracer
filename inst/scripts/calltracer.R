#!/usr/bin/env Rscript

usage <- function() {
  cat(
    "calltracer CLI\n",
    "Usage:\n",
    "  Rscript inst/scripts/calltracer.R --entry=path/to/main.R --package-dir=path/to/pkg [options]\n\n",
    "Options:\n",
    "  --entry=PATH          Entry R script to analyze (required)\n",
    "  --package-dir=PATH    Target package root with R/ and NAMESPACE (required)\n",
    "  --package-name=NAME   Package name override (optional)\n",
    "  --output-dir=PATH     Output directory (default: .)\n",
    "  --prefix=NAME         Output prefix (default: entry script stem)\n",
    "  --help                Show this message\n",
    sep = ""
  )
}

parse_args <- function(args) {
  out <- list()
  for (arg in args) {
    if (identical(arg, "--help")) {
      out$help <- TRUE
      next
    }
    if (!startsWith(arg, "--") || !grepl("=", arg, fixed = TRUE)) {
      stop("Invalid argument format: ", arg)
    }
    kv <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1]]
    key <- kv[1]
    value <- paste(kv[-1], collapse = "=")
    out[[key]] <- value
  }
  out
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
if (!is.null(args$help)) {
  usage()
  quit(status = 0)
}

if (is.null(args$entry) || is.null(args[["package-dir"]])) {
  usage()
  stop("--entry and --package-dir are required")
}

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_arg) == 0) {
  stop("Unable to determine script path from commandArgs()")
}
script_path <- normalizePath(sub("^--file=", "", script_arg[1]), mustWork = TRUE)
pkg_root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
source(file.path(pkg_root, "R", "calltracer.R"), local = TRUE)

run_calltracer(
  entry_script = args$entry,
  package_dir = args[["package-dir"]],
  package_name = args[["package-name"]],
  output_dir = if (is.null(args[["output-dir"]])) "." else args[["output-dir"]],
  output_prefix = args$prefix
)
