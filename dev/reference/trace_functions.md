# Trace Functions and Write One Output Format

Trace Functions and Write One Output Format

## Usage

``` r
trace_functions(
  entry_script,
  package_dir,
  package_name = NULL,
  output_dir = ".",
  output_prefix = NULL,
  output_format = c("csv", "json", "svg")
)
```

## Arguments

- entry_script:

  Path to the entry R script.

- package_dir:

  Path to a package root directory containing `R/` and `NAMESPACE`.

- package_name:

  Optional package name. If `NULL`, inferred from `DESCRIPTION`.

- output_dir:

  Directory where output artifacts are written.

- output_prefix:

  Prefix for output files. Defaults to entry script stem.

- output_format:

  Output file format. One of `"csv"`, `"json"`, or `"svg"`.

## Value

Invisibly returns a list with: `output_path` (artifact path),
`output_format` (selected format), and `dependencies` (the same data
frame returned by
[`analyze_dependencies()`](https://ccbr.github.io/functracer/dev/reference/analyze_dependencies.md)).

## Examples

``` r
temp_root <- tempfile("functracer-example-")
dir.create(temp_root)
pkg_dir <- file.path(temp_root, "demoPkg")
dir.create(pkg_dir)
dir.create(file.path(pkg_dir, "R"))
writeLines(
  c(
    "Package: demoPkg",
    "Version: 0.0.1",
    "Title: Demo Package",
    "Description: Demo package for examples.",
    "License: MIT"
  ),
  file.path(pkg_dir, "DESCRIPTION")
)
writeLines("export(core_fn)", file.path(pkg_dir, "NAMESPACE"))
writeLines(
  c(
    "core_fn <- function(x) {",
    "  x + 1",
    "}"
  ),
  file.path(pkg_dir, "R", "core.R")
)
entry_script <- file.path(temp_root, "main.R")
writeLines("core_fn(3)", entry_script)
out <- trace_functions(
  entry_script = entry_script,
  package_dir = pkg_dir,
  output_dir = temp_root,
  output_format = "json"
)
#> Dependency analysis complete
#> Format: json
#> Output: /tmp/RtmpXipNcC/functracer-example-1b0610f73619/main_dependencies.json
file.exists(out$output_path)
#> [1] TRUE
```
