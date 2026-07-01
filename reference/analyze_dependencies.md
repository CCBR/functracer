# Analyze Direct and Transitive R Function Dependencies

Build a dependency map starting from an entry script and tracing into a
target package source directory.

## Usage

``` r
analyze_dependencies(entry_script, package_dir, package_name = NULL)
```

## Arguments

- entry_script:

  Path to the entry R script.

- package_dir:

  Path to a package root directory containing `R/` and `NAMESPACE`.

- package_name:

  Optional package name. If `NULL`, inferred from `DESCRIPTION`.

## Value

A data frame with one row per traced function and columns: `function`,
`dep_type`, `hop_depth`, `call_path`, `source`, and `is_exported`.

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
analyze_dependencies(entry_script = entry_script, package_dir = pkg_dir)
#>   function dep_type hop_depth         call_path  source is_exported
#> 1  core_fn   direct         1 main.R -> core_fn demoPkg        TRUE
```
