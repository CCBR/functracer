# functracer: Introductory vignette

This vignette shows the smallest useful workflow:

1.  Pick an entry script.
2.  Point to a package source tree.
3.  Run
    [`analyze_dependencies()`](https://ccbr.github.io/functracer/dev/reference/analyze_dependencies.md).
4.  Optionally write one output artifact.

## Quick real-world call

``` r
library(functracer)

result <- analyze_dependencies(
  entry_script = "path/to/main.R",
  package_dir = "path/to/package-root"
)

result

output <- trace_functions(
  entry_script = "path/to/main.R",
  package_dir = "path/to/package-root",
  output_format = "svg",
  output_dir = ".",
  output_prefix = "analysis"
)

output$output_path
```

## Tiny self-contained example

This example creates a minimal package and script in a temp folder.

``` r
library(functracer)

work_dir <- tempfile("functracer-demo-")
dir.create(work_dir)

demo_pkg <- file.path(work_dir, "demoPkg")
dir.create(demo_pkg)
dir.create(file.path(demo_pkg, "R"))

writeLines(
  c(
    "Package: demoPkg",
    "Version: 0.0.1",
    "Title: Demo Package",
    "Description: Minimal package for functracer vignette.",
    "License: MIT"
  ),
  file.path(demo_pkg, "DESCRIPTION")
)

writeLines(
  c("export(core_fn)", "export(helper_fn)"),
  file.path(demo_pkg, "NAMESPACE")
)

writeLines(
  c(
    "core_fn <- function(x) {",
    "  helper_fn(x)",
    "}",
    "",
    "helper_fn <- function(x) {",
    "  x + 1",
    "}"
  ),
  file.path(demo_pkg, "R", "functions.R")
)

entry_script <- file.path(work_dir, "main.R")
writeLines(
  c(
    "run <- function(v) {",
    "  core_fn(v)",
    "}",
    "",
    "run(3)"
  ),
  entry_script
)

result <- analyze_dependencies(
  entry_script = entry_script,
  package_dir = demo_pkg
)

result
#>    function dep_type hop_depth                             call_path  source
#> 1   core_fn   direct         1                     main.R -> core_fn demoPkg
#> 2       run   direct         1                         main.R -> run  main.R
#> 3   core_fn indirect         2              main.R -> run -> core_fn demoPkg
#> 4 helper_fn indirect         2        main.R -> core_fn -> helper_fn demoPkg
#> 5 helper_fn indirect         3 main.R -> run -> core_fn -> helper_fn demoPkg
#>   is_exported
#> 1        TRUE
#> 2       FALSE
#> 3        TRUE
#> 4        TRUE
#> 5        TRUE
```

## Inspect output data

``` r
output <- trace_functions(
  entry_script = entry_script,
  package_dir = demo_pkg,
  output_format = "csv",
  output_dir = work_dir,
  output_prefix = "demo"
)
#> Dependency analysis complete
#> Format: csv
#> Output: /tmp/RtmpJMN9Au/functracer-demo-1c0bdbf7360/demo_dependencies.csv

out <- read.csv(output$output_path, check.names = FALSE)
out
#>    function dep_type hop_depth                             call_path  source
#> 1   core_fn   direct         1                     main.R -> core_fn demoPkg
#> 2       run   direct         1                         main.R -> run  main.R
#> 3   core_fn indirect         2              main.R -> run -> core_fn demoPkg
#> 4 helper_fn indirect         2        main.R -> core_fn -> helper_fn demoPkg
#> 5 helper_fn indirect         3 main.R -> run -> core_fn -> helper_fn demoPkg
#>   is_exported
#> 1        TRUE
#> 2       FALSE
#> 3        TRUE
#> 4        TRUE
#> 5        TRUE
```

## Release impact analysis

Use
[`trace_release_impact()`](https://ccbr.github.io/functracer/dev/reference/trace_release_impact.md)
when you want to check whether functions in the traced dependency graph
changed between tags.

``` r
release_result <- trace_release_impact(
  entry_script = "path/to/main.R",
  repository = "https://github.com/owner/package.git",
  release_tag = "v1.2.0",
  package_subdir = "packages/myPkg"
)

release_result$script_affected
release_result$changed_dependencies
```

`script_affected` is `TRUE` when at least one traced dependency comes
from an R source file that changed between the inferred `previous_tag`
(or an explicit `previous_tag`) and `release_tag`.

## Interpretation

- `dep_type = direct` means called from the entry script.
- `dep_type = indirect` means reached through other functions.
- Larger `hop_depth` means farther from the entry script.
- `call_path` gives the exact chain used to reach each function.

That is enough to answer: “If I change function X, what workflows are
affected?”
