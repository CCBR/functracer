# functracer: Introductory vignette

This vignette shows the smallest useful workflow:

1.  Pick an entry script.
2.  Point to a package source tree.
3.  Run
    [`analyze_dependencies()`](https://ccbr.github.io/functracer/reference/analyze_dependencies.md).
4.  Read the output files.

## Quick real-world call

``` r
library(functracer)

result <- analyze_dependencies(
  entry_script = "path/to/main.R",
  package_dir = "path/to/package-root",
  output_dir = ".",
  output_prefix = "analysis"
)

result$csv
result$json
result$svg
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
  package_dir = demo_pkg,
  output_dir = work_dir,
  output_prefix = "demo"
)

result[c("csv", "json", "svg")]
#> $csv
#> [1] "/tmp/RtmpKxnLwc/functracer-demo-1ddd122d4395/demo_dependencies.csv"
#> 
#> $json
#> [1] "/tmp/RtmpKxnLwc/functracer-demo-1ddd122d4395/demo_dependencies.json"
#> 
#> $svg
#> [1] "/tmp/RtmpKxnLwc/functracer-demo-1ddd122d4395/demo_dependencies.svg"
```

## Inspect output data

``` r
out <- read.csv(result$csv, check.names = FALSE)
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

## Interpretation

- `dep_type = direct` means called from the entry script.
- `dep_type = indirect` means reached through other functions.
- Larger `hop_depth` means farther from the entry script.
- `call_path` gives the exact chain used to reach each function.

That is enough to answer: “If I change function X, what workflows are
affected?”
