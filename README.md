
<!-- README.md is generated from README.Rmd. Please edit that file -->

# functracer

Trace direct and transitive function dependencies for an R entry script
against a target package source tree.

<!-- badges: start -->

[![R-CMD-check](https://github.com/CCBR/functracer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CCBR/functracer/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/CCBR/functracer/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/CCBR/functracer/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

## Features

- Extract direct function calls from an entry script.
- Build package function adjacency from `R/` source files.
- Traverse transitive dependencies and retain call paths.
- Export dependency artifacts as CSV, JSON, and SVG.
- Supports regular function assignments and S7 generics/methods.

## Install (local)

``` r
# from the repository root
install.packages(".", repos = NULL, type = "source")
```

## Programmatic usage

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
  output_format = "json",
  output_dir = ".",
  output_prefix = "analysis"
)

output$output_path
```

## CLI usage

``` sh
Rscript inst/scripts/functracer.R \
  --entry path/to/main.R \
  --package-dir path/to/package-root \
  --output-format json \
  --output-dir . \
  --prefix analysis
```

## Output schema

CSV includes:

- `function`
- `dep_type` (`direct` or `indirect`)
- `hop_depth`
- `call_path`
- `source`
- `is_exported`
