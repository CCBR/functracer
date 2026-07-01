
<!-- README.md is generated from README.Rmd. Please edit that file -->

# functracer

Trace direct and transitive function dependencies for an R script
against a target package source tree.

<!-- badges: start -->

[![R-CMD-check](https://github.com/CCBR/functracer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CCBR/functracer/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/CCBR/functracer/graph/badge.svg?token=5BLOfOM2Z8)](https://codecov.io/gh/CCBR/functracer)
<!-- badges: end -->

<https://ccbr.github.io/functracer>

## Features

- Extract direct function calls from an R script.
- Build package function adjacency from `R/` source files.
- Traverse transitive dependencies and retain call paths.
- Compare a tagged GitHub release against the previous tag and flag
  traced dependencies that changed.
- Export dependency artifacts as CSV, JSON, and SVG.
- Supports regular function assignments and S7 generics/methods.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("CCBR/functracer")
```

## Usage

See the [introductory
vignette](https://ccbr.github.io/functracer/articles/intro.html) for a
detailed introduction.

### R

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

release_result <- trace_release_impact(
  entry_script = "path/to/main.R",
  repository = "https://github.com/owner/package.git",
  release_tag = "v1.2.0",
  package_subdir = "packages/myPkg"
)

release_result$script_affected
release_result$changed_dependencies
```

### CLI

These commands are for source checkouts of this repository (they call
`inst/scripts/functracer.R` directly).

``` sh
Rscript inst/scripts/functracer.R \
  --entry path/to/main.R \
  --package-dir path/to/package-root \
  --output-format json \
  --output-dir . \
  --prefix analysis

Rscript inst/scripts/functracer.R \
  --entry path/to/main.R \
  --repo-url https://github.com/owner/package.git \
  --release-tag v1.2.0 \
  --previous-tag v1.1.0 \
  --output-format json \
  --output-dir . \
  --prefix release-analysis
```

The CLI currently assumes the package lives at repository root for
release-analysis mode.

### Output schema

#### Local dependency tracing (`analyze_dependencies()`, `trace_functions()`)

CSV/JSON includes:

- `function`
- `dep_type` (`direct` or `indirect`)
- `hop_depth`
- `call_path`
- `source`
- `is_exported`

#### Release impact tracing (`trace_release_impact()`)

CSV/JSON includes all local fields above plus:

- `source_file`
- `source_file_changed`
- `release_tag`
- `previous_tag`
