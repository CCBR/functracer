# Trace Functions and Print Output Paths

Trace Functions and Print Output Paths

## Usage

``` r
trace_functions(
  entry_script,
  package_dir,
  package_name = NULL,
  output_dir = ".",
  output_prefix = NULL
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

## Value

Invisibly returns the same list as
[`analyze_dependencies()`](https://ccbr.github.io/functracer/reference/analyze_dependencies.md).
