# Analyze Direct and Transitive R Function Dependencies

Build a dependency map starting from an entry script and tracing into a
target package source directory.

## Usage

``` r
analyze_dependencies(
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

A list containing paths to output files and in-memory data frames.
