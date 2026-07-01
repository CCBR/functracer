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

Invisibly returns a list with output path and dependency data.
