# Trace dependencies against a tagged package release

Clone a git repository, check out the requested release tag, compare it
to the previous tag, and identify traced dependencies whose source files
changed.

## Usage

``` r
trace_release_impact(
  entry_script,
  repository,
  release_tag,
  package_name = NULL,
  previous_tag = NULL,
  output_dir = ".",
  output_prefix = NULL,
  output_format = c("csv", "json", "svg")
)
```

## Arguments

- entry_script:

  Path to the entry R script.

- repository:

  GitHub repository URL or local git repository path containing a tagged
  package release.

- release_tag:

  Release tag to analyze.

- package_name:

  Optional package name override.

- previous_tag:

  Optional previous tag override.

- output_dir:

  Directory where output artifacts are written.

- output_prefix:

  Prefix for output files. Defaults to entry script stem.

- output_format:

  Output file format. One of `"csv"`, `"json"`, or `"svg"`.

## Value

Invisibly returns a list with output path, dependency data, and release
impact metadata.
