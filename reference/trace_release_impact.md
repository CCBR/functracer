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
  package_subdir = NULL,
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

- package_subdir:

  Optional repository subdirectory containing the package.

- previous_tag:

  Optional previous tag override.

- output_dir:

  Directory where output artifacts are written.

- output_prefix:

  Prefix for output files. Defaults to entry script stem.

- output_format:

  Output file format. One of `"csv"`, `"json"`, or `"svg"`.

## Value

Invisibly returns a list with: `output_path`, `output_format`,
`dependencies`, `changed_dependencies`, `changed_files`, `release_tag`,
`previous_tag`, `repository`, and `script_affected`.

## Examples

``` r
if (FALSE) { # \dontrun{
temp_root <- tempfile("functracer-release-example-")
dir.create(temp_root)

repo_dir <- file.path(temp_root, "demo-repo")
dir.create(repo_dir)
dir.create(file.path(repo_dir, "R"))

writeLines(
  c(
    "Package: demoPkg",
    "Version: 0.0.1",
    "Title: Demo Package",
    "Description: Demo package for release examples.",
    "License: MIT"
  ),
  file.path(repo_dir, "DESCRIPTION")
)
writeLines("export(core_fn)", file.path(repo_dir, "NAMESPACE"))
writeLines(
  c(
    "core_fn <- function(x) {",
    "  x + 1",
    "}"
  ),
  file.path(repo_dir, "R", "core.R")
)

old_wd <- getwd()
setwd(repo_dir)
on.exit(setwd(old_wd), add = TRUE)
system("git init --quiet")
system("git config user.email 'example@example.com'")
system("git config user.name 'Example User'")
system("git add DESCRIPTION NAMESPACE R/core.R")
system("git commit -m 'initial release' --quiet")
system("git tag v0.1.0")

writeLines(
  c(
    "core_fn <- function(x) {",
    "  x + 2",
    "}"
  ),
  file.path(repo_dir, "R", "core.R")
)
system("git add R/core.R")
system("git commit -m 'update core' --quiet")
system("git tag v0.2.0")

entry_script <- file.path(temp_root, "main.R")
writeLines("core_fn(3)", entry_script)

out <- trace_release_impact(
  entry_script = entry_script,
  repository = repo_dir,
  release_tag = "v0.2.0",
  previous_tag = "v0.1.0",
  output_dir = temp_root,
  output_format = "csv"
)
out$script_affected
} # }
```
