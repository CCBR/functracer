build_demo_dependency_fixture <- function() {
  work_dir <- tempfile("functracer-test-")
  dir.create(work_dir)

  demo_pkg <- file.path(work_dir, "demoPkg")
  dir.create(demo_pkg)
  dir.create(file.path(demo_pkg, "R"))

  writeLines(
    c(
      "Package: demoPkg",
      "Version: 0.0.1",
      "Title: Demo Package",
      "Description: Minimal package for testing functracer.",
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

  list(work_dir = work_dir, demo_pkg = demo_pkg, entry_script = entry_script)
}

test_that("analyze_dependencies rejects missing inputs", {
  expect_error(
    analyze_dependencies(
      entry_script = "does-not-exist.R",
      package_dir = tempdir()
    ),
    "Cannot find entry script"
  )
})

test_that("CLI help is available through argparse", {
  skip_if_not_installed("argparse")

  cli_path <- system.file("scripts", "functracer.R", package = "functracer")
  expect_true(nzchar(cli_path))

  cli_output <- system2(
    command = file.path(R.home("bin"), "Rscript"),
    args = c(cli_path, "--help"),
    stdout = TRUE,
    stderr = TRUE
  )

  expect_true(any(grepl("--entry", cli_output, fixed = TRUE)))
  expect_true(any(grepl("--package-dir", cli_output, fixed = TRUE)))
  expect_true(any(grepl("--output-format", cli_output, fixed = TRUE)))
})

test_that("analyze_dependencies returns a dependency data frame", {
  fixture <- build_demo_dependency_fixture()

  dependencies <- analyze_dependencies(
    entry_script = fixture$entry_script,
    package_dir = fixture$demo_pkg
  )

  expect_s3_class(dependencies, "data.frame")
  expect_true(all(
    c(
      "function",
      "dep_type",
      "hop_depth",
      "call_path",
      "source",
      "is_exported"
    ) %in%
      colnames(dependencies)
  ))
  expect_false(file.exists(file.path(
    fixture$work_dir,
    "main_dependencies.csv"
  )))
  expect_false(file.exists(file.path(
    fixture$work_dir,
    "main_dependencies.json"
  )))
  expect_false(file.exists(file.path(
    fixture$work_dir,
    "main_dependencies.svg"
  )))
})

test_that("trace_functions writes the selected output format", {
  fixture <- build_demo_dependency_fixture()

  result <- trace_functions(
    entry_script = fixture$entry_script,
    package_dir = fixture$demo_pkg,
    output_format = "json",
    output_dir = fixture$work_dir,
    output_prefix = "demo"
  )

  expect_identical(result$output_format, "json")
  expect_true(file.exists(result$output_path))
  expect_match(result$output_path, "demo_dependencies\\.json$")
  expect_false(file.exists(file.path(
    fixture$work_dir,
    "demo_dependencies.csv"
  )))
  expect_false(file.exists(file.path(
    fixture$work_dir,
    "demo_dependencies.svg"
  )))
})
