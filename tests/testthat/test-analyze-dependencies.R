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

  return(list(
    work_dir = work_dir,
    demo_pkg = demo_pkg,
    entry_script = entry_script
  ))
}

build_release_dependency_fixture <- function() {
  work_dir <- tempfile("functracer-release-test-")
  dir.create(work_dir)

  repo_dir <- file.path(work_dir, "demoPkg")
  dir.create(repo_dir)
  dir.create(file.path(repo_dir, "R"))

  writeLines(
    c(
      "Package: demoPkg",
      "Version: 0.0.1",
      "Title: Demo Package",
      "Description: Minimal package for testing release impact.",
      "License: MIT"
    ),
    file.path(repo_dir, "DESCRIPTION")
  )

  writeLines(
    c("export(core_fn)", "export(helper_fn)"),
    file.path(repo_dir, "NAMESPACE")
  )

  writeLines(
    c(
      "core_fn <- function(x) {",
      "  helper_fn(x)",
      "}"
    ),
    file.path(repo_dir, "R", "core_fn.R")
  )

  writeLines(
    c(
      "helper_fn <- function(x) {",
      "  x + 1",
      "}"
    ),
    file.path(repo_dir, "R", "helper_fn.R")
  )

  old_wd <- getwd()
  setwd(repo_dir)
  on.exit(setwd(old_wd), add = TRUE)

  run_fixture_git <- function(args) {
    output <- system2("git", args, stdout = TRUE, stderr = TRUE)
    exit_code <- attr(output, "status")

    if (!is.null(exit_code) && exit_code != 0) {
      stop(
        paste0(
          "Fixture git command failed: git ",
          paste(args, collapse = " "),
          "\n",
          paste(output, collapse = "\n")
        )
      )
    }

    return(invisible(output))
  }

  run_fixture_git(c("init", "-q"))
  run_fixture_git(c("config", "user.name", "Test User"))
  run_fixture_git(c("config", "user.email", "test@example.com"))
  run_fixture_git(c("add", "."))
  run_fixture_git(c(
    "-c",
    "commit.gpgSign=false",
    "commit",
    "-q",
    "-m",
    "Initial release"
  ))

  writeLines(
    c(
      "helper_fn <- function(x) {",
      "  x + 2",
      "}"
    ),
    file.path(repo_dir, "R", "helper_fn.R")
  )

  run_fixture_git(c("add", "R/helper_fn.R"))
  run_fixture_git(c(
    "-c",
    "commit.gpgSign=false",
    "commit",
    "-q",
    "-m",
    "Update helper"
  ))

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

  return(list(
    work_dir = work_dir,
    repo_dir = repo_dir,
    entry_script = entry_script
  ))
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
  expect_true(any(grepl("--repo-url", cli_output, fixed = TRUE)))
  expect_true(any(grepl("--release-tag", cli_output, fixed = TRUE)))
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

test_that("trace_release_impact flags changed dependencies", {
  fixture <- build_release_dependency_fixture()

  result <- trace_release_impact(
    entry_script = fixture$entry_script,
    repository = fixture$repo_dir,
    release_tag = "HEAD",
    previous_tag = "HEAD~1",
    output_format = "json",
    output_dir = fixture$work_dir,
    output_prefix = "demo"
  )

  expect_identical(result$previous_tag, "HEAD~1")
  expect_true(result$script_affected)
  expect_true(any(result$changed_dependencies[["function"]] == "helper_fn"))
  expect_false(any(result$changed_dependencies[["function"]] == "core_fn"))
  expect_true(any(result$dependencies$source_file_changed))
  expect_true(file.exists(result$output_path))
  expect_match(result$output_path, "demo_release_impact\\.json$")
})

test_that("resolve_previous_release_tag identifies correct previous tag", {
  work_dir <- tempfile("functracer-tag-test-")
  dir.create(work_dir)

  repo_dir <- file.path(work_dir, "test_repo")
  dir.create(repo_dir)

  writeLines("test file", file.path(repo_dir, "test.txt"))

  old_wd <- getwd()
  setwd(repo_dir)
  on.exit(setwd(old_wd), add = TRUE)

  run_tag_test_git <- function(args) {
    output <- system2("git", args, stdout = TRUE, stderr = TRUE)
    exit_code <- attr(output, "status")

    if (!is.null(exit_code) && exit_code != 0) {
      stop(
        paste0(
          "Test git command failed: git ",
          paste(args, collapse = " "),
          "\n",
          paste(output, collapse = "\n")
        )
      )
    }

    return(invisible(output))
  }

  run_tag_test_git(c("init", "-q"))
  run_tag_test_git(c("config", "user.name", "Test User"))
  run_tag_test_git(c("config", "user.email", "test@example.com"))
  run_tag_test_git(c("add", "test.txt"))
  run_tag_test_git(c(
    "-c",
    "commit.gpgSign=false",
    "commit",
    "-q",
    "-m",
    "v1 commit"
  ))
  run_tag_test_git(c("tag", "v1.0.0"))

  writeLines("updated", file.path(repo_dir, "test.txt"))
  run_tag_test_git(c("add", "test.txt"))
  run_tag_test_git(c(
    "-c",
    "commit.gpgSign=false",
    "commit",
    "-q",
    "-m",
    "v2 commit"
  ))
  run_tag_test_git(c("tag", "v2.0.0"))

  writeLines("final", file.path(repo_dir, "test.txt"))
  run_tag_test_git(c("add", "test.txt"))
  run_tag_test_git(c(
    "-c",
    "commit.gpgSign=false",
    "commit",
    "-q",
    "-m",
    "v3 commit"
  ))
  run_tag_test_git(c("tag", "v3.0.0"))

  previous <- functracer:::resolve_previous_release_tag(
    repo_dir = repo_dir,
    release_tag = "v2.0.0"
  )
  expect_identical(previous, "v1.0.0")

  previous_latest <- functracer:::resolve_previous_release_tag(
    repo_dir = repo_dir,
    release_tag = "v3.0.0"
  )
  expect_identical(previous_latest, "v2.0.0")

  expect_error(
    functracer:::resolve_previous_release_tag(
      repo_dir = repo_dir,
      release_tag = "v1.0.0"
    ),
    "No previous tag found for release"
  )

  expect_error(
    functracer:::resolve_previous_release_tag(
      repo_dir = repo_dir,
      release_tag = "nonexistent"
    ),
    "Release tag not found in repository"
  )

  previous_override <- functracer:::resolve_previous_release_tag(
    repo_dir = repo_dir,
    release_tag = "v3.0.0",
    previous_tag = "v1.0.0"
  )
  expect_identical(previous_override, "v1.0.0")
})
