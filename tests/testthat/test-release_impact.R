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
    cmd <- paste(c("git", shQuote(args)), collapse = " ")
    output <- system(cmd, intern = TRUE, ignore.stderr = FALSE)
    exit_code <- attr(output, "status")

    if (!is.null(exit_code) && exit_code != 0) {
      stop(
        paste0(
          "Fixture git command failed: ",
          cmd,
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

build_release_dependency_monorepo_fixture <- function() {
  work_dir <- tempfile("functracer-release-monorepo-test-")
  dir.create(work_dir)

  repo_dir <- file.path(work_dir, "demoMonorepo")
  dir.create(repo_dir)
  dir.create(file.path(repo_dir, "modules", "hello", "R"), recursive = TRUE)
  dir.create(file.path(repo_dir, "modules", "other", "R"), recursive = TRUE)

  writeLines(
    c(
      "Package: helloPkg",
      "Version: 0.0.1",
      "Title: Hello Package",
      "Description: Package in a monorepo subdirectory.",
      "License: MIT"
    ),
    file.path(repo_dir, "modules", "hello", "DESCRIPTION")
  )

  writeLines(
    c("export(core_fn)", "export(helper_fn)"),
    file.path(repo_dir, "modules", "hello", "NAMESPACE")
  )

  writeLines(
    c(
      "core_fn <- function(x) {",
      "  helper_fn(x)",
      "}"
    ),
    file.path(repo_dir, "modules", "hello", "R", "core_fn.R")
  )

  writeLines(
    c(
      "helper_fn <- function(x) {",
      "  x + 1",
      "}"
    ),
    file.path(repo_dir, "modules", "hello", "R", "helper_fn.R")
  )

  writeLines(
    c(
      "other_fn <- function(y) {",
      "  y * 2",
      "}"
    ),
    file.path(repo_dir, "modules", "other", "R", "other_fn.R")
  )

  old_wd <- getwd()
  setwd(repo_dir)
  on.exit(setwd(old_wd), add = TRUE)

  run_fixture_git <- function(args) {
    cmd <- paste(c("git", shQuote(args)), collapse = " ")
    output <- system(cmd, intern = TRUE, ignore.stderr = FALSE)
    exit_code <- attr(output, "status")

    if (!is.null(exit_code) && exit_code != 0) {
      stop(
        paste0(
          "Fixture git command failed: ",
          cmd,
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
    "Initial monorepo release"
  ))
  initial_sha <- trimws(system("git rev-parse HEAD", intern = TRUE))

  writeLines(
    c(
      "other_fn <- function(y) {",
      "  y * 3",
      "}"
    ),
    file.path(repo_dir, "modules", "other", "R", "other_fn.R")
  )
  run_fixture_git(c("add", "modules/other/R/other_fn.R"))
  run_fixture_git(c(
    "-c",
    "commit.gpgSign=false",
    "commit",
    "-q",
    "-m",
    "Update outside target package subdir"
  ))
  outside_sha <- trimws(system("git rev-parse HEAD", intern = TRUE))

  writeLines(
    c(
      "helper_fn <- function(x) {",
      "  x + 2",
      "}"
    ),
    file.path(repo_dir, "modules", "hello", "R", "helper_fn.R")
  )
  run_fixture_git(c("add", "modules/hello/R/helper_fn.R"))
  run_fixture_git(c(
    "-c",
    "commit.gpgSign=false",
    "commit",
    "-q",
    "-m",
    "Update target package subdir"
  ))
  inside_sha <- trimws(system("git rev-parse HEAD", intern = TRUE))

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
    entry_script = entry_script,
    initial_sha = initial_sha,
    outside_sha = outside_sha,
    inside_sha = inside_sha
  ))
}

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

test_that("trace_release_impact ignores changes outside package_subdir", {
  fixture <- build_release_dependency_monorepo_fixture()

  result <- trace_release_impact(
    entry_script = fixture$entry_script,
    repository = fixture$repo_dir,
    release_tag = fixture$outside_sha,
    previous_tag = fixture$initial_sha,
    package_subdir = "modules/hello",
    output_format = "json",
    output_dir = fixture$work_dir,
    output_prefix = "hello_outside_only"
  )

  expect_false(result$script_affected)
  expect_identical(nrow(result$changed_dependencies), 0L)
  expect_false(any(result$dependencies$source_file_changed))
  expect_true(file.exists(result$output_path))
  expect_match(result$output_path, "hello_outside_only_release_impact\\.json$")
})

test_that("trace_release_impact detects changes inside package_subdir", {
  fixture <- build_release_dependency_monorepo_fixture()

  result <- trace_release_impact(
    entry_script = fixture$entry_script,
    repository = fixture$repo_dir,
    release_tag = fixture$inside_sha,
    previous_tag = fixture$outside_sha,
    package_subdir = "modules/hello",
    output_format = "json",
    output_dir = fixture$work_dir,
    output_prefix = "hello_inside_change"
  )

  expect_true(result$script_affected)
  expect_true(any(result$changed_dependencies[["function"]] == "helper_fn"))
  expect_true(any(result$dependencies$source_file_changed))
  expect_true(file.exists(result$output_path))
  expect_match(result$output_path, "hello_inside_change_release_impact\\.json$")
})
