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
})
