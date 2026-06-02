test_that("analyze_dependencies rejects missing inputs", {
  expect_error(
    analyze_dependencies(
      entry_script = "does-not-exist.R",
      package_dir = tempdir()
    ),
    "Cannot find entry script"
  )
})
