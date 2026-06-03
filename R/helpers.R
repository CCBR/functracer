infer_package_name <- function(description_file) {
  if (!file.exists(description_file)) {
    stop(
      "Cannot infer package name because DESCRIPTION is missing: ",
      description_file
    )
  }

  desc <- read.dcf(description_file)
  if (!"Package" %in% colnames(desc)) {
    stop("DESCRIPTION does not contain a Package field: ", description_file)
  }

  pkg <- desc[1, "Package"]
  if (!nzchar(pkg)) {
    stop("DESCRIPTION has an empty Package field: ", description_file)
  }

  pkg
}

extract_exported_functions <- function(namespace_path) {
  namespace_lines <- readLines(namespace_path, warn = FALSE)
  exported <- grep('^export\\(', namespace_lines, value = TRUE)
  exported <- sub('^export\\("?', "", exported)
  exported <- sub('"?\\)$', "", exported)
  unique(exported)
}
