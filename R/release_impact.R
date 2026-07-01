#' Run a git command and return its output
#'
#' @param args Character vector of git arguments.
#' @param repo_dir Optional repository working directory.
#'
#' @return A character vector containing command output.
#' @keywords internal
#' @noRd
run_git_command <- function(args, repo_dir = NULL) {
  command_args <- args
  if (!is.null(repo_dir)) {
    command_args <- c("-C", repo_dir, command_args)
  }

  output <- system2("git", args = command_args, stdout = TRUE, stderr = TRUE)
  exit_code <- attr(output, "status")
  if (!is.null(exit_code) && exit_code != 0) {
    stop(paste(output, collapse = "\n"))
  }

  return(output)
}

#' List tags in a git repository
#'
#' @param repo_dir Path to a git repository.
#'
#' @return A character vector of tags sorted from newest to oldest.
#' @keywords internal
#' @noRd
list_git_tags <- function(repo_dir) {
  tags <- run_git_command(
    c("tag", "--sort=-version:refname"),
    repo_dir = repo_dir
  )

  return(tags[nzchar(tags)])
}

#' Resolve the previous release tag
#'
#' @param repo_dir Path to a git repository.
#' @param release_tag The release tag to analyze.
#' @param previous_tag Optional explicit previous tag override.
#'
#' @return The previous release tag.
#' @keywords internal
#' @noRd
resolve_previous_release_tag <- function(
  repo_dir,
  release_tag,
  previous_tag = NULL
) {
  if (!is.null(previous_tag) && nzchar(previous_tag)) {
    return(previous_tag)
  }

  tags <- list_git_tags(repo_dir)
  release_index <- match(release_tag, tags)

  if (is.na(release_index)) {
    stop("Release tag not found in repository: ", release_tag)
  }

  if (release_index == length(tags)) {
    stop("No previous tag found for release: ", release_tag)
  }

  return(tags[[release_index + 1L]])
}

#' Clone a repository for release analysis
#'
#' @param repository Repository URL or local path.
#'
#' @return Path to a temporary git checkout.
#' @keywords internal
#' @noRd
clone_release_repository <- function(repository) {
  checkout_dir <- tempfile("functracer-release-")
  clone_output <- run_git_command(c(
    "clone",
    "--quiet",
    repository,
    checkout_dir
  ))
  if (length(clone_output) > 0) {
    invisible(clone_output)
  }

  return(checkout_dir)
}

#' Lookup the source file for each dependency function
#'
#' @param dependency_names Function names from the dependency graph.
#' @param function_files Named list of source files keyed by function name.
#'
#' @return A character vector of source file basenames.
#' @keywords internal
#' @noRd
lookup_dependency_source_files <- function(dependency_names, function_files) {
  return(vapply(
    dependency_names,
    function(function_name) {
      if (function_name %in% names(function_files)) {
        return(function_files[[function_name]])
      } else {
        return(NA_character_)
      }
    },
    character(1)
  ))
}

#' Trace dependencies against a tagged package release
#'
#' Clone a git repository, check out the requested release tag, compare it to
#' the previous tag, and identify traced dependencies whose source files changed.
#'
#' @param entry_script Path to the entry R script.
#' @param repository GitHub repository URL or local git repository path containing a tagged package release.
#' @param release_tag Release tag to analyze.
#' @param package_name Optional package name override.
#' @param package_subdir Optional repository subdirectory containing the package.
#' @param previous_tag Optional previous tag override.
#' @param output_dir Directory where output artifacts are written.
#' @param output_prefix Prefix for output files. Defaults to entry script stem.
#' @param output_format Output file format. One of `"csv"`, `"json"`, or
#'   `"svg"`.
#'
#' @return Invisibly returns a list with output path, dependency data, and
#'   release impact metadata.
#' @export
trace_release_impact <- function(
  entry_script,
  repository,
  release_tag,
  package_name = NULL,
  package_subdir = NULL,
  previous_tag = NULL,
  output_dir = ".",
  output_prefix = NULL,
  output_format = c("csv", "json", "svg")
) {
  output_format <- match.arg(output_format)

  checkout_dir <- clone_release_repository(repository)
  on.exit(unlink(checkout_dir, recursive = TRUE, force = TRUE), add = TRUE)

  previous_tag <- resolve_previous_release_tag(
    repo_dir = checkout_dir,
    release_tag = release_tag,
    previous_tag = previous_tag
  )

  run_git_command(
    c("checkout", "--quiet", release_tag),
    repo_dir = checkout_dir
  )

  normalized_package_subdir <- ""
  if (!is.null(package_subdir) && nzchar(package_subdir)) {
    normalized_package_subdir <- gsub("^/+|/+$", "", package_subdir)
  }

  package_dir <- checkout_dir
  if (nzchar(normalized_package_subdir)) {
    package_dir <- file.path(checkout_dir, normalized_package_subdir)
  }

  analysis <- collect_dependency_analysis(
    entry_script = entry_script,
    package_dir = package_dir,
    package_name = package_name
  )

  dependencies <- analysis$dependencies
  dependencies$source_file <- lookup_dependency_source_files(
    dependency_names = dependencies[["function"]],
    function_files = analysis$function_files
  )

  changed_pathspec <- "R"
  changed_file_prefix <- "R/"
  if (nzchar(normalized_package_subdir)) {
    changed_pathspec <- file.path(normalized_package_subdir, "R")
    changed_file_prefix <- changed_pathspec
    changed_file_prefix <- paste0(changed_file_prefix, "/")
  }

  changed_files <- run_git_command(
    c(
      "diff",
      "--name-only",
      paste0(previous_tag, "..", release_tag),
      "--",
      changed_pathspec
    ),
    repo_dir = checkout_dir
  )
  changed_r_files <- basename(
    changed_files[startsWith(changed_files, changed_file_prefix)]
  )

  dependencies$source_file_changed <- dependencies$source_file %in%
    changed_r_files
  dependencies$release_tag <- release_tag
  dependencies$previous_tag <- previous_tag

  changed_dependencies <- dependencies[
    dependencies$source_file_changed,
    ,
    drop = FALSE
  ]
  script_affected <- nrow(changed_dependencies) > 0

  if (is.null(output_prefix) || identical(output_prefix, "")) {
    output_prefix <- tools::file_path_sans_ext(basename(entry_script))
  }

  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  extension <- switch(
    output_format,
    csv = "csv",
    json = "json",
    svg = "svg"
  )
  output_path <- file.path(
    output_dir,
    paste0(output_prefix, "_release_impact.", extension)
  )

  if (output_format == "csv") {
    write_dependencies_csv(dependencies, output_path)
  } else if (output_format == "json") {
    write_dependencies_json(dependencies, output_path)
  } else {
    write_dependencies_svg(dependencies, output_path)
  }

  message("Release impact analysis complete")
  message("Repository: ", repository)
  message("Release tag: ", release_tag)
  message("Previous tag: ", previous_tag)
  message("Changed dependencies: ", nrow(changed_dependencies))
  message("Script affected: ", script_affected)
  message("Output: ", output_path)

  return(invisible(list(
    output_path = output_path,
    output_format = output_format,
    dependencies = dependencies,
    changed_dependencies = changed_dependencies,
    changed_files = changed_files,
    release_tag = release_tag,
    previous_tag = previous_tag,
    repository = repository,
    script_affected = script_affected
  )))
}
