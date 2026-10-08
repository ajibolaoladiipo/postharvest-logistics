# Shared public-repository safeguards.
# These helpers do not read data and may be sourced before the analysis scripts.

required_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Install required packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

project_root <- function() {
  root <- Sys.getenv("POSTHARVEST_LOGISTICS_PROJECT_DIR", unset = getwd())
  normalizePath(root, mustWork = TRUE)
}

assert_local_raw_directory <- function() {
  raw_dir <- Sys.getenv("GHS_RAW_DIR")
  if (!nzchar(raw_dir) || !dir.exists(raw_dir)) {
    stop("Set GHS_RAW_DIR to an existing local directory containing authorised source files.", call. = FALSE)
  }
  normalizePath(raw_dir, mustWork = TRUE)
}

assert_ignored_paths <- function(root = project_root()) {
  protected <- file.path(root, c("data/raw", "data/derived", "outputs"))
  if (!all(dir.exists(protected))) {
    stop("Expected local data and output directories are missing.", call. = FALSE)
  }
  invisible(protected)
}
