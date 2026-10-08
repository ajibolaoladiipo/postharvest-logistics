# Preflight checks for the public code-only replication package.

project_dir <- Sys.getenv("POSTHARVEST_LOGISTICS_PROJECT_DIR", unset = getwd())
source(file.path(project_dir, "R", "project_utils.R"))

required_packages(c("dplyr", "haven", "readr", "survey", "yaml"))
assert_ignored_paths(project_dir)
raw_dir <- assert_local_raw_directory()

manifest <- readr::read_csv(file.path(project_dir, "config", "source_manifest.csv"), show_col_types = FALSE)
missing <- manifest$filename[!file.exists(file.path(raw_dir, manifest$filename))]
if (length(missing) > 0) {
  stop("Missing authorised source files: ", paste(missing, collapse = ", "), call. = FALSE)
}

message("Environment check passed. Source files were found locally and will not be copied into this repository.")
