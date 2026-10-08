# Produce local construction diagnostics only. No statistical model is fitted in this audit.

required_packages <- c("dplyr", "readr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) stop("Install required packages: ", paste(missing_packages, collapse = ", "))

library(dplyr)
library(readr)

project_dir <- Sys.getenv("POSTHARVEST_LOGISTICS_PROJECT_DIR", unset = getwd())
derived_data_dir <- file.path(project_dir, "data", "derived")
output_dir <- file.path(project_dir, "outputs")
panel_file <- file.path(derived_data_dir, "wave4_postharvest_logistics_panel.rds")
if (!file.exists(panel_file)) stop("Run 01_build_wave4_postharvest_logistics_panel.R first.")

panel <- readr::read_rds(panel_file) %>% filter(analytic_sample == 1)

weighted_mean <- function(x, w) {
  ok <- !is.na(x) & is.finite(x) & !is.na(w) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok])
}

audit_summary <- panel %>%
  summarise(
    household_crop_records = n(),
    unique_households = n_distinct(hhid),
    weighted_paid_hire_share = weighted_mean(any_paid_hire_reported, household_weight),
    weighted_mean_distance_to_market_km = weighted_mean(distance_to_market_km, household_weight),
    unweighted_median_distance_to_market_km = stats::median(distance_to_market_km, na.rm = TRUE),
    unweighted_max_distance_to_market_km = max(distance_to_market_km, na.rm = TRUE),
    median_total_recorded_worker_hours = stats::median(total_recorded_worker_hours, na.rm = TRUE),
    p99_total_recorded_worker_hours = as.numeric(stats::quantile(total_recorded_worker_hours, 0.99, na.rm = TRUE)),
    max_total_recorded_worker_hours = max(total_recorded_worker_hours, na.rm = TRUE),
    paid_hire_reports_without_valid_hours = sum(any_paid_hire_reported == 1 & any_paid_hire_with_valid_hours == 0, na.rm = TRUE)
  )

component_diagnostics <- panel %>%
  summarise(
    household_member_hours = sum(household_member_worker_hours, na.rm = TRUE),
    paid_hired_hours = sum(paid_hired_worker_hours, na.rm = TRUE),
    unpaid_external_hours = sum(outside_household_unpaid_worker_hours, na.rm = TRUE),
    records_with_paid_hire = sum(any_paid_hire_reported == 1, na.rm = TRUE),
    records_with_positive_paid_hired_hours = sum(paid_hired_worker_hours > 0, na.rm = TRUE)
  )

distance_by_rurality <- panel %>%
  group_by(rural) %>%
  summarise(
    household_crop_records = n(),
    unique_households = n_distinct(hhid),
    weighted_mean_distance_to_market_km = weighted_mean(distance_to_market_km, household_weight),
    median_distance_to_market_km = stats::median(distance_to_market_km, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(audit_summary, file.path(output_dir, "wave4_postharvest_logistics_audit_summary.csv"))
readr::write_csv(component_diagnostics, file.path(output_dir, "wave4_postharvest_logistics_component_diagnostics.csv"))
readr::write_csv(distance_by_rurality, file.path(output_dir, "wave4_postharvest_logistics_distance_by_rurality.csv"))
message("local audit complete. Review output files before specifying descriptive models.")
