# Build a local Wave 4 household-crop panel for Paper 1.
# It reads authorised source data only from GHS_RAW_DIR and writes all row-level products to ignored local folders.

required_packages <- c("haven", "dplyr", "readr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) stop("Install required packages: ", paste(missing_packages, collapse = ", "))

library(dplyr)
library(readr)

project_dir <- Sys.getenv("POSTHARVEST_LOGISTICS_PROJECT_DIR", unset = getwd())
raw_dir <- Sys.getenv("GHS_RAW_DIR")
if (!nzchar(raw_dir)) stop("Set GHS_RAW_DIR to the authorised local GHS source-data folder before running this script.")

derived_data_dir <- file.path(project_dir, "data", "derived")
output_dir <- file.path(project_dir, "outputs")
dir.create(derived_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

source_file <- function(name) {
  path <- file.path(raw_dir, name)
  if (!file.exists(path)) stop("Missing required source file: ", path)
  path
}

read_source <- function(name) haven::read_dta(source_file(name))
numeric_value <- function(x) suppressWarnings(as.numeric(haven::zap_labels(x)))
yes_value <- function(x) as.integer(numeric_value(x) == 1)

# This is a source-derived labour-component record. It is deliberately constructed before task expansion.
# Reported days and hours describe work on any post-harvest activity for the crop.
self_source <- read_source("sectaphl1_harvestw4.dta")
crop_value_labels <- attr(self_source$cropcode, "labels")
crop_lookup <- tibble(
  cropcode = as.numeric(crop_value_labels),
  crop_label = unname(names(crop_value_labels))
) %>% distinct(cropcode, .keep_all = TRUE)

self_labour <- self_source %>%
  transmute(
    hhid,
    cropcode = numeric_value(cropcode),
    component = "household_member",
    worker_count = 1,
    days_per_worker = numeric_value(phl_q01b),
    hours_per_day = numeric_value(phl_q01c),
    person_hours = worker_count * days_per_worker * hours_per_day,
    valid_work_hours = !is.na(person_hours) & is.finite(person_hours) & person_hours > 0
  )

external_source <- read_source("sectaphl2_harvestw4.dta")
external_labour <- bind_rows(lapply(seq_len(3), function(group_index) {
  group_name <- c("men", "women", "children_under_15")[group_index]
  paid <- tibble(
    hhid = external_source$hhid,
    cropcode = numeric_value(external_source$cropcode),
    component = paste0("hired_paid_", group_name),
    paid_hire_reported = yes_value(external_source[[paste0("phl_q02_", group_index)]]),
    worker_count = numeric_value(external_source[[paste0("phl_q03_", group_index)]]),
    days_per_worker = numeric_value(external_source[[paste0("phl_q04_", group_index)]]),
    hours_per_day = numeric_value(external_source[[paste0("phl_q05_", group_index)]]),
    daily_payment = numeric_value(external_source[[paste0("phl_q06_", group_index)]])
  ) %>%
    mutate(
      person_hours = worker_count * days_per_worker * hours_per_day,
      valid_work_hours = paid_hire_reported == 1 & !is.na(person_hours) & is.finite(person_hours) & person_hours > 0
    )

  unpaid <- tibble(
    hhid = external_source$hhid,
    cropcode = numeric_value(external_source$cropcode),
    component = paste0("outside_household_unpaid_", group_name),
    paid_hire_reported = 0L,
    unpaid_work_reported = yes_value(external_source[[paste0("phl_q08__", group_index)]]),
    worker_count = numeric_value(external_source[[paste0("phl_q09_", group_index)]]),
    days_per_worker = numeric_value(external_source[[paste0("phl_q10_", group_index)]]),
    hours_per_day = numeric_value(external_source[[paste0("phl_q11_", group_index)]]),
    daily_payment = NA_real_
  ) %>%
    mutate(
      person_hours = worker_count * days_per_worker * hours_per_day,
      # Encode the source skip logic explicitly rather than relying on its implementation.
      valid_work_hours = unpaid_work_reported == 1 & !is.na(person_hours) & is.finite(person_hours) & person_hours > 0
    )
  bind_rows(paid, unpaid)
}))

labour_components <- bind_rows(
  self_labour %>% mutate(paid_hire_reported = 0L, daily_payment = NA_real_),
  external_labour
)

# One household-crop record is retained when any source component has valid recorded work-hours.
# Paid hiring is based on the source yes/no question, rather than inferred solely from nonmissing hours.
household_crop_labour <- labour_components %>%
  group_by(hhid, cropcode) %>%
  summarise(
    any_recorded_work = as.integer(any(valid_work_hours)),
    any_paid_hire_reported = as.integer(any(paid_hire_reported == 1, na.rm = TRUE)),
    any_paid_hire_with_valid_hours = as.integer(any(component %in% grep("^hired_paid_", component, value = TRUE) & valid_work_hours)),
    total_recorded_worker_hours = sum(if_else(valid_work_hours, person_hours, 0), na.rm = TRUE),
    paid_hired_worker_hours = sum(if_else(grepl("^hired_paid_", component) & valid_work_hours, person_hours, 0), na.rm = TRUE),
    household_member_worker_hours = sum(if_else(component == "household_member" & valid_work_hours, person_hours, 0), na.rm = TRUE),
    outside_household_unpaid_worker_hours = sum(if_else(grepl("^outside_household_unpaid_", component) & valid_work_hours, person_hours, 0), na.rm = TRUE),
    components_with_invalid_hours = sum(!valid_work_hours),
    .groups = "drop"
  ) %>%
  filter(any_recorded_work == 1) %>%
  mutate(
    paid_hired_hour_share = if_else(total_recorded_worker_hours > 0, paid_hired_worker_hours / total_recorded_worker_hours, NA_real_)
  )

household_frame <- read_source("secta_harvestw4.dta") %>%
  transmute(
    hhid,
    state = numeric_value(state),
    lga = numeric_value(lga),
    sector = numeric_value(sector),
    # The primary sampling unit is a source character identifier, not a numeric variable.
    cluster = as.character(cluster),
    strata = numeric_value(strata),
    rural = case_when(sector == 2 ~ 1L, sector == 1 ~ 0L, TRUE ~ NA_integer_),
    household_weight = numeric_value(wt_wave4)
  )

geovars <- read_source("NGA_HouseholdGeovars_Y4.dta") %>%
  transmute(
    hhid,
    distance_to_market_km = numeric_value(dist_market),
    log_distance_to_market = log1p(distance_to_market_km),
    distance_to_road_km = numeric_value(dist_road2),
    distance_to_population_center_km = numeric_value(dist_popcenter2)
  )

# Community market access is retained as a separate measure.
# The official Wave 4 community questionnaire defines Question C2.3 in kilometres.
community_market <- read_source("sectc2_harvestw4.dta") %>%
  filter(numeric_value(is_cd) == 222) %>%
  transmute(
    cluster = as.character(cluster_id),
    community_market_access_response = numeric_value(c2q1),
    community_market_distance_km = numeric_value(c2q3),
    community_market_one_way_cost = numeric_value(c2q5),
    community_market_travel_minutes = 60 * numeric_value(c2q6a) + numeric_value(c2q6b)
  ) %>%
  mutate(
    community_market_access_status = case_when(
      community_market_access_response == 1 & community_market_distance_km == 0 ~ "market_within_community",
      community_market_access_response == 1 & community_market_distance_km > 0 & community_market_distance_km < 999 ~ "external_market_reported_distance",
      community_market_access_response == 1 & community_market_distance_km == 999 ~ "market_access_distance_unknown",
      community_market_access_response == 2 ~ "no_market_access_reported",
      TRUE ~ "market_access_unknown"
    ),
    community_market_distance_km = if_else(community_market_distance_km == 999, NA_real_, community_market_distance_km)
  )

analytic_panel <- household_crop_labour %>%
  left_join(crop_lookup, by = "cropcode") %>%
  left_join(household_frame, by = "hhid") %>%
  left_join(geovars, by = "hhid") %>%
  left_join(community_market, by = "cluster") %>%
  mutate(
    eligible_weight = !is.na(household_weight) & is.finite(household_weight) & household_weight > 0,
    eligible_distance = !is.na(distance_to_market_km) & is.finite(distance_to_market_km) & distance_to_market_km >= 0,
    analytic_sample = as.integer(eligible_weight & eligible_distance)
  )

source_diagnostics <- bind_rows(
  tibble(stage = "self labour source rows", records = nrow(self_labour)),
  tibble(stage = "external labour component rows", records = nrow(external_labour)),
  tibble(stage = "household-crop records with valid recorded work", records = nrow(household_crop_labour)),
  tibble(stage = "matched to household frame", records = sum(!is.na(analytic_panel$household_weight))),
  tibble(stage = "matched to market-distance source", records = sum(!is.na(analytic_panel$distance_to_market_km))),
  tibble(stage = "matched to community market-access source", records = sum(!is.na(analytic_panel$community_market_access_response))),
  tibble(stage = "analytic household-crop records", records = sum(analytic_panel$analytic_sample == 1)),
  tibble(stage = "unique analytic households", records = n_distinct(analytic_panel$hhid[analytic_panel$analytic_sample == 1]))
)

coverage_diagnostics <- analytic_panel %>%
  summarise(
    household_crop_records = n(),
    analytic_records = sum(analytic_sample == 1),
    missing_or_invalid_weight = sum(!eligible_weight),
    missing_or_invalid_distance = sum(!eligible_distance),
    paid_hire_reported = sum(any_paid_hire_reported == 1, na.rm = TRUE),
    paid_hire_reported_without_valid_hours = sum(any_paid_hire_reported == 1 & any_paid_hire_with_valid_hours == 0, na.rm = TRUE),
    zero_or_missing_paid_hired_hour_share = sum(is.na(paid_hired_hour_share)),
    max_total_recorded_worker_hours = max(total_recorded_worker_hours, na.rm = TRUE)
  )

readr::write_rds(analytic_panel, file.path(derived_data_dir, "wave4_postharvest_logistics_panel.rds"), compress = "xz")
readr::write_csv(source_diagnostics, file.path(output_dir, "wave4_postharvest_logistics_source_flow.csv"))
readr::write_csv(coverage_diagnostics, file.path(output_dir, "wave4_postharvest_logistics_coverage.csv"))
message("local Wave 4 household-crop panel built: ", sum(analytic_panel$analytic_sample == 1), " analytic records.")
