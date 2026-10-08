# Run the registered descriptive analysis for Paper 1 after the construction audit.
# All outputs remain local and ignored by Git.

required_packages <- c("dplyr", "readr", "survey")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) stop("Install required packages: ", paste(missing_packages, collapse = ", "))

library(dplyr)
library(readr)
library(survey)

project_dir <- Sys.getenv("POSTHARVEST_LOGISTICS_PROJECT_DIR", unset = getwd())
derived_data_dir <- file.path(project_dir, "data", "derived")
output_dir <- file.path(project_dir, "outputs")
panel_file <- file.path(derived_data_dir, "wave4_postharvest_logistics_panel.rds")
if (!file.exists(panel_file)) stop("Run 01_build_wave4_postharvest_logistics_panel.R first.")
crop_crosswalk_file <- file.path(project_dir, "config", "crop_group_crosswalk.csv")
if (!file.exists(crop_crosswalk_file)) stop("Missing documented crop-group crosswalk: ", crop_crosswalk_file)

weighted_quartile <- function(x, w, probs = c(0.25, 0.50, 0.75)) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]
  w <- w[keep]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cumulative_weight <- cumsum(w) / sum(w)
  vapply(probs, function(p) x[which(cumulative_weight >= p)[1]], numeric(1))
}

weighted_correlation <- function(x, y, w) {
  keep <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
  if (sum(keep) < 2) return(NA_real_)
  x <- x[keep]
  y <- y[keep]
  w <- w[keep] / sum(w[keep])
  x_mean <- sum(w * x)
  y_mean <- sum(w * y)
  sum(w * (x - x_mean) * (y - y_mean)) / sqrt(sum(w * (x - x_mean)^2) * sum(w * (y - y_mean)^2))
}

tidy_svyglm <- function(model, model_name, design_degrees_freedom) {
  est <- stats::coef(model)
  se <- sqrt(diag(stats::vcov(model)))
  tibble(
    model = model_name,
    term = names(est),
    estimate = unname(est),
    standard_error = unname(se),
    statistic = estimate / standard_error,
    p_value = 2 * stats::pt(abs(statistic), df = design_degrees_freedom, lower.tail = FALSE),
    conf_low = estimate - stats::qt(0.975, df = design_degrees_freedom) * standard_error,
    conf_high = estimate + stats::qt(0.975, df = design_degrees_freedom) * standard_error
  )
}

crop_crosswalk <- readr::read_csv(crop_crosswalk_file, show_col_types = FALSE) %>%
  mutate(cropcode = as.numeric(cropcode))

panel <- readr::read_rds(panel_file) %>%
  filter(analytic_sample == 1) %>%
  left_join(crop_crosswalk, by = "cropcode") %>%
  mutate(
    log_worker_hours = log(total_recorded_worker_hours),
    rural_factor = factor(rural, levels = c(0, 1), labels = c("urban", "rural")),
    crop_group_factor = factor(crop_group),
    state_factor = factor(state)
  )

if (any(!is.finite(panel$log_worker_hours))) stop("Registered log worker-hours outcome is undefined. Revisit construction before estimation.")
if (any(is.na(panel$cluster) | is.na(panel$strata))) stop("Survey cluster or strata missing. Revisit household-frame construction before estimation.")
if (any(is.na(panel$crop_group))) stop("At least one observed crop lacks a documented crop-group mapping. Revisit the crosswalk before estimation.")

quartile_cuts <- weighted_quartile(panel$distance_to_market_km, panel$household_weight)
if (length(unique(quartile_cuts)) < 3) stop("Weighted distance quartile cut points are not distinct. Do not proceed with quartile presentation.")

panel <- panel %>%
  mutate(
    distance_quartile = cut(
      distance_to_market_km,
      breaks = c(-Inf, quartile_cuts, Inf),
      labels = c("Q1 nearest", "Q2", "Q3", "Q4 furthest"),
      include.lowest = TRUE,
      right = TRUE
    )
  )

options(survey.lonely.psu = "adjust")
design <- survey::svydesign(
  ids = ~cluster,
  strata = ~strata,
  weights = ~household_weight,
  nest = TRUE,
  data = panel
)

# The primary design uses the first-stage cluster because only final weights are available.
# Under the survey package documentation, adding lower-stage identifiers without finite-population corrections does not change variance estimation.
p99_worker_hours <- as.numeric(stats::quantile(panel$total_recorded_worker_hours, 0.99, na.rm = TRUE))
design_without_top_one_percent <- subset(design, total_recorded_worker_hours <= p99_worker_hours)

community_access_design <- subset(
  design,
  !is.na(community_market_access_status) & community_market_access_status != "market_access_distance_unknown"
)

community_access_design <- update(
  community_access_design,
  community_access_factor = factor(
    community_market_access_status,
    levels = c("market_within_community", "external_market_reported_distance", "no_market_access_reported"),
    labels = c("market within community", "external market with reported distance", "no market access reported")
  )
)

quartile_summary <- survey::svyby(
  ~any_paid_hire_reported + total_recorded_worker_hours + paid_hired_hour_share,
  ~distance_quartile,
  design,
  survey::svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")
) %>% as_tibble()

models <- list(
  paid_hire_A = survey::svyglm(any_paid_hire_reported ~ log_distance_to_market, design = design, family = quasibinomial()),
  paid_hire_B = survey::svyglm(any_paid_hire_reported ~ log_distance_to_market + rural_factor + crop_group_factor, design = design, family = quasibinomial()),
  paid_hire_C = survey::svyglm(any_paid_hire_reported ~ log_distance_to_market + rural_factor + crop_group_factor + state_factor, design = design, family = quasibinomial()),
  worker_hours_A = survey::svyglm(log_worker_hours ~ log_distance_to_market, design = design),
  worker_hours_B = survey::svyglm(log_worker_hours ~ log_distance_to_market + rural_factor + crop_group_factor, design = design),
  worker_hours_C = survey::svyglm(log_worker_hours ~ log_distance_to_market + rural_factor + crop_group_factor + state_factor, design = design),
  worker_hours_C_drop_top_1pct = survey::svyglm(log_worker_hours ~ log_distance_to_market + rural_factor + crop_group_factor + state_factor, design = design_without_top_one_percent)
)

model_degrees_freedom <- c(
  paid_hire_A = survey::degf(design),
  paid_hire_B = survey::degf(design),
  paid_hire_C = survey::degf(design),
  worker_hours_A = survey::degf(design),
  worker_hours_B = survey::degf(design),
  worker_hours_C = survey::degf(design),
  worker_hours_C_drop_top_1pct = survey::degf(design_without_top_one_percent)
)

model_output <- bind_rows(Map(function(model, model_name) {
  tidy_svyglm(model, model_name, model_degrees_freedom[[model_name]])
}, models, names(models)))

influence_audit <- panel %>%
  transmute(
    hhid, cropcode, crop_label, total_recorded_worker_hours, paid_hired_worker_hours,
    household_member_worker_hours, outside_household_unpaid_worker_hours,
    distance_to_market_km, household_weight
  ) %>%
  arrange(desc(total_recorded_worker_hours)) %>%
  slice_head(n = 25)

community_access_summary <- survey::svyby(
  ~any_paid_hire_reported + total_recorded_worker_hours + paid_hired_hour_share,
  ~community_access_factor,
  community_access_design,
  survey::svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")
) %>% as_tibble()

community_access_models <- list(
  community_access_paid_hire_A = survey::svyglm(any_paid_hire_reported ~ community_access_factor, design = community_access_design, family = quasibinomial()),
  community_access_paid_hire_B = survey::svyglm(any_paid_hire_reported ~ community_access_factor + rural_factor + crop_group_factor + state_factor, design = community_access_design, family = quasibinomial()),
  community_access_worker_hours_A = survey::svyglm(log_worker_hours ~ community_access_factor, design = community_access_design),
  community_access_worker_hours_B = survey::svyglm(log_worker_hours ~ community_access_factor + rural_factor + crop_group_factor + state_factor, design = community_access_design)
)

community_access_model_output <- bind_rows(Map(function(model, model_name) {
  tidy_svyglm(model, model_name, survey::degf(community_access_design))
}, community_access_models, names(community_access_models)))

community_access_measure_audit <- tibble(
  household_crop_records = nrow(panel),
  records_with_community_market_status = sum(!is.na(panel$community_market_access_status)),
  records_with_community_market_distance_km = sum(is.finite(panel$community_market_distance_km)),
  weighted_correlation_household_and_community_market_distance = weighted_correlation(
    panel$distance_to_market_km,
    panel$community_market_distance_km,
    panel$household_weight
  )
)

readr::write_csv(tibble(
  percentile = c(25, 50, 75),
  distance_to_market_km = quartile_cuts,
  quartile_definition = "Weighted empirical quantile using positive Wave 4 cross-sectional weights"
), file.path(output_dir, "wave4_postharvest_logistics_weighted_distance_quartiles.csv"))
readr::write_csv(tibble(
  influence_rule = "Exclude household-crop records above the unweighted 99th percentile of total recorded worker-hours",
  threshold_total_recorded_worker_hours = p99_worker_hours,
  excluded_records = sum(panel$total_recorded_worker_hours > p99_worker_hours)
), file.path(output_dir, "wave4_postharvest_logistics_worker_hour_influence_rule.csv"))
readr::write_csv(quartile_summary, file.path(output_dir, "wave4_postharvest_logistics_quartile_summary.csv"))
readr::write_csv(model_output, file.path(output_dir, "wave4_postharvest_logistics_registered_models.csv"))
readr::write_csv(influence_audit, file.path(output_dir, "wave4_postharvest_logistics_top_worker_hour_records.csv"))
readr::write_csv(community_access_summary, file.path(output_dir, "wave4_postharvest_logistics_community_access_summary.csv"))
readr::write_csv(community_access_model_output, file.path(output_dir, "wave4_postharvest_logistics_community_access_models.csv"))
readr::write_csv(community_access_measure_audit, file.path(output_dir, "wave4_postharvest_logistics_community_access_measure_audit.csv"))
saveRDS(models, file.path(derived_data_dir, "wave4_postharvest_logistics_registered_models.rds"), compress = "xz")
message("Registered Wave 4 descriptive analysis complete. Outputs remain local pending audit and interpretation.")
