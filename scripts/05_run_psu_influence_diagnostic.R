# Chunked, local leave-one-PSU influence diagnostic for Paper 1 Model C.
# Run once per outcome and chunk. It creates no row-level export.

required_packages <- c("dplyr", "readr", "survey")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) stop("Install required packages: ", paste(missing_packages, collapse = ", "))
library(dplyr); library(readr); library(survey)

project_dir <- Sys.getenv("POSTHARVEST_LOGISTICS_PROJECT_DIR", unset = getwd())
panel_file <- file.path(project_dir, "data", "derived", "wave4_postharvest_logistics_panel.rds")
crosswalk_file <- file.path(project_dir, "config", "crop_group_crosswalk.csv")
output_dir <- file.path(project_dir, "outputs", "final_v01_2026-10-08")
outcome <- Sys.getenv("PSU_INFLUENCE_OUTCOME")
if (!outcome %in% c("paid_hire", "worker_hours")) stop("Set PSU_INFLUENCE_OUTCOME to paid_hire or worker_hours.")
chunk_size <- as.integer(Sys.getenv("PSU_CHUNK_SIZE", unset = "50"))
chunk_index <- as.integer(Sys.getenv("PSU_CHUNK_INDEX", unset = "1"))

crop_crosswalk <- readr::read_csv(crosswalk_file, show_col_types = FALSE) %>% mutate(cropcode = as.numeric(cropcode))
panel <- readr::read_rds(panel_file) %>% filter(analytic_sample == 1) %>% left_join(crop_crosswalk, by = "cropcode") %>%
  mutate(log_worker_hours = log(total_recorded_worker_hours), rural_factor = factor(rural, levels = c(0, 1), labels = c("urban", "rural")), crop_group_factor = factor(crop_group), state_factor = factor(state))
options(survey.lonely.psu = "adjust")
make_design <- function(data) svydesign(ids = ~cluster, strata = ~strata, weights = ~household_weight, nest = TRUE, data = data)
tidy_distance <- function(model, psu, data, design) {
  b <- coef(model)[["log_distance_to_market"]]; se <- sqrt(diag(vcov(model)))[["log_distance_to_market"]]
  df <- summary(model)$df.residual; cv <- qt(0.975, df = df)
  tibble(omitted_psu = psu, outcome = outcome, estimate = b, standard_error = se, residual_df = df,
         conf_low = b - cv * se, conf_high = b + cv * se, records = nrow(data), households = n_distinct(data$hhid), design_df = degf(design))
}
psus <- unique(panel$cluster)
start <- (chunk_index - 1L) * chunk_size + 1L; end <- min(length(psus), chunk_index * chunk_size)
if (start > length(psus)) stop("PSU chunk index is out of range.")
psus <- psus[start:end]
results <- bind_rows(lapply(psus, function(psu) {
  d <- panel %>% filter(cluster != psu); des <- make_design(d)
  fit <- if (outcome == "paid_hire") svyglm(any_paid_hire_reported ~ log_distance_to_market + rural_factor + crop_group_factor + state_factor, design = des, family = quasibinomial()) else svyglm(log_worker_hours ~ log_distance_to_market + rural_factor + crop_group_factor + state_factor, design = des)
  tidy_distance(fit, psu, d, des)
}))
write_csv(results, file.path(output_dir, paste0("table_s4_leave_one_psu_influence_", outcome, "_chunk", chunk_index, ".csv")))
message("Completed ", outcome, " PSU influence chunk ", chunk_index, ".")
