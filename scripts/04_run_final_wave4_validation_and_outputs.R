# Final local validation and output-generation workflow for Paper 1.
# It writes only non-row-level tables, figures, and machine-readable model summaries.
# Source microdata, identifiers, geographic variables, and the analytical panel remain local and ignored.

required_packages <- c("dplyr", "readr", "survey")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) stop("Install required packages: ", paste(missing_packages, collapse = ", "))

library(dplyr)
library(readr)
library(survey)

project_dir <- Sys.getenv("POSTHARVEST_LOGISTICS_PROJECT_DIR", unset = getwd())
derived_data_dir <- file.path(project_dir, "data", "derived")
output_dir <- file.path(project_dir, "outputs", "final_v01_2026-10-08")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
panel_file <- file.path(derived_data_dir, "wave4_postharvest_logistics_panel.rds")
crop_crosswalk_file <- file.path(project_dir, "config", "crop_group_crosswalk.csv")
if (!file.exists(panel_file)) stop("Run 01_build_wave4_postharvest_logistics_panel.R first.")
if (!file.exists(crop_crosswalk_file)) stop("Missing documented crop-group crosswalk.")

weighted_quantile <- function(x, w, probs = c(0.25, 0.5, 0.75)) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  ord <- order(x); x <- x[ord]; w <- w[ord]
  cw <- cumsum(w) / sum(w)
  vapply(probs, function(p) x[which(cw >= p)[1]], numeric(1))
}

weighted_correlation <- function(x, y, w) {
  keep <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
  x <- x[keep]; y <- y[keep]; w <- w[keep] / sum(w[keep])
  mx <- sum(w * x); my <- sum(w * y)
  sum(w * (x - mx) * (y - my)) / sqrt(sum(w * (x - mx)^2) * sum(w * (y - my)^2))
}

model_df <- function(model) {
  value <- summary(model)$df.residual
  if (length(value) != 1L || !is.finite(value) || value <= 0) stop("Model residual degrees of freedom are unavailable.")
  as.numeric(value)
}

model_tidy <- function(model, model_name, outcome, status, design, records, households, psus) {
  estimate <- stats::coef(model)
  standard_error <- sqrt(diag(stats::vcov(model)))
  residual_df <- model_df(model)
  critical_value <- stats::qt(0.975, df = residual_df)
  tibble(
    model = model_name,
    outcome = outcome,
    status = status,
    term = names(estimate),
    estimate = unname(estimate),
    standard_error = unname(standard_error),
    residual_df = residual_df,
    conf_low = estimate - critical_value * standard_error,
    conf_high = estimate + critical_value * standard_error,
    statistic = estimate / standard_error,
    p_value = 2 * stats::pt(abs(statistic), df = residual_df, lower.tail = FALSE),
    records = records,
    households = households,
    psus = psus,
    design_df = survey::degf(design)
  )
}

make_design <- function(data) {
  survey::svydesign(ids = ~cluster, strata = ~strata, weights = ~household_weight, nest = TRUE, data = data)
}

count_design_units <- function(data) {
  tibble(records = nrow(data), households = n_distinct(data$hhid), psus = n_distinct(data$cluster), states = n_distinct(data$state))
}

extract_distance_term <- function(model, label, outcome, status, design, data) {
  model_tidy(model, label, outcome, status, design, nrow(data), n_distinct(data$hhid), n_distinct(data$cluster)) %>%
    filter(term == "log_distance_to_market")
}

# Average model-based conditional predictions at observed weighted distance quartiles.
# The delta-method interval recognises coefficient uncertainty but does not turn this descriptive model into a causal contrast.
average_logit_prediction <- function(model, data, distance_value, label) {
  new_data <- data
  new_data$log_distance_to_market <- log1p(distance_value)
  terms_no_response <- stats::delete.response(stats::terms(model))
  x <- stats::model.matrix(terms_no_response, new_data)
  beta <- stats::coef(model)
  if (!identical(colnames(x), names(beta))) {
    x <- x[, names(beta), drop = FALSE]
  }
  eta <- as.vector(x %*% beta)
  probability <- stats::plogis(eta)
  w <- new_data$household_weight / sum(new_data$household_weight)
  estimate <- sum(w * probability)
  gradient <- colSums(x * as.vector(w * probability * (1 - probability)))
  standard_error <- sqrt(as.numeric(t(gradient) %*% stats::vcov(model) %*% gradient))
  df <- model_df(model)
  critical_value <- stats::qt(0.975, df = df)
  tibble(prediction = label, distance_km = distance_value, predicted_probability = estimate,
         standard_error = standard_error, residual_df = df,
         conf_low = max(0, estimate - critical_value * standard_error),
         conf_high = min(1, estimate + critical_value * standard_error))
}

crop_crosswalk <- readr::read_csv(crop_crosswalk_file, show_col_types = FALSE) %>% mutate(cropcode = as.numeric(cropcode))
panel <- readr::read_rds(panel_file) %>%
  filter(analytic_sample == 1) %>%
  left_join(crop_crosswalk, by = "cropcode") %>%
  mutate(
    log_worker_hours = log(total_recorded_worker_hours),
    rural_factor = factor(rural, levels = c(0, 1), labels = c("urban", "rural")),
    crop_group_factor = factor(crop_group),
    state_factor = factor(state)
  )

if (anyDuplicated(panel[c("hhid", "cropcode")])) stop("Duplicate household-crop keys in final panel.")
if (any(!is.finite(panel$log_worker_hours))) stop("Log worker-hour outcome is undefined.")
if (any(is.na(panel$cluster) | is.na(panel$strata) | is.na(panel$crop_group))) stop("Required design or crop-group field is missing.")
if (nrow(panel) != 6332L) stop("Primary analytic denominator changed. Stop and audit.")

options(survey.lonely.psu = "adjust")
distance_cuts <- weighted_quantile(panel$distance_to_market_km, panel$household_weight)
panel <- panel %>% mutate(
  distance_quartile = cut(distance_to_market_km, breaks = c(-Inf, distance_cuts, Inf),
                          labels = c("Q1 nearest", "Q2", "Q3", "Q4 furthest"), include.lowest = TRUE)
)
# Recalculate state-demeaned distance transparently with state-specific survey weights.
state_weighted_means <- panel %>% group_by(state) %>% summarise(
  state_weighted_log_distance = weighted.mean(log_distance_to_market, household_weight), .groups = "drop")
panel <- panel %>% left_join(state_weighted_means, by = "state") %>%
  mutate(state_demeaned_log_distance = log_distance_to_market - state_weighted_log_distance)

design <- make_design(panel)
design_units <- count_design_units(panel)
p99_worker_hours <- as.numeric(stats::quantile(panel$total_recorded_worker_hours, 0.99, na.rm = TRUE))
panel_drop_top <- panel %>% filter(total_recorded_worker_hours <= p99_worker_hours)
design_drop_top <- make_design(panel_drop_top)

# Registered primary sequence. Model C is a within-state conditional descriptive specification.
primary_models <- list(
  paid_hire_A = survey::svyglm(any_paid_hire_reported ~ log_distance_to_market, design = design, family = quasibinomial()),
  paid_hire_B = survey::svyglm(any_paid_hire_reported ~ log_distance_to_market + rural_factor + crop_group_factor, design = design, family = quasibinomial()),
  paid_hire_C = survey::svyglm(any_paid_hire_reported ~ log_distance_to_market + rural_factor + crop_group_factor + state_factor, design = design, family = quasibinomial()),
  worker_hours_A = survey::svyglm(log_worker_hours ~ log_distance_to_market, design = design),
  worker_hours_B = survey::svyglm(log_worker_hours ~ log_distance_to_market + rural_factor + crop_group_factor, design = design),
  worker_hours_C = survey::svyglm(log_worker_hours ~ log_distance_to_market + rural_factor + crop_group_factor + state_factor, design = design),
  worker_hours_C_drop_top_1pct = survey::svyglm(log_worker_hours ~ log_distance_to_market + rural_factor + crop_group_factor + state_factor, design = design_drop_top)
)

primary_model_ledger <- bind_rows(
  model_tidy(primary_models$paid_hire_A, "A", "Paid hired-labour participation", "Registered primary", design, design_units$records, design_units$households, design_units$psus),
  model_tidy(primary_models$paid_hire_B, "B", "Paid hired-labour participation", "Registered primary", design, design_units$records, design_units$households, design_units$psus),
  model_tidy(primary_models$paid_hire_C, "C", "Paid hired-labour participation", "Registered primary within-state conditional", design, design_units$records, design_units$households, design_units$psus),
  model_tidy(primary_models$worker_hours_A, "A", "Log recorded worker-hours", "Registered primary", design, design_units$records, design_units$households, design_units$psus),
  model_tidy(primary_models$worker_hours_B, "B", "Log recorded worker-hours", "Registered primary", design, design_units$records, design_units$households, design_units$psus),
  model_tidy(primary_models$worker_hours_C, "C", "Log recorded worker-hours", "Registered primary within-state conditional", design, design_units$records, design_units$households, design_units$psus),
  model_tidy(primary_models$worker_hours_C_drop_top_1pct, "C, 99th-percentile check", "Log recorded worker-hours", "Registered influence sensitivity", design_drop_top, nrow(panel_drop_top), n_distinct(panel_drop_top$hhid), n_distinct(panel_drop_top$cluster))
)
distance_terms <- primary_model_ledger %>% filter(term == "log_distance_to_market")

# Household-level post-result sensitivity: each final household weight contributes once.
household_panel <- panel %>% group_by(hhid) %>% summarise(
  household_weight = first(household_weight), cluster = first(cluster), strata = first(strata), state = first(state),
  rural = first(rural), distance_to_market_km = first(distance_to_market_km), log_distance_to_market = first(log_distance_to_market),
  any_paid_hire_reported = as.integer(any(any_paid_hire_reported == 1)),
  total_recorded_worker_hours = sum(total_recorded_worker_hours),
  log_worker_hours = log(total_recorded_worker_hours),
  .groups = "drop") %>% mutate(rural_factor = factor(rural, levels = c(0, 1), labels = c("urban", "rural")), state_factor = factor(state))
household_design <- make_design(household_panel)
household_models <- list(
  household_paid_hire_C = survey::svyglm(any_paid_hire_reported ~ log_distance_to_market + rural_factor + state_factor, design = household_design, family = quasibinomial()),
  household_worker_hours_C = survey::svyglm(log_worker_hours ~ log_distance_to_market + rural_factor + state_factor, design = household_design)
)
household_sensitivity <- bind_rows(
  model_tidy(household_models$household_paid_hire_C, "Household C", "Any paid hiring across eligible crops", "Post-result household sensitivity", household_design, nrow(household_panel), nrow(household_panel), n_distinct(household_panel$cluster)),
  model_tidy(household_models$household_worker_hours_C, "Household C", "Log total worker-hours across eligible crops", "Post-result household sensitivity", household_design, nrow(household_panel), nrow(household_panel), n_distinct(household_panel$cluster))
) %>% filter(term == "log_distance_to_market")

# State support and leave-one-state diagnostics.
state_support <- panel %>% group_by(state) %>% summarise(
  household_crop_records = n(), households = n_distinct(hhid), psus = n_distinct(cluster),
  weighted_share = sum(household_weight) / sum(panel$household_weight),
  weighted_mean_distance_km = weighted.mean(distance_to_market_km, household_weight),
  weighted_sd_log_distance = sqrt(sum(household_weight * (log_distance_to_market - weighted.mean(log_distance_to_market, household_weight))^2) / sum(household_weight)),
  .groups = "drop")

fit_state_omission <- function(state_value, outcome) {
  d <- panel %>% filter(state != state_value)
  des <- make_design(d)
  f <- if (outcome == "paid_hire") {
    survey::svyglm(any_paid_hire_reported ~ log_distance_to_market + rural_factor + crop_group_factor + state_factor, design = des, family = quasibinomial())
  } else {
    survey::svyglm(log_worker_hours ~ log_distance_to_market + rural_factor + crop_group_factor + state_factor, design = des)
  }
  extract_distance_term(f, paste0("Omit state ", state_value), outcome, "Post-result influence diagnostic", des, d)
}
state_influence <- bind_rows(lapply(sort(unique(panel$state)), fit_state_omission, outcome = "paid_hire"),
                             lapply(sort(unique(panel$state)), fit_state_omission, outcome = "worker_hours")) %>%
  mutate(omitted_state = as.numeric(gsub("Omit state ", "", model)))

# PSU deletion checks require no individual record to be silently removed from the main estimates.
# They are deliberately optional at the command level because 874 full survey refits are computationally expensive.
# Use RUN_PSU_INFLUENCE=paid_hire or RUN_PSU_INFLUENCE=worker_hours to generate each outcome's audit.
fit_psu_omission <- function(psu_value, outcome) {
  d <- panel %>% filter(cluster != psu_value)
  des <- make_design(d)
  f <- if (outcome == "paid_hire") {
    survey::svyglm(any_paid_hire_reported ~ log_distance_to_market + rural_factor + crop_group_factor + state_factor, design = des, family = quasibinomial())
  } else {
    survey::svyglm(log_worker_hours ~ log_distance_to_market + rural_factor + crop_group_factor + state_factor, design = des)
  }
  extract_distance_term(f, paste0("Omit PSU ", psu_value), outcome, "Post-result influence diagnostic", des, d)
}
run_psu_influence <- Sys.getenv("RUN_PSU_INFLUENCE", unset = "none")
if (run_psu_influence %in% c("paid_hire", "worker_hours")) {
  psu_values <- unique(panel$cluster)
  psu_chunk_size <- as.integer(Sys.getenv("PSU_CHUNK_SIZE", unset = "50"))
  psu_chunk_index <- as.integer(Sys.getenv("PSU_CHUNK_INDEX", unset = "1"))
  psu_start <- (psu_chunk_index - 1L) * psu_chunk_size + 1L
  psu_end <- min(length(psu_values), psu_chunk_index * psu_chunk_size)
  if (psu_start > length(psu_values)) stop("PSU chunk index is out of range.")
  psu_values <- psu_values[psu_start:psu_end]
  psu_influence <- bind_rows(lapply(psu_values, fit_psu_omission, outcome = run_psu_influence)) %>%
    mutate(omitted_psu = gsub("Omit PSU ", "", model))
}

# Support sensitivity omits only states represented by a single PSU. This is not an alternative primary estimand.
single_psu_states <- state_support %>% filter(psus == 1) %>% pull(state)
support_panel <- panel %>% filter(!state %in% single_psu_states)
support_design <- make_design(support_panel)
support_models <- list(
  paid_hire = survey::svyglm(any_paid_hire_reported ~ log_distance_to_market + rural_factor + crop_group_factor + state_factor, design = support_design, family = quasibinomial()),
  worker_hours = survey::svyglm(log_worker_hours ~ log_distance_to_market + rural_factor + crop_group_factor + state_factor, design = support_design)
)
support_sensitivity <- bind_rows(
  extract_distance_term(support_models$paid_hire, "C excluding one-PSU states", "Paid hired-labour participation", "Post-result support sensitivity", support_design, support_panel),
  extract_distance_term(support_models$worker_hours, "C excluding one-PSU states", "Log recorded worker-hours", "Post-result support sensitivity", support_design, support_panel)
)

# Registered quartile descriptives and component composition.
quartile_summary <- survey::svyby(~any_paid_hire_reported + total_recorded_worker_hours + paid_hired_hour_share,
                                  ~distance_quartile, design, survey::svymean, na.rm = TRUE, vartype = c("se", "ci")) %>% as_tibble()
component_summary <- survey::svymean(~household_member_worker_hours + paid_hired_worker_hours + outside_household_unpaid_worker_hours,
                                     design, na.rm = TRUE, vartype = c("se", "ci")) %>%
  as_tibble() %>%
  transmute(
    component = c("Household-member worker-hours", "Paid hired-worker hours", "Outside-household unpaid worker-hours"),
    weighted_mean_hours = mean,
    standard_error = SE
  )

# Exploratory, separately labelled community measures.
community_data <- panel %>% filter(community_market_access_status %in% c("market_within_community", "external_market_reported_distance", "no_market_access_reported")) %>%
  mutate(community_access_factor = factor(community_market_access_status,
    levels = c("market_within_community", "external_market_reported_distance", "no_market_access_reported"),
    labels = c("Market within community", "External market reported", "No market access reported")))
community_design <- make_design(community_data)
community_summary <- survey::svyby(~any_paid_hire_reported + total_recorded_worker_hours + paid_hired_hour_share,
                                   ~community_access_factor, community_design, survey::svymean, na.rm = TRUE, vartype = c("se", "ci")) %>% as_tibble()
community_models <- list(
  paid_A = survey::svyglm(any_paid_hire_reported ~ community_access_factor, design = community_design, family = quasibinomial()),
  paid_B = survey::svyglm(any_paid_hire_reported ~ community_access_factor + rural_factor + crop_group_factor + state_factor, design = community_design, family = quasibinomial()),
  hours_A = survey::svyglm(log_worker_hours ~ community_access_factor, design = community_design),
  hours_B = survey::svyglm(log_worker_hours ~ community_access_factor + rural_factor + crop_group_factor + state_factor, design = community_design)
)
community_model_ledger <- bind_rows(
  model_tidy(community_models$paid_A, "Community A", "Paid hired-labour participation", "Exploratory community measure", community_design, nrow(community_data), n_distinct(community_data$hhid), n_distinct(community_data$cluster)),
  model_tidy(community_models$paid_B, "Community B", "Paid hired-labour participation", "Exploratory community measure", community_design, nrow(community_data), n_distinct(community_data$hhid), n_distinct(community_data$cluster)),
  model_tidy(community_models$hours_A, "Community A", "Log recorded worker-hours", "Exploratory community measure", community_design, nrow(community_data), n_distinct(community_data$hhid), n_distinct(community_data$cluster)),
  model_tidy(community_models$hours_B, "Community B", "Log recorded worker-hours", "Exploratory community measure", community_design, nrow(community_data), n_distinct(community_data$hhid), n_distinct(community_data$cluster))
)
external_community <- community_data %>% filter(community_market_access_status == "external_market_reported_distance")
community_access_audit <- tibble(
  primary_household_crop_records = nrow(panel),
  records_with_community_status = sum(!is.na(panel$community_market_access_status)),
  records_with_both_distance_measures = sum(is.finite(panel$distance_to_market_km) & is.finite(panel$community_market_distance_km)),
  weighted_household_community_distance_correlation = weighted_correlation(panel$distance_to_market_km, panel$community_market_distance_km, panel$household_weight),
  external_market_records = nrow(external_community),
  external_market_communities = n_distinct(external_community$cluster),
  external_market_records_with_travel_minutes = sum(is.finite(external_community$community_market_travel_minutes)),
  external_market_records_with_one_way_cost = sum(is.finite(external_community$community_market_one_way_cost))
)
community_cost_time <- external_community %>% group_by(cluster) %>% summarise(
  distance_km = first(community_market_distance_km), travel_minutes = first(community_market_travel_minutes),
  one_way_cost = first(community_market_one_way_cost), .groups = "drop") %>% summarise(
  communities = n(), median_distance_km = median(distance_km, na.rm = TRUE), median_travel_minutes = median(travel_minutes, na.rm = TRUE),
  median_one_way_cost = median(one_way_cost, na.rm = TRUE), distance_nonmissing = sum(is.finite(distance_km)),
  time_nonmissing = sum(is.finite(travel_minutes)), cost_nonmissing = sum(is.finite(one_way_cost)))

# Conditional predicted probabilities from Model C at the observed weighted quartiles.
predicted_hiring <- bind_rows(
  average_logit_prediction(primary_models$paid_hire_C, panel, distance_cuts[1], "Weighted 25th percentile"),
  average_logit_prediction(primary_models$paid_hire_C, panel, distance_cuts[3], "Weighted 75th percentile")
)

# Non-disclosive tables and model ledger. No hhid, cluster id, coordinates, or row-level values are exported.
write_csv(tibble(
  statistic = c("Household-crop records", "Households", "Observed PSUs", "Strata", "States/FCT", "Design degrees of freedom", "Weighted distance Q1 km", "Weighted distance median km", "Weighted distance Q3 km", "Worker-hours 99th percentile", "Records above 99th percentile"),
  value = c(nrow(panel), n_distinct(panel$hhid), n_distinct(panel$cluster), n_distinct(panel$strata), n_distinct(panel$state), survey::degf(design), distance_cuts, p99_worker_hours, sum(panel$total_recorded_worker_hours > p99_worker_hours))
), file.path(output_dir, "table_1_analytic_sample_and_definitions.csv"))
write_csv(quartile_summary, file.path(output_dir, "table_2_distance_quartile_descriptives.csv"))
write_csv(distance_terms, file.path(output_dir, "table_3_primary_model_sequence.csv"))
write_csv(predicted_hiring, file.path(output_dir, "table_4_model_based_hiring_predictions.csv"))
write_csv(component_summary, file.path(output_dir, "table_5_worker_hour_components.csv"))
write_csv(household_sensitivity, file.path(output_dir, "table_s1_household_level_sensitivity.csv"))
write_csv(state_support, file.path(output_dir, "table_s2_state_support.csv"))
write_csv(state_influence, file.path(output_dir, "table_s3_leave_one_state_influence.csv"))
if (exists("psu_influence")) write_csv(psu_influence, file.path(output_dir, paste0("table_s4_leave_one_psu_influence_", run_psu_influence, "_chunk", psu_chunk_index, ".csv")))
write_csv(support_sensitivity, file.path(output_dir, "table_s5_state_support_sensitivity.csv"))
write_csv(community_summary, file.path(output_dir, "table_s6_exploratory_community_access_descriptives.csv"))
write_csv(community_model_ledger, file.path(output_dir, "table_s7_exploratory_community_access_models.csv"))
write_csv(community_access_audit, file.path(output_dir, "table_s8_community_measure_audit.csv"))
write_csv(community_cost_time, file.path(output_dir, "table_s9_external_community_market_time_cost.csv"))
write_csv(primary_model_ledger, file.path(output_dir, "model_ledger_final.csv"))

# Publication figures.
png(file.path(output_dir, "figure_1_distance_quartile_labour_patterns.png"), width = 2200, height = 1000, res = 220)
par(mfrow = c(1, 2), mar = c(6, 5, 3, 1))
q <- quartile_summary
plot(seq_len(nrow(q)), q$any_paid_hire_reported, ylim = c(0, 0.7), xaxt = "n", xlab = "Household distance-to-market quartile", ylab = "Weighted paid-hire share", pch = 19, col = "#1B4F72", main = "Paid post-harvest hiring")
arrows(seq_len(nrow(q)), q$ci_l.any_paid_hire_reported, seq_len(nrow(q)), q$ci_u.any_paid_hire_reported, angle = 90, code = 3, length = 0.05, col = "#1B4F72")
axis(1, at = seq_len(nrow(q)), labels = q$distance_quartile)
plot(seq_len(nrow(q)), q$total_recorded_worker_hours, ylim = c(0, max(q$ci_u.total_recorded_worker_hours) * 1.1), xaxt = "n", xlab = "Household distance-to-market quartile", ylab = "Weighted mean recorded worker-hours", pch = 19, col = "#7D3C98", main = "Recorded post-harvest worker-hours")
arrows(seq_len(nrow(q)), q$ci_l.total_recorded_worker_hours, seq_len(nrow(q)), q$ci_u.total_recorded_worker_hours, angle = 90, code = 3, length = 0.05, col = "#7D3C98")
axis(1, at = seq_len(nrow(q)), labels = q$distance_quartile)
dev.off()

png(file.path(output_dir, "figure_2_primary_model_coefficients.png"), width = 1800, height = 1200, res = 220)
plot_data <- distance_terms %>% filter(model %in% c("A", "B", "C")) %>% mutate(label = paste(outcome, model, sep = "  "))
plot(seq_len(nrow(plot_data)), plot_data$estimate, xlim = range(c(plot_data$conf_low, plot_data$conf_high)), ylim = c(0.5, nrow(plot_data) + 0.5), yaxt = "n", xlab = "Coefficient on log(1 + household distance to nearest market)", ylab = "", pch = 19, col = "#1B4F72", main = "Primary model sequence")
abline(v = 0, lty = 2, col = "grey40")
segments(plot_data$conf_low, seq_len(nrow(plot_data)), plot_data$conf_high, seq_len(nrow(plot_data)), col = "#1B4F72", lwd = 2)
axis(2, at = seq_len(nrow(plot_data)), labels = plot_data$label, las = 1)
dev.off()

png(file.path(output_dir, "figure_s1_state_demeaned_distance_distribution.png"), width = 1800, height = 900, res = 220)
par(mfrow = c(1, 2), mar = c(5, 5, 3, 1))
hist(panel$log_distance_to_market, breaks = 35, col = "#85C1E9", border = "white", main = "Raw household distance", xlab = "log(1 + kilometres to nearest market)")
hist(panel$state_demeaned_log_distance, breaks = 35, col = "#A9DFBF", border = "white", main = "Within-state distance variation", xlab = "State-demeaned log(1 + kilometres)")
dev.off()

png(file.path(output_dir, "figure_s2_state_influence.png"), width = 1800, height = 1000, res = 220)
par(mfrow = c(1, 2), mar = c(5, 5, 3, 1))
for (outcome_value in c("paid_hire", "worker_hours")) {
  d <- state_influence %>% filter(outcome == outcome_value) %>% arrange(omitted_state)
  plot(d$omitted_state, d$estimate, pch = 19, col = "#1B4F72", xlab = "Omitted state/FCT code", ylab = "Leave-one-state distance coefficient", main = outcome_value)
  abline(h = distance_terms %>% filter(outcome == ifelse(outcome_value == "paid_hire", "Paid hired-labour participation", "Log recorded worker-hours"), model == "C") %>% pull(estimate), lty = 2, col = "grey40")
}
dev.off()

saveRDS(list(primary_models = primary_models, household_models = household_models, community_models = community_models),
        file.path(derived_data_dir, "wave4_postharvest_logistics_final_models.rds"), compress = "xz")
message("Final local Paper 1 validation and analysis outputs complete: ", output_dir)
