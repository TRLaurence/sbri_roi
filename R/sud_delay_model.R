library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
source("R/utils_paths.R")
source("R/utils_parameter_vals.R")
source("R/graphs_all.R")


# Reimplementation of the core Sud et al. delay-survival formulas
# based on the published manuscript + appendix.
#
# Key interpretation:
#   beta_day is not itself a hazard ratio. It is a per-day log(HR) coefficient.
#   Sud's n-day delay HR is:
#       HR_n = exp(beta_day * n_days)

#---------------------------
# Core Sud logic of delay model
#---------------------------

delay_hr_n <- function(beta_day, n_days) {
  exp(beta_day * n_days)
}

#---------------------------
# Data helpers
#---------------------------

explode_to_daily <- function(rel_data_small) {
  year_range <- tibble::tibble(year = seq(1, max(rel_data_small$year), by = 1))
  
  year_range %>%
    left_join(rel_data_small, by = "year") %>%
    arrange(year) %>%
    fill(daily_hazard_rate, .direction = "up") %>%
    fill(country, cancer_type, stage, .direction = "up") %>%
    mutate(day_count = 365L) %>%
    uncount(weights = day_count) %>%
    group_by(country, cancer_type, stage, year) %>%
    mutate(day_of_year = row_number()) %>%
    ungroup() %>%
    mutate(
      year = year - 1L,
      days_since_diagnosis = year * 365L + day_of_year
    )
}

estimate_cumulative_mortality <- function(rel_daily) {
  rel_daily %>%
    arrange(days_since_diagnosis) %>%
    mutate(
      cumulative_hazard = cumsum(daily_hazard_rate),
      cumulative_survival = exp(-cumulative_hazard),
      cumulative_mortality = 1 - cumulative_survival
    )
}

apply_hr_and_estimate_survival <- function(rel_daily, hr_multiplier) {
  rel_daily %>%
    arrange(days_since_diagnosis) %>%
    mutate(
      daily_hazard_rate = daily_hazard_rate * hr_multiplier,
      cumulative_hazard = cumsum(daily_hazard_rate),
      cumulative_survival = exp(-cumulative_hazard),
      cumulative_mortality = 1 - cumulative_survival
    )
}

estimate_qaly <- function(survival_df, qaly_weight, discount_rate) {
  daily_qaly_contribution <- qaly_weight / 365
  
  survival_df %>%
    arrange(days_since_diagnosis) %>%
    mutate(
      years_since_diagnosis = days_since_diagnosis / 365,
      qaly = cumulative_survival * daily_qaly_contribution,
      discount_factor = 1 / ((1 + discount_rate) ^ years_since_diagnosis),
      discounted_qaly = qaly * discount_factor
    ) %>%
    summarise(total_qaly = sum(discounted_qaly), .groups = "drop") %>%
    pull(total_qaly)
}


qaly_comparison <- function(post_speed_up, rel_daily_baseline, utility_weight, discount_rate) {
  baseline_qaly <- estimate_qaly(
    rel_daily_baseline,
    qaly_weight = utility_weight,
    discount_rate = discount_rate
  )
  
  post_speed_up_qaly <- estimate_qaly(
    post_speed_up,
    qaly_weight = utility_weight,
    discount_rate = discount_rate
  )
  
  list(
    baseline_qaly = baseline_qaly,
    post_speed_up_qaly = post_speed_up_qaly,
    qaly_gains = post_speed_up_qaly - baseline_qaly
  )
}

format_results_to_survival_chart_df <- function(modelling_results_case_study) {
  baseline_daily <- modelling_results_case_study$baseline
  post_speed_up_daily <- modelling_results_case_study$post_speed_up
  checkpoint <- modelling_results_case_study$checkpoints
  
  stage <- dplyr::first(str_to_title(as.character(baseline_daily$stage)))
  cancer_type <- dplyr::first(str_to_title(baseline_daily$cancer_type))
  country <- dplyr::first(baseline_daily$country)
  
  rel_cols_to_keep <- c("days_since_diagnosis", "cumulative_survival")
  
  baseline_formatted <- baseline_daily %>%
    select(all_of(rel_cols_to_keep)) %>%
    mutate(scen = "Baseline")
  
  post_speed_up_formatted <- post_speed_up_daily %>%
    select(all_of(rel_cols_to_keep)) %>%
    mutate(scen = "Intervention")
  
  checkpoint_formatted <- checkpoint %>%
    select(days_since_diagnosis, observed_survival) %>%
    rename(cumulative_survival = observed_survival) %>%
    mutate(scen = "Observed")
  
  graph_df <- bind_rows(
    baseline_formatted,
    post_speed_up_formatted,
    checkpoint_formatted
  )
  
  plot_title_str <- paste("Survival Curves for", cancer_type, "Stage", stage, "in", country)
  
  list(
    graph_df = graph_df,
    plot_title_str = plot_title_str
  )
}

#---------------------------
# Sud default coefficients from Appendix Table 9
#---------------------------

modelling_results <- list()

# From evaluations
pancreatic_days_speed_up_base <- 7
pancreatic_days_speed_up_high <- pancreatic_days_speed_up_base*1.2
pancreatic_days_speed_up_low <- pancreatic_days_speed_up_base*0.8
brain_days_speed_up_base <- 7.3
brain_days_speed_up_high <- brain_days_speed_up_base*1.5
brain_days_speed_up_low <- brain_days_speed_up_base*0.5
melanoma_days_speed_up_high <- 62.8
melanoma_days_speed_up_low <- 9.9
melanoma_days_speed_up_base <- mean(c(melanoma_days_speed_up_high, melanoma_days_speed_up_low))

pancreatic_case_study_title <- "CS_SBRIC01P3008"
brain_cancer_case_study_title <- "SBRIC01P3041"
melanoma_case_study_title <- "SBRIC01P3030"

sud_beta_day <- c(
  high_conserv_hanna = 0.00275, # Comes from the central estimate of Hanna et al for Breast cancer which is low progressivity
  low_default        = 0.0030,
  moderate_default   = 0.0056,
  high_default       = 0.0056,
  low_minus_2sd      = 0.0025,
  low_plus_2sd       = 0.0035,
  moderate_minus_2sd = 0.0047,
  moderate_plus_2sd  = 0.0065,
  high_alternative   = 0.0105
)

stopifnot(unname(sud_beta_day["moderate_default"]) == unname(sud_beta_day["high_default"]))

# Base = current Sud default used for these three tumours in this script
# High / low = sensitivity bounds around that base coefficient
sud_beta_day[["pancreatic_high"]] <- sud_beta_day[["moderate_plus_2sd"]]
sud_beta_day[["pancreatic_base"]] <- sud_beta_day[["moderate_default"]]
sud_beta_day[["pancreatic_low"]]  <- sud_beta_day[["moderate_minus_2sd"]]

sud_beta_day[["brain_high"]] <- sud_beta_day[["moderate_plus_2sd"]]
sud_beta_day[["brain_base"]] <- sud_beta_day[["moderate_default"]]
sud_beta_day[["brain_low"]]  <- sud_beta_day[["moderate_minus_2sd"]]

sud_beta_day[["melanoma_high"]] <- sud_beta_day[["moderate_plus_2sd"]]
sud_beta_day[["melanoma_base"]] <- sud_beta_day[["moderate_default"]]
sud_beta_day[["melanoma_low"]]  <- sud_beta_day[["moderate_minus_2sd"]]

sud_daily_hr_multiplier <- exp(sud_beta_day)

hr_vec <- c(
  pancreatic_high = delay_hr_n(sud_beta_day[["pancreatic_high"]], -pancreatic_days_speed_up_high),
  pancreatic_base = delay_hr_n(sud_beta_day[["pancreatic_base"]], -pancreatic_days_speed_up_base),
  pancreatic_low  = delay_hr_n(sud_beta_day[["pancreatic_low"]],  -pancreatic_days_speed_up_low),
  pancreatic_90_day = delay_hr_n(sud_beta_day[["pancreatic_base"]], 90),
  brain_high      = delay_hr_n(sud_beta_day[["brain_high"]],      -brain_days_speed_up_high),
  brain_base      = delay_hr_n(sud_beta_day[["brain_base"]],      -brain_days_speed_up_base),
  brain_low       = delay_hr_n(sud_beta_day[["brain_low"]],       -brain_days_speed_up_low),
  brain_90_day     = delay_hr_n(sud_beta_day[["brain_base"]],      90),
  melanoma_high   = delay_hr_n(sud_beta_day[["melanoma_high"]],   -melanoma_days_speed_up_high),
  melanoma_base   = delay_hr_n(sud_beta_day[["melanoma_base"]],   -melanoma_days_speed_up_base),
  melanoma_low    = delay_hr_n(sud_beta_day[["melanoma_low"]],    -melanoma_days_speed_up_low),
  melanoma_90_day   = delay_hr_n(sud_beta_day[["melanoma_base"]],   90),
  conserv_90_day = delay_hr_n(sud_beta_day[["high_conserv_hanna"]], 90)
)

utility_weights <- list(
  pancreatic = 0.6,
  brain = 0.7,
  melanoma = 0.75
)

#---------------------------
# Read and process survival data
#---------------------------

survival_data <- file.path(raw_path, "rel_cancer_survival.csv")

rel_survival <- read_csv(survival_data, show_col_types = FALSE) %>%
  mutate(survival = survival / 100) %>%
  mutate(survival = ifelse((survival > 0.995) & (year == 1), 0.995, survival)) %>%
  mutate(survival = ifelse((survival > 0.99)  & (year >= 2), 0.99, survival)) %>%
  mutate(survival = ifelse((survival > 0.985) & (year >= 5), 0.985, survival)) %>%
  mutate(survival = ifelse((survival > 0.98)  & (year >= 10), 0.98, survival)) %>%
  group_by(country, cancer_type, stage) %>%
  arrange(country, cancer_type, stage, year) %>%
  mutate(
    prev_year = lag(year),
    surv_prev = lag(survival),
    prev_year = replace_na(prev_year, 0),
    surv_prev = replace_na(surv_prev, 1),
    days_at_risk = (year - prev_year) * 365.25,
    interval_survival = survival / surv_prev,
    interval_mortality_risk = 1 - interval_survival,
    daily_hazard_rate = -log(interval_survival) / days_at_risk,
    daily_mortality_probability = 1 - exp(-daily_hazard_rate)
  ) %>%
  ungroup() %>%
  mutate(colour_key = paste(country, stage, sep = ": "))

for (cancer_site in unique(rel_survival$cancer_type)) {
  p <- ggplot(
    rel_survival %>% filter(cancer_type == cancer_site),
    aes(x = year, y = daily_mortality_probability, color = colour_key)
  ) +
    geom_point() +
    labs(
      title = paste("Daily Mortality Probability over Time for", cancer_site),
      x = "Years Since Diagnosis",
      y = "Daily Mortality Probability"
    ) +
    theme_minimal()
  print(p)
}

#---------------------------
# Pancreatic disease modelling
#---------------------------

rel_pancreatic <- rel_survival %>%
  filter(cancer_type == "pancreatic") %>%
  filter(as.character(stage) == "3") %>%
  filter(country %in% c("England", "Northern Ireland")) %>%
  select(country, cancer_type, stage, year, daily_hazard_rate, survival)

# Keep Northern Ireland values as a visual sense check only
rel_pancreatic_checkpoints <- rel_pancreatic %>%
  filter(year %in% c(1, 2, 5, 10)) %>%
  filter(!(country == "Northern Ireland" & year == 1)) %>%
  transmute(days_since_diagnosis = year * 365, observed_survival = survival)

# Remove NI and use 1->2 year mortality as proxy for 2->5 year
fill_missing_pancreatic <- function(rel_pancreatic) {
  rel_pancreatic <- rel_pancreatic %>%
    filter(country == "England")
  
  row_to_add <- rel_pancreatic %>%
    filter(year == 2) %>%
    mutate(year = 5)
  
  bind_rows(rel_pancreatic, row_to_add) %>%
    arrange(year) %>%
    select(-survival)
}

rel_pancreatic <- fill_missing_pancreatic(rel_pancreatic)

rel_pancreatic_daily <- explode_to_daily(rel_pancreatic)
rel_pancreatic_daily <- estimate_cumulative_mortality(rel_pancreatic_daily)

pancreatic_checkpoints <- rel_pancreatic_checkpoints

post_speed_up_pancreatic <- apply_hr_and_estimate_survival(rel_pancreatic_daily, hr_vec["pancreatic_base"])
post_speed_up_pancreatic_high <- apply_hr_and_estimate_survival(rel_pancreatic_daily, hr_vec["pancreatic_high"])
post_speed_up_pancreatic_low <- apply_hr_and_estimate_survival(rel_pancreatic_daily, hr_vec["pancreatic_low"])
post_speed_up_pancreatic_90_day <- apply_hr_and_estimate_survival(rel_pancreatic_daily, hr_vec["pancreatic_90_day"])
post_speed_up_pancreatic_conserv_90_day <- apply_hr_and_estimate_survival(rel_pancreatic_daily, hr_vec["conserv_90_day"])

qalys_pancreatic <- qaly_comparison(
  post_speed_up_pancreatic,
  rel_pancreatic_daily,
  utility_weights$pancreatic,
  health_discount_rate
)

qalys_pancreatic_high <- qaly_comparison(
  post_speed_up_pancreatic_high,
  rel_pancreatic_daily,
  utility_weights$pancreatic,
  health_discount_rate
)

qalys_pancreatic_low <- qaly_comparison(
  post_speed_up_pancreatic_low,
  rel_pancreatic_daily,
  utility_weights$pancreatic,
  health_discount_rate
)

modelling_results[[pancreatic_case_study_title]] <- list(
  cancer = "pancreatic",
  baseline = rel_pancreatic_daily,
  checkpoints = pancreatic_checkpoints,
  post_speed_up = post_speed_up_pancreatic,
  post_speed_up_high = post_speed_up_pancreatic_high,
  post_speed_up_low = post_speed_up_pancreatic_low,
  qalys = qalys_pancreatic,
  qalys_high = qalys_pancreatic_high,
  qalys_low = qalys_pancreatic_low
)

#---------------------------
# Melanoma disease modelling
#---------------------------

rel_melanoma <- rel_survival %>%
  filter(cancer_type == "melanoma") %>%
  filter(as.character(stage) == "all") %>%
  filter(country == "England") %>%
  select(country, cancer_type, stage, year, daily_hazard_rate, survival)

rel_melanoma_checkpoints <- rel_melanoma %>%
  filter(year %in% c(1, 2, 5, 10)) %>%
  transmute(days_since_diagnosis = year * 365, observed_survival = survival)

rel_melanoma <- rel_melanoma %>%
  select(-survival)

rel_melanoma_daily <- explode_to_daily(rel_melanoma)
rel_melanoma_daily <- estimate_cumulative_mortality(rel_melanoma_daily)

melanoma_checkpoints <- rel_melanoma_checkpoints

post_speed_up_melanoma <- apply_hr_and_estimate_survival(rel_melanoma_daily, hr_vec["melanoma_base"])
post_speed_up_melanoma_high <- apply_hr_and_estimate_survival(rel_melanoma_daily, hr_vec["melanoma_high"])
post_speed_up_melanoma_low <- apply_hr_and_estimate_survival(rel_melanoma_daily, hr_vec["melanoma_low"])
post_speed_up_melanoma_90_day <- apply_hr_and_estimate_survival(rel_melanoma_daily, hr_vec["melanoma_90_day"])
post_speed_up_melanoma_conserv_90_day <- apply_hr_and_estimate_survival(rel_melanoma_daily, hr_vec["conserv_90_day"])

qalys_melanoma <- qaly_comparison(
  post_speed_up_melanoma,
  rel_melanoma_daily,
  utility_weights$melanoma,
  health_discount_rate
)

qalys_melanoma_high <- qaly_comparison(
  post_speed_up_melanoma_high,
  rel_melanoma_daily,
  utility_weights$melanoma,
  health_discount_rate
)

qalys_melanoma_low <- qaly_comparison(
  post_speed_up_melanoma_low,
  rel_melanoma_daily,
  utility_weights$melanoma,
  health_discount_rate
)

modelling_results[[melanoma_case_study_title]] <- list(
  cancer = "melanoma",
  baseline = rel_melanoma_daily,
  checkpoints = melanoma_checkpoints,
  post_speed_up = post_speed_up_melanoma,
  post_speed_up_high = post_speed_up_melanoma_high,
  post_speed_up_low = post_speed_up_melanoma_low,
  qalys = qalys_melanoma,
  qalys_high = qalys_melanoma_high,
  qalys_low = qalys_melanoma_low
)

#---------------------------
# Brain cancer disease modelling
#---------------------------

rel_brain <- rel_survival %>%
  filter(cancer_type == "brain all") %>%
  filter(as.character(stage) == "all") %>%
  filter(country == "United Kingdom") %>%
  select(country, cancer_type, stage, year, daily_hazard_rate, survival)

rel_brain_checkpoints <- rel_brain %>%
  filter(year %in% c(1, 2, 5, 10)) %>%
  transmute(days_since_diagnosis = year * 365, observed_survival = survival)

rel_brain <- rel_brain %>%
  select(-survival)

rel_brain_daily <- explode_to_daily(rel_brain)
rel_brain_daily <- estimate_cumulative_mortality(rel_brain_daily)

brain_checkpoints <- rel_brain_checkpoints

post_speed_up_brain <- apply_hr_and_estimate_survival(rel_brain_daily, hr_vec["brain_base"])
post_speed_up_brain_high <- apply_hr_and_estimate_survival(rel_brain_daily, hr_vec["brain_high"])
post_speed_up_brain_low <- apply_hr_and_estimate_survival(rel_brain_daily, hr_vec["brain_low"])
post_speed_up_brain_90_day <- apply_hr_and_estimate_survival(rel_brain_daily, hr_vec["brain_90_day"])
post_speed_up_brain_conserv_90_day <- apply_hr_and_estimate_survival(rel_brain_daily, hr_vec["conserv_90_day"])

qalys_brain <- qaly_comparison(
  post_speed_up_brain,
  rel_brain_daily,
  utility_weights$brain,
  health_discount_rate
)

qalys_brain_high <- qaly_comparison(
  post_speed_up_brain_high,
  rel_brain_daily,
  utility_weights$brain,
  health_discount_rate
)

qalys_brain_low <- qaly_comparison(
  post_speed_up_brain_low,
  rel_brain_daily,
  utility_weights$brain,
  health_discount_rate
)

modelling_results[[brain_cancer_case_study_title]] <- list(
  cancer = "brain",
  baseline = rel_brain_daily,
  checkpoints = brain_checkpoints,
  post_speed_up = post_speed_up_brain,
  post_speed_up_high = post_speed_up_brain_high,
  post_speed_up_low = post_speed_up_brain_low,
  qalys = qalys_brain,
  qalys_high = qalys_brain_high,
  qalys_low = qalys_brain_low
)

qaly_table <- bind_rows(

  tibble::tibble(
    case_study = melanoma_case_study_title,
    qaly_gain = modelling_results[[melanoma_case_study_title]]$qalys$qaly_gains,
    qaly_gain_low = modelling_results[[melanoma_case_study_title]]$qalys_low$qaly_gains,
    qaly_gain_high = modelling_results[[melanoma_case_study_title]]$qalys_high$qaly_gains
  ),
  tibble::tibble(
    case_study = pancreatic_case_study_title,
    qaly_gain = modelling_results[[pancreatic_case_study_title]]$qalys$qaly_gains,
    qaly_gain_low = modelling_results[[pancreatic_case_study_title]]$qalys_low$qaly_gains,
    qaly_gain_high = modelling_results[[pancreatic_case_study_title]]$qalys_high$qaly_gains
  ),
  tibble::tibble(
    case_study = brain_cancer_case_study_title,
    qaly_gain = modelling_results[[brain_cancer_case_study_title]]$qalys$qaly_gains,
    qaly_gain_low = modelling_results[[brain_cancer_case_study_title]]$qalys_low$qaly_gains,
    qaly_gain_high = modelling_results[[brain_cancer_case_study_title]]$qalys_high$qaly_gains
  )
)
#---------------------------
# Charts
#---------------------------

pancreatic_chart_df <- format_results_to_survival_chart_df(modelling_results[[pancreatic_case_study_title]])
brain_chart_df <- format_results_to_survival_chart_df(modelling_results[[brain_cancer_case_study_title]])
melanoma_chart_df <- format_results_to_survival_chart_df(modelling_results[[melanoma_case_study_title]])


graph_survival(
  fig_path = fig_path,
  case_study_num = pancreatic_case_study_title,
  graph_df = pancreatic_chart_df$graph_df,
  color_palette = COLOR_CATEGORICAL,
  plot_title_str = pancreatic_chart_df$plot_title_str
)
graph_survival(
  fig_path = fig_path,
  case_study_num = brain_cancer_case_study_title,
  graph_df = brain_chart_df$graph_df,
  color_palette = COLOR_CATEGORICAL,
  plot_title_str = brain_chart_df$plot_title_str
)
graph_survival(
  fig_path = fig_path,
  case_study_num = melanoma_case_study_title,
  graph_df = melanoma_chart_df$graph_df,
  color_palette = COLOR_CATEGORICAL,
  plot_title_str = melanoma_chart_df$plot_title_str
)

change_in_final_surv_90_days_pancreatic <- min(rel_pancreatic_daily$cumulative_survival) - min(post_speed_up_pancreatic_90_day$cumulative_survival)
change_in_final_surv_hanna_90_days_pancreatic <- min(rel_pancreatic_daily$cumulative_survival) - min(post_speed_up_pancreatic_conserv_90_day$cumulative_survival)
change_in_final_surv_90_days_brain <- min(rel_brain_daily$cumulative_survival) - min(post_speed_up_brain_90_day$cumulative_survival)
change_in_final_surv_hanna_90_days_brain <- min(rel_brain_daily$cumulative_survival) - min(post_speed_up_brain_conserv_90_day$cumulative_survival)
change_in_final_surv_90_days_melanoma <- min(rel_melanoma_daily$cumulative_survival) - min(post_speed_up_melanoma_90_day$cumulative_survival)
change_in_final_surv_hanna_90_days_melanoma <- min(rel_melanoma_daily$cumulative_survival) - min(post_speed_up_melanoma_conserv_90_day$cumulative_survival)


pancreatic_cancer_mortality_changes <- c(
  "Sud 30-39" = 0.0392,
  "Sud 40-49" = 0.0314,
  "Sud 50-59" = 0.0340,
  "Sud 60-69" = 0.0339,
  "Sud 70-79" = 0.0336,
  "Sud 80+"   = 0.0635,
  "Hanna conservative" = change_in_final_surv_hanna_90_days_pancreatic,
  "Modelled" = change_in_final_surv_90_days_pancreatic
)

brain_cancer_mortality_changes <- c(
  "Sud 30-39" = 0.1175,
  "Sud 40-49" = 0.1415,
  "Sud 50-59" = 0.1782,
  "Sud 60-69" = 0.1824,
  "Sud 70-79" = 0.1664,
  "Sud 80+"   = 0.1670,
  "Hanna conservative" = change_in_final_surv_hanna_90_days_brain,
  "Modelled" = change_in_final_surv_90_days_brain
)

melanoma_cancer_mortality_changes <- c(
  "Sud 30-39" = 0.0313,
  "Sud 40-49" = 0.0396,
  "Sud 50-59" = 0.0489,
  "Sud 60-69" = 0.0566,
  "Sud 70-79" = 0.0732,
  "Sud 80+"   = 0.1256,
  "Hanna conservative" = change_in_final_surv_hanna_90_days_melanoma,
  "Modelled" = change_in_final_surv_90_days_melanoma
)  

comparison_to_sud_chart_df <- bind_rows(
  tibble::tibble(age_group = names(pancreatic_cancer_mortality_changes), mortality_change = pancreatic_cancer_mortality_changes, cancer_type = "Pancreatic"),
  tibble::tibble(age_group = names(brain_cancer_mortality_changes), mortality_change = brain_cancer_mortality_changes, cancer_type = "Brain"),
  tibble::tibble(age_group = names(melanoma_cancer_mortality_changes), mortality_change = melanoma_cancer_mortality_changes, cancer_type = "Melanoma")
)

colour_comparison <- c(COLOR_SEQUENTIAL[c(1,3,5,6,8,10)], COLOR_CATEGORICAL[5], COLOR_CATEGORICAL[6])

graph_comparison_to_sud(
  fig_path = fig_path,
  case_study_num = "comparison_to_sud",
  graph_df = comparison_to_sud_chart_df,
  color_palette = colour_comparison,
  plot_title_str = "Mortality Change from 90 Day Delay from our Model and Data Compared\nwith Sud et al Published Estimates",
  cancer_type_mapping = c("Pancreatic", "Brain", "Melanoma"),
  age_group_mapping = c(
    "Sud 30-39",
    "Sud 40-49",
    "Sud 50-59",
    "Sud 60-69",
    "Sud 70-79",
    "Sud 80+",
    "Hanna conservative",
    "Modelled"
  )
)

pvflp_df <- read_csv(file.path(proc_path, "pvflp_per_death_by_cancer_site_by_year.csv"), show_col_types = FALSE) %>%
  mutate(cause = str_to_lower(cause)) %>%
  mutate(cause = ifelse(cause == "pancreas", "pancreatic", cause)) %>%
  mutate(cause = ifelse(cause == "melanoma skin cancer", "melanoma", cause)) %>%
  mutate(cause = ifelse(cause == "brain, other cns and intracranial tumours", "brain all", cause)) %>%
  mutate(year = year - 1)
pvflp_df$cause %>% unique()
rel_brain_daily$cancer_type %>% unique()
estimate_productivity <- function(survival_df, pvflp_df) {
  
  pvflp_df <- pvflp_df %>%
    mutate(daily_prod_if_alive = weighted_total_gain_per_survivor / 365)
  
  survival_df <- survival_df %>%
    arrange(days_since_diagnosis) %>%
    left_join(pvflp_df, by = c("cancer_type" = "cause",
                               "year")) %>%
    mutate(
      years_since_diagnosis = days_since_diagnosis / 365,
      productivity_contribution = cumulative_survival * daily_prod_if_alive) %>%
    select(year, country, cancer_type, stage, days_since_diagnosis, cumulative_survival, daily_prod_if_alive, productivity_contribution)
  
  prod_point <- survival_df %>%
    summarise(productivity_contribution = sum(productivity_contribution), .groups = "drop") %>%
    pull(productivity_contribution)
  
  return_list <- list(
    productivity_df = survival_df,
    overall_productivity = prod_point
  )
  return(return_list)
  
}

compare_productivity <- function(intervention_survival_df, baseline_survival_df, pvflp_df) {
  intervention_prod <- estimate_productivity(intervention_survival_df, pvflp_df)
  baseline_prod <- estimate_productivity(baseline_survival_df, pvflp_df)
  
  productivity_gain = intervention_prod$overall_productivity - baseline_prod$overall_productivity
  
  return(list(
    intervention_prod = intervention_prod,
    baseline_prod = baseline_prod,
    productivity_gain = productivity_gain
  ))
}
modelling_results[[melanoma_case_study_title]]$productivity <- compare_productivity(
  post_speed_up_melanoma,
  rel_melanoma_daily,
  pvflp_df
)
modelling_results[[pancreatic_case_study_title]]$productivity <- compare_productivity(
  post_speed_up_pancreatic,
  rel_pancreatic_daily,
  pvflp_df
)
modelling_results[[brain_cancer_case_study_title]]$productivity <- compare_productivity(
  post_speed_up_brain,
  rel_brain_daily,
  pvflp_df
)

# Add the upper and the lower bounds for the productivity gain based on the high and low HR scenarios
modelling_results[[melanoma_case_study_title]]$productivity_high <- compare_productivity(
  post_speed_up_melanoma_high,
  rel_melanoma_daily,
  pvflp_df
)
modelling_results[[melanoma_case_study_title]]$productivity_low <- compare_productivity(
  post_speed_up_melanoma_low,
  rel_melanoma_daily,
  pvflp_df
)

modelling_results[[pancreatic_case_study_title]]$productivity_high <- compare_productivity(
  post_speed_up_pancreatic_high,
  rel_pancreatic_daily,
  pvflp_df
)
modelling_results[[pancreatic_case_study_title]]$productivity_low <- compare_productivity(
  post_speed_up_pancreatic_low,
  rel_pancreatic_daily,
  pvflp_df
)
modelling_results[[brain_cancer_case_study_title]]$productivity_high <- compare_productivity(
  post_speed_up_brain_high,
  rel_brain_daily,
  pvflp_df
)
modelling_results[[brain_cancer_case_study_title]]$productivity_low <- compare_productivity(
  post_speed_up_brain_low,
  rel_brain_daily,
  pvflp_df
)

productivity_gain_table <- bind_rows(
  tibble::tibble(
    case_study = melanoma_case_study_title,
    productivity_gain = modelling_results[[melanoma_case_study_title]]$productivity$productivity_gain,
    productivity_gain_low = modelling_results[[melanoma_case_study_title]]$productivity_low$productivity_gain,
    productivity_gain_high = modelling_results[[melanoma_case_study_title]]$productivity_high$productivity_gain
  ),
  tibble::tibble(
    case_study = pancreatic_case_study_title,
    productivity_gain = modelling_results[[pancreatic_case_study_title]]$productivity$productivity_gain,
    productivity_gain_low = modelling_results[[pancreatic_case_study_title]]$productivity_low$productivity_gain,
    productivity_gain_high = modelling_results[[pancreatic_case_study_title]]$productivity_high$productivity_gain
  ),
  tibble::tibble(
    case_study = brain_cancer_case_study_title,
    productivity_gain = modelling_results[[brain_cancer_case_study_title]]$productivity$productivity_gain,
    productivity_gain_low = modelling_results[[brain_cancer_case_study_title]]$productivity_low$productivity_gain,
    productivity_gain_high = modelling_results[[brain_cancer_case_study_title]]$productivity_high$productivity_gain
  )
)

write_csv(qaly_table, file.path(proc_path, "qaly_gains_by_case_study.csv"))
write_csv(productivity_gain_table, file.path(proc_path, "productivity_gains_by_case_study.csv"))