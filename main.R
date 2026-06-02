source("R/data_loader_parameters.R")
source("R/data_loader_inflation.R")
source("R/data_loader_funding.R")
source("R/utils_temporal.R")
source("R/analysis_coverage.R")
source("R/analysis_framework_adjustments.R")
source("R/wrangle_sensitivity_parameters.R")
source("R/tables_all.R")
source("R/graphs_all.R")
source("R/utils.R")

set.seed(1)
library(testthat)
testthat::test_dir("tests/testthat")
library(dplyr)
library(tidyr)


deterministic_sensitivity <- TRUE
probabilistic_sensitivity <- TRUE
rerun_modelling <- TRUE

# Import functional parameters
source("R/utils_paths.R")

# Import general parameters
source("R/utils_parameter_vals.R")



# Import case study specific parameters (filled with initial values)
param_file <-  "parameters_20260528.csv"

# Load the inflation data, cannot read from file if it has never been run
inflation_df <- inflation_data_loader(load_from_file = TRUE, proc_path = proc_path, file_name = "inflation_data.csv")

parameter_vals <- parameter_loader(raw_path, param_file)

# Fix ORION optimism bias to make it lower because the evaluation has a lower standard of evidence
parameter_vals <- parameter_vals %>%
  mutate(optimism_bias_benefits_value = ifelse(case_study_number == "SBRIC01P3041", 0.5, optimism_bias_benefits_value),
         optimism_bias_benefits_lower = ifelse(case_study_number == "SBRIC01P3041", 0.3, optimism_bias_benefits_lower),
         optimism_bias_benefits_upper = ifelse(case_study_number == "SBRIC01P3041", 0.7, optimism_bias_benefits_upper))

name_vals <- graph_names_loader(raw_path, param_file)

# Spell check the name values with hunspell
allowed_words <- c(
  "DCIS",
  "Fraumenei",
  "biliary",
  "tumours",
  "reflux",
  "invasiveness",
  "eDERMA",
  "DERM",
  "USC",
  "MRI",
  "CNS",
  "ORION"
)

cells_with_bad_spelling <- check_name_vals_spelling(name_vals, allowed_words)


# Load the funding data and fill missing values
funding <- data_loader_funding("funding_data_20260528.csv", baseline_split_salaries, baseline_fec_markup)


# Perform all the adjustments to the funding data
funding <- all_funding_adjustments(funding, 
                                    employer_ni, 
                                    inflation_df, 
                                    cost_discount_rate, 
                                    target_cost_year, 
                                    under_ascertainment_bias, 
                                    under_ascertaintment_lower, 
                                    under_ascertaintment_upper,
                                    applied_adjustment,
                                    applied_adjustment_lower,
                                    applied_adjustment_upper)

parameter_vals <- full_join(parameter_vals, funding, by = "case_study_number")

case_studies_to_remove <- parameter_vals %>%
  filter(is.na(initial_population_value)) %>%
  pull(case_study_number)
  
case_studies_to_remove <- c(case_studies_to_remove, "SBRIC01P3031")

#Fill NAs for unassessed awards with defaults
names(parameter_vals)
replace_missing_defaults <- function(df) {
  defaults_vector <- c(
    "initial_population" = 1, # Setting by 1 to avoid divide by 0
    "adjustment_for_sub_population" = 0.5, # Setting to 0.5 to avoid 0-1 beta issues
    "coverage_init" = 0.5, # Setting to 0.5 to avoid 0-1 beta issues
    "coverage_end" = 0.5, # Setting to 0.5 to avoid 0-1 beta issues
    "qaly_gains" = 1, # Setting to 1
    "healthcare_cost_savings" = 1, # Setting to 1
    "productivity_gains" = 1, # Setting to 1
    "socialcare_cost_savings" = 1, # Setting to 1
    # "research_funding" =  Commenting this out because funding is non-missing
    "optimism_bias_benefits" = 0.85
    # "applied_adjustment" = Commenting this out because applied adjustment is non-missing1
  )
  
  defaults_lookup <- c(
    setNames(defaults_vector,     paste0(names(defaults_vector), "_value")),
    setNames(defaults_vector / 2, paste0(names(defaults_vector), "_lower")),
    setNames(defaults_vector * 1.5, paste0(names(defaults_vector), "_upper"))
  )
  
  df <- df %>%
    mutate(
      across(
        all_of(names(defaults_lookup)),
        ~ replace_na(.x, unname(defaults_lookup[cur_column()]))
      ),
      coverage_init_year = replace_na(coverage_init_year, 2024),
      coverage_end_year  = replace_na(coverage_end_year, 2033),
      across(ends_with("s_year"), ~ replace_na(.x, 2023)),
      across(where(is.character), ~ replace_na(.x, "dummy"))
    )
  
  # Print a warning if any NAs remain
  if (any(is.na(df))) {
    warning("There are still missing values in the dataframe after filling defaults.")
  }
  return(df)
  
}

parameter_vals <- replace_missing_defaults(parameter_vals)

names(parameter_vals)

parameter_scenarios <- set_up_all_sensitivities(parameter_vals,
                                                deterministic_sensitivity,
                                                probabilistic_sensitivity,
                                                number_of_samples)


research_costs <- parameter_scenarios %>%
  select(case_study = case_study_number, 
         scenario, 
         research_costs = research_funding_value,
         applied_adjustment = applied_adjustment_value) 

value_year_mapping <- create_value_year_mapping(parameter_scenarios)

intervention_param_scenarios <- parameter_scenarios %>%
  select(-all_of(c("research_funding_value", "applied_adjustment_value" )))

intervention_param_scenarios <- standardise_df_to_target(intervention_param_scenarios, 
                                                inflation_df, 
                                                value_year_mapping, 
                                                target_cost_year, 
                                                discount_rate = cost_discount_rate)

remove_dummy_benefits <- function(benefits_df, case_studies_to_remove) {
  benefits_df <- benefits_df %>%
    mutate(replace_benefits = case_study  %in% case_studies_to_remove)
  columns_to_replace <- c("init_pop", "target_pop", "benefitting_pop", "qaly_gains", "healthcare_cost_savings",  "socialcare_cost_savings",  "productivity_gains",       "optimism_bias_adjustment" ,"total_benefits","roi")
  columns_to_replace <- columns_to_replace[columns_to_replace %in% colnames(benefits_df)]
  benefits_df <- benefits_df %>%
    rowwise() %>%
    mutate(across(all_of(columns_to_replace), ~ ifelse(replace_benefits, NA, .x))) %>%
    ungroup() %>%
    select(-replace_benefits)
  return(benefits_df)
}

if (rerun_modelling) {
  granular_benefits_df <- benefits_for_all_rows(intervention_param_scenarios, health_discount_rate, cost_discount_rate, monetary_qaly, target_cost_year, years_of_coverage)
  
  total_benefits_df <- aggregate_to_total_benefits(granular_benefits_df, research_costs)
  granular_benefits_df <-remove_dummy_benefits(granular_benefits_df, case_studies_to_remove)
  total_benefits_df <- remove_dummy_benefits(total_benefits_df, case_studies_to_remove)
  
  write_csv(total_benefits_df, file.path(proc_path, "total_benefits_df.csv"))
  write_csv(granular_benefits_df, file.path(proc_path, "granular_benefits_df.csv"))
} else {
  total_benefits_df <- read_csv(file.path(proc_path, "total_benefits_df.csv"))
  granular_benefits_df <- read_csv(file.path(proc_path, "granular_benefits_df.csv"))
}

na_case_studies <- total_benefits_df %>%
  filter(is.na(roi)) %>%
  pull(case_study) %>%
  unique() %>%
  as.character()

filtered_total_benefits_df <- total_benefits_df %>%
  filter(!(case_study %in% na_case_studies))

# Estimate the overall ROI
measured_roi_ci <- format_to_overall_roi(filtered_total_benefits_df, confidence = 0.90)

upper_missing_roi <- mean(c(measured_roi_ci$reference_roi, roi_ci$lower_roi))
mean_missing_roi <- max(measured_roi_ci$lower_roi ,1)
lower_missing_roi <- min(measured_roi_ci$lower_roi , 0.5)

rbounded_beta <- function(n, lower, mean, upper, kappa = 10) {
  if (lower >= upper) stop("lower must be less than upper")
  if (mean <= lower || mean >= upper) stop("mean must be between lower and upper")
  
  mu <- (mean - lower) / (upper - lower)
  alpha <- mu * kappa
  beta <- (1 - mu) * kappa
  
  lower + (upper - lower) * rbeta(n, alpha, beta)
}

samples <- rbounded_beta(
  n = 1000,
  lower = lower_missing_roi,
  mean = mean_missing_roi,
  upper = upper_missing_roi,
  kappa = 10
)

check_tol <- function(samples, lower, mean, upper, tol = 0.2) {
  sample_mean <- mean(samples)
  sample_lower <- quantile(samples, 0.05)
  sample_upper <- quantile(samples, 0.95)
  
  mean_check <- abs(sample_mean - mean)/abs(mean) < tol
  lower_check <- (lower < sample_lower) 
  upper_check <- (sample_upper < upper)
  
  if (!mean_check) {
    cat(sprintf("Mean check failed: sample mean = %.4f, expected mean = %.4f\n", sample_mean, mean))
  } else {
    cat(sprintf("Mean check passed: sample mean = %.4f, expected mean = %.4f\n", sample_mean, mean))
  }
  if (!lower_check) {
    cat(sprintf("Lower check failed: sample lower = %.4f, expected lower = %.4f\n", sample_lower, lower))
  } else {
    cat(sprintf("Lower check passed: sample lower = %.4f, expected lower = %.4f\n", sample_lower, lower))
  }
  if (!upper_check) {
    cat(sprintf("Upper check failed: sample upper = %.4f, expected upper = %.4f\n", sample_upper, upper))
  } else {
    cat(sprintf("Upper check passed: sample upper = %.4f, expected upper = %.4f\n", sample_upper, upper))
  }
}

check_tol(samples, lower_missing_roi, mean_missing_roi, upper_missing_roi)

filled_total_benefits_df <- total_benefits_df %>%
  rowwise() %>%
  mutate(roi = ifelse(is.na(roi) & str_detect(scenario, "prob"), samples[sample(1:length(samples), 1)], roi)) %>%
  mutate(roi = ifelse(is.na(roi), mean_missing_roi, roi)) %>%
  ungroup() %>%
  mutate(total_benefits  =  ifelse(is.na(total_benefits), research_costs * roi, total_benefits))

total_benefits_df <- filled_total_benefits_df

roi_ci <- format_to_overall_roi(total_benefits_df, confidence = 0.90)

# Recreate the overall graphs for the case studies in comparison
summary_ci_df <- overall_graphs(total_benefits_df, case_study_mapping, add_overall = TRUE)

# Recreate the case study specific graphs
case_studies_to_rerun <- total_benefits_df %>%
  filter(!is.na(init_pop)) %>%
  pull(case_study) %>%
  unique()

reference_research_costs_only <- total_benefits_df %>%
  filter(scenario == "reference") %>%
  select(case_study, research_costs)

lapply(case_studies_to_rerun , function(x) case_study_specific_graphs(total_benefits_df, 
                                                                      reference_research_costs_only, 
                                                                      granular_benefits_df, 
                                                                      name_vals,
                                                                      x))



# Write the case study ROIs to a file
write_inidividual_case_study_rois(total_benefits_df, case_study_mapping, table_path, confidence = 0.9)

# Write the overall aggregated benefits table to a file
write_aggregated_benefits_table(total_benefits_df, table_path)

# Write the table Karl requested to a file
write_karl_table(total_benefits_df, table_path)




