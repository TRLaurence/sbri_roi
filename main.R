source("R/data_loader_parameters.R")
source("R/data_loader_inflation.R")
source("R/data_loader_funding.R")
source("R/utils_temporal.R")
source("R/analysis_coverage.R")
source("R/analysis_framework_adjustments.R")
source("R/wrangle_sensitivity_parameters.R")
source("R/graphs_all.R")

set.seed(1)
library(testthat)
testthat::test_dir("tests/testthat")
library(dplyr)
library(tidyr)

deterministic_sensitivity <- TRUE
probabilistic_sensitivity <- TRUE
rerun_modelling <- FALSE

# Import functional parameters
source("R/utils_paths.R")

# Import general parameters
source("R/utils_parameter_vals.R")



# Import case study specific parameters (filled with initial values)
param_file <-  "parameters_20241103.csv"

# Load the inflation data, cannot read from file if it has never been run
inflation_df <- inflation_data_loader(load_from_file = TRUE, proc_path = proc_path, file_name = "inflation_data.csv")

parameter_vals <- parameter_loader(raw_path, param_file)

name_vals <- graph_names_loader(raw_path, param_file)

# Load the funding data and fill missing values
funding <- data_loader_funding("funding_data.csv", baseline_split_salaries, baseline_fec_markup)

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

parameter_vals <- left_join(parameter_vals, funding, by = "case_study_number")

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

if (rerun_modelling) {
  granular_benefits_df <- benefits_for_all_rows(intervention_param_scenarios, health_discount_rate, cost_discount_rate, monetary_qaly, target_cost_year, years_of_coverage)
  total_benefits_df <- aggregate_to_total_benefits(granular_benefits_df, research_costs)
  
  write_csv(total_benefits_df, file.path(proc_path, "total_benefits_df.csv"))
  write_csv(granular_benefits_df, file.path(proc_path, "granular_benefits_df.csv"))
} else {
  total_benefits_df <- read_csv(file.path(proc_path, "total_benefits_df.csv"))
  granular_benefits_df <- read_csv(file.path(proc_path, "granular_benefits_df.csv"))
}

# Recreate the overall graphs for the case studies in comparison
overall_graphs(total_benefits_df, case_study_mapping)

# Recreate the case study specific graphs
case_studies_to_rerun <- total_benefits_df$case_study %>% unique() 

reference_research_costs_only <- total_benefits_df %>%
  filter(scenario == "reference") %>%
  select(case_study, research_costs)

lapply(case_studies_to_rerun , function(x) case_study_specific_graphs(total_benefits_df, 
                                                                      reference_research_costs_only, 
                                                                      granular_benefits_df, 
                                                                      name_vals,
                                                                      x))


# Estimate the overall ROI
roi_ci <- format_to_overall_roi(total_benefits_df, confidence = 0.90)

# Write the case study ROIs to a file
write_inidividual_case_study_rois(total_benefits_df, case_study_mapping, table_path, confidence = 0.9)

# Write the overall aggregated benefits table to a file
write_aggregated_benefits_table(total_benefits_df, table_path)

# Write the table Karl requested to a file
write_karl_table(total_benefits_df, table_path)




