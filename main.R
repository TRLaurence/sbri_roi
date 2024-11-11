source("R/data_loaders/parameters.R")
source("R/data_loaders/inflation.R")
source("R/data_loaders/funding.R")
source("R/utils/temporal.R")
source("R/analysis/coverage.R")
source("R/analysis/framework_adjustments.R")
source("R/sensitivity/wrangle_sensitivity_parameters.R")
source("R/vis/graphs_all.R")

library(dplyr)
library(tidyr)

# renv::install("dplyr")
# renv::install("tidyr")

deterministic_sensitivity <- TRUE
probabilistic_sensitivity <- TRUE

# Import functional parameters
source("R/utils/paths.R")

# Import general parameters
source("R/utils/parameter_vals.R")

set.seed(1)

# Import case study specific parameters (filled with initial values)
param_file <-  "parameters_20241103.csv"

case_study_mapping <- c("12" = "Vocational Advice MSK", 
                        "28" = "Breast Cancer Fractions", 
                        "56" = "Hospital at Home", 
                        "103" = "Compression Gloves", 
                        "118" = "REACH-HF")

# TODO coverage and discounting
inflation_df <- inflation_data_loader()

parameter_vals <- parameter_loader(raw_path, param_file)

name_vals <- graph_names_loader(raw_path, param_file)

funding <- data_loader_funding("funding_data.csv", baseline_split_salaries, baseline_fec_markup)

funding_to_report <- funding %>%
  group_by(case_study_number) %>%
  summarise(total_funding = sum(annualised_spend)) %>%
  ungroup() %>%
  mutate(funding_lower = total_funding * under_ascertaintment_upper) %>%
  mutate(funding_upper = total_funding * under_ascertaintment_lower) %>%
  mutate(funding_value = total_funding * under_ascertainment_bias) 

funding <- adjust_for_fec_and_ni(funding, employer_ni)

funding <- funding %>%
  rowwise() %>%
  mutate(adjusted_annual_spend = apply_inflation(adjusted_annual_spend, inflation_df, spend_year, target_cost_year)) %>%
  mutate(adjusted_annual_spend = apply_discount_basic(adjusted_annual_spend, cost_discount_rate, spend_year, target_cost_year))

funding <- funding %>%
  group_by(case_study_number) %>%
  summarise(research_costs_value = sum(adjusted_annual_spend)) %>%
  ungroup()

funding <- funding %>%
  mutate(research_funding_lower = research_costs_value * under_ascertaintment_lower) %>%
  mutate(research_funding_upper = research_costs_value * under_ascertaintment_upper) %>%
  mutate(research_funding_value = research_costs_value * under_ascertainment_bias) %>%
  mutate(applied_adjustment_value = applied_adjustment) %>%
  mutate(applied_adjustment_lower = applied_adjustment_lower) %>%
  mutate(applied_adjustment_upper = applied_adjustment_upper) %>%
  select(-research_costs_value )

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

cost_cols <- str_subset(colnames(parameter_scenarios), "cost|productivity")
value_cols  <- str_subset(cost_cols, "_value")
value_year_mapping  <- str_subset(cost_cols, "_year")
names(value_year_mapping) <- value_cols 

intervention_param_scenarios <- parameter_scenarios %>%
  select(-all_of(c("research_funding_value", "applied_adjustment_value" )))

intervention_param_scenarios <- standardise_df_to_target(intervention_param_scenarios, 
                                                inflation_df, 
                                                value_year_mapping, 
                                                target_cost_year, 
                                                discount_rate = cost_discount_rate)

granular_benefits_df <- benefits_for_all_rows(intervention_param_scenarios, health_discount_rate, cost_discount_rate, monetary_qaly, target_cost_year, years_of_coverage)


rel_cols <- str_subset(names(granular_benefits_df), "_pop$|_savings$|_gains$|^optimism_bias")
rel_cols <- c("case_study", "scenario", rel_cols)
total_benefits_df <- granular_benefits_df %>%
  select(all_of(rel_cols)) %>%
  group_by(case_study, scenario) %>%
  summarise_all(sum) %>%
  ungroup()



total_benefits_df <- left_join(total_benefits_df, research_costs, by = c("case_study", "scenario"))

total_benefits_df <- total_benefits_df %>%
  mutate(total_benefits = qaly_gains + healthcare_cost_savings + socialcare_cost_savings + productivity_gains + optimism_bias_adjustment) %>%
  mutate(research_costs = research_costs * applied_adjustment) %>%
  mutate(roi = return_on_investment(total_benefits, research_costs))


negative_rois <- filter(total_benefits_df, roi < 0 )

negative_rois <- left_join(negative_rois, 
                           parameter_scenarios, 
                           by = c("case_study" = "case_study_number", "scenario")) 



write_csv(total_benefits_df, file.path(proc_path, "total_benefits_df.csv"))
write_csv(granular_benefits_df, file.path(proc_path, "granular_benefits_df.csv"))


total_benefits_df <- read_csv(file.path(proc_path, "total_benefits_df.csv"))
granular_benefits_df <- read_csv(file.path(proc_path, "granular_benefits_df.csv"))





overall_graphs(total_benefits_df, case_study_mapping)



case_studies_to_rerun <- total_benefits_df$case_study %>% unique() 

reference_research_costs_only <- total_benefits_df %>%
  filter(scenario == "reference") %>%
  select(case_study, research_costs)

lapply(case_studies_to_rerun , function(x) case_study_specific_graphs(total_benefits_df, 
                                                                      reference_research_costs_only, 
                                                                      granular_benefits_df, 
                                                                      x))


table_to_use <-  format_to_ci(total_benefits_df, case_study_mapping, confidence = 0.90)

format_to_overall_roi <- function(total_benefits_df, confidence = 0.90) {
  uncertainty <- 1-confidence
  
  summary_ci_df <- total_benefits_df %>%
    filter(str_detect(scenario, "reference|probabilistic")) %>%
    group_by(scenario) %>%
    summarise(weighted_roi = sum(total_benefits)/sum(research_costs)) %>%
    ungroup()
  
  reference_roi <- summary_ci_df %>%
    filter(scenario == "reference") %>%
    select(weighted_roi) %>%
    pull()
  
  uncertainty_interval <- summary_ci_df %>%
    filter(scenario != "reference") %>%
    summarise(lower = quantile(weighted_roi, uncertainty/2), 
              upper = quantile(weighted_roi, 1 - uncertainty/2)) 
  
  list_rois <- list(reference_roi, lower_roi = uncertainty_interval$lower, upper_roi = uncertainty_interval$upper)
  
  return(list_rois)
}

roi_ci <- format_to_overall_roi(total_benefits_df, confidence = 0.90)

write_csv(table_to_use, file.path(table_path, "summary_rois.csv"))


aggregated_benefits <- total_benefits_df %>%
  filter(scenario == "reference")

total_agg_benefits <- aggregated_benefits %>%
  summarise_if(is.numeric, sum) %>%
  mutate(case_study = "Total") %>%
  mutate(scenario = "reference")

aggregated_benefits <- rbind(aggregated_benefits, total_agg_benefits)

# Format as millions
aggregated_benefits <- aggregated_benefits %>%
  mutate_at(vars(-case_study, -scenario), funs(. / 1e6))

write_csv(aggregated_benefits, file.path(table_path, "aggregated_benefits.csv"))
