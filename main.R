source("R/data_loaders/parameters.R")
source("R/data_loaders/inflation.R")
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

case_studies_to_rerun <- c(99999, 99998, 99997, 99996, 99995)

# Import functional parameters
source("R/utils/paths.R")

# Import general parameters
source("R/utils/parameter_vals.R")

# Import case study specific parameters (filled with initial values)
parameter_vals <- parameter_loader(raw_path, "dummy_parameters.csv")

parameter_scenarios <- set_up_all_sensitivities(parameter_vals,
                                                deterministic_sensitivity,
                                                probabilistic_sensitivity,
                                                number_of_samples)

# TODO coverage and discounting
inflation_df <- inflation_data_loader()

cost_cols <- str_subset(colnames(parameter_scenarios), "cost|productivity")
value_cols  <- str_subset(cost_cols, "_value")
value_year_mapping  <- str_subset(cost_cols, "_year")
names(value_year_mapping) <- value_cols 


parameter_scenarios <- standardise_df_to_target(parameter_scenarios, 
                                                inflation_df, 
                                                value_year_mapping, 
                                                target_cost_year, 
                                                discount_rate = cost_discount_rate)

all_outputs <- benefits_for_all_rows(parameter_scenarios, health_discount_rate, cost_discount_rate, monetary_qaly, target_cost_year, years_of_coverage)

total_benefits_df <- all_outputs[["total_benefits_df"]]
mean_benefits_df <- all_outputs[["mean_benefits_df"]]
granular_benefits_df <- all_outputs[["granular_benefits_df"]]

total_benefits_df <- total_benefits_df %>%
  mutate(total_benefits = qaly_gains + healthcare_cost_savings + socialcare_cost_savings + productivity_gains) %>%
  mutate(roi = return_on_investment(total_benefits, research_costs))


write_csv(total_benefits_df, file.path(proc_path, "total_benefits_df.csv"))
write_csv(mean_benefits_df, file.path(proc_path, "mean_benefits_df.csv"))
write_csv(granular_benefits_df, file.path(proc_path, "granular_benefits_df.csv"))


overall_graphs(total_benefits_df)



lapply(case_studies_to_rerun , function(x) case_study_specific_graphs(total_benefits_df, granular_benefits_df, x))












