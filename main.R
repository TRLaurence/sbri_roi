source("R/data_loaders/parameters.R")
source("R/data_loaders/inflation.R")
source("R/utils/temporal.R")
source("R/analysis/coverage.R")
source("R/analysis/framework_adjustments.R")
source("R/sensitivity/wrangle_sensitivity_parameters.R")

library(dplyr)
library(tidyr)

# renv::install("dplyr")
# renv::install("tidyr")

deterministic_sensitivity <- TRUE
probabilistic_sensitivity <- TRUE

case_studies_to_rerun <- c(9999, 9998, 9997, 9996, 9995)

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
row_val <- parameter_scenarios[1,]

standardise_df_to_target <- function(parameter_scenarios, inflation_df, value_year_mapping, target_cost_year, discount_rate) {
  inflation_adjust_row <- function(row_val, inflation_df, value_year_mapping, target_cost_year) {
    for (i in 1:length(value_year_mapping)) {
      col_name <- names(value_year_mapping)[i]
      val <- row_val[[col_name]]
      observed_year <- row_val[[value_year_mapping[i]]]
      row_val[[col_name]] <- apply_inflation(val, inflation_df, cost_year = observed_year, target_year = target_cost_year)
    }
    return(row_val)
  }
  
  for (i in 1:nrow(parameter_scenarios)) {
    parameter_scenarios[i,] <- inflation_adjust_row(parameter_scenarios[i,], inflation_df, value_year_mapping, target_cost_year)
  }
  
  parameter_scenarios <- apply_discount(parameter_scenarios, value_year_mapping, target_cost_year, discount_rate)
  
  return(parameter_scenarios)
}

parameter_scenarios <- standardise_df_to_target(parameter_scenarios, 
                                                inflation_df, 
                                                value_year_mapping, 
                                                target_cost_year, 
                                                discount_rate = cost_discount_rate)

parameter_scenarios$target_pop <- estimate_target_population(parameter_scenarios$initial_population_value, parameter_scenarios$adjustment_for_sub_population_value)

row_val <- parameter_scenarios[1,]
coverage_adjust_row <- function(row_val, years_of_coverage) {
  init_coverage <- row_val[["coverage_init_value"]]
  final_coverage <- row_val[["coverage_end_value"]]
  year_start_coverage <- row_val[["coverage_init_year"]]
  year_final_coverage <- row_val[["coverage_end_year"]]
  coverage_df <- estimate_coverage(init_coverage, final_coverage, year_start_coverage, year_final_coverage, n_years = years_of_coverage)
  
  return(coverage_df)
}


benefitting_pop_df <- estimate_benefitting_pop(row_val[["target_pop"]], coverage_df)

create_benefit_df <- function(benefitting_pop_df, benefit_value, benefit_name, discount_rate, target_cost_year) {
  benefitting_pop_df <- estimate_discounted_benefit_df(benefitting_pop_df, benefit_value, cost_discount_rate, target_cost_year) 
  benefitting_pop_df$benefit_name <- benefit_name
  
  aggegrated_outputs <- c(estimate_total_benefit(benefitting_pop_df),estimate_mean_benefit(benefitting_pop_df))
  aggegrated_df <- as.data.frame(t(aggegrated_outputs))
  colnames(aggegrated_df) <- c("total", "mean")
  aggegrated_df$benefit_name <- benefit_name
  return_list <- list("aggregated_df" = aggegrated_df, 
                      "benefitting_pop_df" = benefitting_pop_df)
  return(return_list)
}


create_all_benefit_dfs <- function(row_val, benefitting_pop_df, health_discount_rate, cost_discount_rate, monetary_qaly) {
  benefit_cols <- str_subset(names(row_val), "gains_value|savings_value")
  
  granular_benefits_df_list <- list()
  aggregate_benefits_df_list <- list()
  for (benefit in benefit_cols) {
    benefit_value <- row_val[[benefit]]
    benefit_name <- benefit
    discount_rate_to_use <- ifelse(grepl("qaly", benefit), health_discount_rate, cost_discount_rate)
    benefit_value <- ifelse(grepl("qaly", benefit), benefit_value * monetary_qaly, benefit_value)
    benefit_list <- create_benefit_df(benefitting_pop_df, benefit_value, benefit_name, cost_discount_rate, target_cost_year)
    granular_benefits_df_list[[benefit]] <- benefit_list[["benefitting_pop_df"]]
    aggregate_benefits_df_list[[benefit]] <- benefit_list[["aggregated_df"]]
  }
  granular_benefits_df <- bind_rows(granular_benefits_df_list)
  aggregate_benefits_df <- bind_rows(aggregate_benefits_df_list)
  
  total_benefits_df <- select(aggregate_benefits_df, total, benefit_name) %>%
    mutate(benefit_name = str_remove_all(benefit_name, "_value")) %>%
    pivot_wider(names_from = benefit_name, values_from = total) 
  
  mean_benefits_df <- select(aggregate_benefits_df, mean, benefit_name) %>%
    mutate(benefit_name = str_remove_all(benefit_name, "_value")) %>%
    pivot_wider(names_from = benefit_name, values_from = mean)
  
  granular_benefits_df <- granular_benefits_df %>%
    mutate(benefit_name = str_remove_all(benefit_name, "_value")) %>%
    pivot_wider(id_cols = c(year, coverage, benefitting_pop), names_from = "benefit_name", values_from = "total_discounted_benefit") %>%
    mutate(year = as.numeric(year))
  
  return_list <- list("total_benefits_df" = total_benefits_df, 
                      "mean_benefits_df" = mean_benefits_df, 
                      "granular_benefits_df" = granular_benefits_df)  
  return(return_list)
}

names(row_val)

add_rel_values <- function(df, df_name, case_study, scenario, research_costs) {
  df$case_study <- case_study
  df$scenario <- scenario
  if (df_name == "total_benefits_df") {
    df$research_costs <- research_costs
  }
  return(df)
}

benefits_for_all_rows <- function(parameter_scenarios, health_discount_rate, cost_discount_rate, monetary_qaly, target_cost_year, years_of_coverage) {
  total_benefits_list <- list()
  mean_benefits_list <- list()
  granular_benefits_list <- list()
  for (i in 1:nrow(parameter_scenarios)) {
    row_val <- parameter_scenarios[i,]
    coverage_df <- coverage_adjust_row(row_val, years_of_coverage)
    benefitting_pop_df <- estimate_benefitting_pop(row_val[["target_pop"]], coverage_df)
    benefit_list <- create_all_benefit_dfs(row_val, benefitting_pop_df, health_discount_rate, cost_discount_rate, monetary_qaly)
    case_study <- row_val[["case_study_number"]]
    scenario <- row_val[["scenario"]]
    research_costs <- row_val[["research_costs_value"]]
    benefit_list <- mapply(function(x, y) add_rel_values(x, y, case_study, scenario, research_costs), 
                           benefit_list, 
                           names(benefit_list), SIMPLIFY = FALSE)
    total_benefits_list[[i]] <- benefit_list[["total_benefits_df"]]
    mean_benefits_list[[i]] <- benefit_list[["mean_benefits_df"]]
    granular_benefits_list[[i]] <- benefit_list[["granular_benefits_df"]]
    
  }
  total_benefits_df <- bind_rows(total_benefits_list)
  mean_benefits_df <- bind_rows(mean_benefits_list)
  granular_benefits_df <- bind_rows(granular_benefits_list)
  return_list <- list("total_benefits_df" = total_benefits_df, 
                      "mean_benefits_df" = mean_benefits_df, 
                      "granular_benefits_df" = granular_benefits_df)
  return(return_list)
}

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

format_total_to_prob <- function(total_benefits_df, case_study_num) {
  probabilistic_df <- total_benefits_df %>%
    filter(case_study == case_study_num) %>%
    filter(str_detect(scenario, "prob")) %>%
    select(case_study, scenario, roi)
  return(probabilistic_df)
}

format_total_to_determ <- function(total_benefits_df, case_study_num) {
  determ_df <- total_benefits_df %>%
    filter(case_study == case_study_num) %>%
    filter(str_detect(scenario, "reference|lower|upper")) %>%
    select(case_study, scenario, roi)
  
  base <- determ_df %>%
    filter(scenario == "reference") %>%
    select(roi) %>%
    pull()
  
  determ_df <- determ_df %>%
    filter(scenario != "reference") %>%
    mutate(variable = str_remove(scenario, "_lower|_upper")) %>%
    mutate(upper_or_lower = ifelse(str_detect(scenario, "lower"), "lower", "upper")) %>%
    select(-scenario) %>%
    pivot_wider(names_from = upper_or_lower, values_from = roi) %>%
    mutate(base = base) %>%
    mutate(range = upper - lower)  
  
  # Sort the graph data so the widest range is at the top and reindex
  determ_df <- determ_df %>%
    arrange(desc(range)) %>%
    mutate(index = rev(row_number()))
  return(determ_df)
}

probabilistic_df <- format_total_to_prob(total_benefits_df, case_study_num = 99999) 

format_to_uncertainty <- function(total_benefits_df, confidence = 0.95) {
  uncertainty <- 1-confidence
  uncertainty_df <- total_benefits_df %>%
    group_by(case_study) %>%
    filter(str_detect(scenario, "probabilistic")) %>%
    select(case_study, roi) %>%
    summarise(lower = quantile(roi, probs = uncertainty/2), 
              upper = quantile(roi, probs = 1 - uncertainty/2))
  return(uncertainty_df)
}

format_to_uncertainty(total_benefits_df, confidence = 0.95)
graph_probability_histogram(fig_path, case_study_num ="comparison", probabilistic_df)
# roi <- return_on_investment(total_benefit, row_val[["research_costs_value"]])
# value to year mapping

format_total_to_comparison_prob <- function(total_benefits_df) {
  probabilistic_df <- total_benefits_df %>%
    filter(str_detect(scenario, "prob")) %>%
    select(case_study, scenario, roi)
  return(probabilistic_df)
}

comparison_probability_df <- format_total_to_comparison_prob(total_benefits_df)

graph_boxplot_probability_comparison(fig_path, case_study_num = "comparison", comparison_probability_df)

format_granular_to_coverage_df <- function(granular_benefits_df, case_study_num) {
  coverage_df <- granular_benefits_df %>%
    filter(str_detect(scenario, "prob|reference")) %>%
    mutate(reference = ifelse(str_detect(scenario, "reference"), "reference", "probabilistic"))%>%
    filter(case_study == case_study_num) %>%
    select(year, scenario, reference, coverage) %>%
  return(coverage_df)
}

coverage_df <- format_granular_to_coverage_df(granular_benefits_df, case_study_num = 99999)

graph_uptake_projection(fig_path, case_study_num = "comparison", coverage_df)

format_uptake_to_confidence_interval <- function(coverage_df, confidence = 0.95) {
  uncertainty <- 1-confidence
  reference_vals <- coverage_df %>%
    filter(reference == "reference") %>%
    select(year, coverage) %>%
    rename(reference_coverage = coverage)
  
  uptake_ci_df <- coverage_df %>%
    filter(reference == "probabilistic") %>%
    select(year, scenario, coverage) %>%
    group_by(year) %>%
    summarise(lower = quantile(coverage, probs = uncertainty/2), 
              upper = quantile(coverage, probs = (1-uncertainty/2))) 
  uptake_ci_df <- left_join(uncertainty_df, reference_vals, by = "year")
  return(uptake_ci_df)
}

uptake_ci_df <- format_uptake_to_confidence_interval(coverage_df, confidence = 0.95)

graph_uptake_confidence_interval(fig_path, case_study_num = "comparison", uptake_ci_df)




graph_data <- data.frame(
  scenario = c("Variable 1", "Variable 2", "Variable 3", "Variable 4", "Variable 5"),
  lower = c(80, 65, 80, 70, 50),  # Lower bound values
  upper = c(120, 100, 110, 100, 160),  # Upper bound values
  base = c(90, 90, 90, 90, 90)  # Base values
) %>%
  mutate(ranges = upper - lower)  # Calculate the range of each variable




determ_df <- format_total_to_determ(total_benefits_df, case_study_num = 99999)
tornado_ggplot(fig_path,  case_study_num = 99999, determ_df)

  