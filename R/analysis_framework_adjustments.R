library(dplyr)
library(tidyr)

#' Estimate Benefitting Population
#'
#' This function calculates the benefitting population for each year based on initial population, 
#' a sub-population adjustment factor, and a coverage data frame. The calculation includes intermediate 
#' steps such as estimating the target population and then applying the coverage to determine the benefitting population.
#'
#' @param init_pop Numeric value representing the initial population size.
#' @param sub_pop_adjust Numeric value representing a factor to adjust the initial population to estimate the target population.
#' @param coverage_df A data frame containing at least two columns:
#'   - `year`: The year for each observation.
#'   - `coverage`: The coverage proportion for the corresponding year (e.g., 0.1 for 10% coverage).
#'
#' @return A data frame identical to `coverage_df` but with additional columns:
#'   - `init_pop`: The initial population value repeated for all rows.
#'   - `sub_pop_adjust`: The sub-population adjustment factor repeated for all rows.
#'   - `target_pop`: The product of `init_pop` and `sub_pop_adjust`.
#'   - `benefitting_pop`: The product of `coverage` and `target_pop`.
#'
#' @details
#' The function performs the following calculations for each row in the `coverage_df`:
#' \itemize{
#'   \item \code{target_pop = init_pop * sub_pop_adjust}
#'   \item \code{benefitting_pop = coverage * target_pop}
#' }
#' These calculated values are appended as new columns to the input data frame.
#'
#' @examples
#' # Example data
#' coverage_df <- data.frame(
#'   year = c(2015, 2016),
#'   coverage = c(0.1, 0.2)
#' )
#'
#' # Estimating benefitting population
#' estimate_benefitting_pop(
#'   init_pop = 1000,
#'   sub_pop_adjust = 0.5,
#'   coverage_df = coverage_df
#' )
#'
#' @export
estimate_benefitting_pop <- function(init_pop, sub_pop_adjust, coverage_df) {
  benefitting_pop_df <- coverage_df %>%
    mutate(init_pop = init_pop) %>%
    mutate(sub_pop_adjust = sub_pop_adjust) %>%
    mutate(target_pop = init_pop * sub_pop_adjust) %>%
    mutate(benefitting_pop = coverage * target_pop) 
  return(benefitting_pop_df)
}

#' Add Discounted Benefit Column to Data Frame
#'
#' This function calculates a discounted benefit value based on a specified discount rate 
#' and target cost year, then adds this calculated benefit as a new column in the provided data frame.
#'
#' @param benefitting_pop_df A data frame containing population and benefit data. It must include 
#'   at least the following columns: `year`, `benefitting_pop`.
#' @param benefit_value Numeric value representing the base value of the benefit.
#' @param benefit_name A string specifying the name of the new column to be added for the discounted benefit.
#' @param discount_rate Numeric value representing the annual discount rate (e.g., 0.05 for 5%).
#' @param target_cost_year An integer representing the reference year for discounting. 
#'   All calculations will discount the benefit based on the difference between `year` and this target.
#'
#' @return A data frame identical to `benefitting_pop_df` but with an additional column named as specified
#'   by `benefit_name`. This column contains the discounted benefit values.
#'
#' @details
#' The discounted benefit is calculated using the formula:
#' \deqn{discounted\_benefit = \frac{benefit\_value}{(1 + discount\_rate)^{(year - target\_cost\_year)}}}
#' The result is then multiplied by the `benefitting_pop` column in the input data frame.
#'
#' @examples
#' # Example data
#' benefitting_pop_df <- data.frame(
#'   year = c(2015, 2016),
#'   benefitting_pop = c(1, 1)
#' )
#'
#' # Adding discounted benefit column
#' add_discounted_benefit_col_df(
#'   benefitting_pop_df,
#'   benefit_value = 10,
#'   benefit_name = "discounted_benefit",
#'   discount_rate = 0.05,
#'   target_cost_year = 2015
#' )
#'
#' @importFrom dplyr mutate select
#' @importFrom rlang sym
#'
#' @export
add_discounted_benefit_col_df <- function(benefitting_pop_df, benefit_value, benefit_name, discount_rate, target_cost_year) {
  discounted_benefit_df <- benefitting_pop_df %>%
    mutate(discounted_benefit = benefit_value / ((1 + discount_rate) ^ (year - target_cost_year))) %>%
    mutate(!!sym(benefit_name) := discounted_benefit * benefitting_pop) %>%
    select(-discounted_benefit)
  
  return(discounted_benefit_df)
}

#' Add Optimism Bias Adjustment Column
#'
#' This function calculates an optimism bias adjustment based on specified benefit columns 
#' (those containing "gains" or "savings" in their names) and a provided optimism bias factor. 
#' It adds the calculated adjustment as a new column to the input data frame.
#'
#' @param granular_benefits_df A data frame containing detailed benefits data, including columns with names 
#'   that contain "gains" or "savings" (e.g., "financial_gains", "energy_savings").
#' @param optimism_bias A numeric value between 0 and 1 representing the optimism bias adjustment factor. 
#'   Values closer to 1 imply lower adjustment, while values closer to 0 imply higher adjustment.
#'
#' @return A data frame identical to `granular_benefits_df` but with an additional column:
#'   - `optimism_bias_adjustment`: The calculated adjustment, which is a negative value derived as:
#'     \deqn{- total\_benefit \times (1 - optimism\_bias)}
#'     where `total_benefit` is the row-wise sum of all columns containing "gains" or "savings".
#'
#' @examples
#' # Example data
#' granular_benefits_df <- data.frame(
#'   year = c(2015, 2016),
#'   productivity_gains = c(100, 200),
#'   healthcare_cost_savings = c(50, 75)
#' )
#'
#' # Adding optimism bias adjustment
#' add_optimism_bias_column(
#'   granular_benefits_df = granular_benefits_df,
#'   optimism_bias = 0.8
#' )
#'
#' @export
add_optimism_bias_column <- function(granular_benefits_df, optimism_bias) {
  benefit_cols <- str_subset(names(granular_benefits_df), "gains|savings")
  total_benefit <- granular_benefits_df %>% 
    select(all_of(benefit_cols)) %>%
    rowSums()
  # Remove names from total_benefit
  names(total_benefit) <- NULL
  
      
  granular_benefits_df <- granular_benefits_df %>%
    mutate(optimism_bias_adjustment = - total_benefit * (1-optimism_bias))
  return(granular_benefits_df)
}

#' Create Granular Benefits Data Frame
#'
#' This function calculates detailed, discounted benefits for each row in a population data frame, 
#' adjusting for health and cost discount rates, monetary value of QALY (Quality-Adjusted Life Years), 
#' and optimism bias. It adds specific benefit columns and an optimism bias adjustment column to the output.
#'
#' @param row_val A named list or vector containing the row-level benefit values and an `optimism_bias_benefits_value`.
#'   Required keys include:
#'   - Benefit values: Keys ending in `_value` (e.g., `gains_value`, `savings_value`) with numeric values.
#'   - `optimism_bias_benefits_value`: A numeric value between 0 and 1 representing the optimism bias factor.
#' @param benefitting_pop_df A data frame containing the population data for the corresponding benefit calculations.
#'   It must include a `year` column.
#' @param health_discount_rate Numeric value representing the discount rate applied to health-related benefits.
#' @param cost_discount_rate Numeric value representing the discount rate applied to cost-related benefits.
#' @param monetary_qaly Numeric value representing the monetary value of one QALY (e.g., a conversion rate).
#'
#' @return A data frame identical to `benefitting_pop_df` but with additional columns:
#'   - Discounted benefit columns: Named after benefit keys in `row_val` with `_value` removed (e.g., `gains`, `savings`).
#'   - `optimism_bias_adjustment`: A column adjusting the total benefits by the optimism bias factor.
#'
#'
#' @examples
#' # Example data
#' row_val <- list(
#'   gains_value = 1000,
#'   savings_value = 500,
#'   optimism_bias_benefits_value = 0.8
#' )
#' benefitting_pop_df <- data.frame(
#'   year = c(2015, 2016),
#'   benefitting_pop = c(100, 200)
#' )
#'
#' # Creating granular benefits data frame
#' create_granular_benefits_df(
#'   row_val = row_val,
#'   benefitting_pop_df = benefitting_pop_df,
#'   health_discount_rate = 0.03,
#'   cost_discount_rate = 0.05,
#'   monetary_qaly = 50000
#' )
#'
#' @export
create_granular_benefits_df <- function(row_val, benefitting_pop_df, health_discount_rate, cost_discount_rate, monetary_qaly, target_cost_year) {
  granular_benefits_df <- benefitting_pop_df
  benefit_cols <- str_subset(names(row_val), "gains_value|savings_value")
  
  stopifnot("optimism_bias_benefits_value" %in% names(row_val))
  
  optimism_bias <- row_val[["optimism_bias_benefits_value"]]
  
  for (benefit in benefit_cols) {
    benefit_value <- row_val[[benefit]]
    benefit_name <- str_replace(benefit, "_value", "")
    discount_rate_to_use <- ifelse(grepl("qaly", benefit), health_discount_rate, cost_discount_rate)
    benefit_value <- ifelse(grepl("qaly", benefit), benefit_value * monetary_qaly, benefit_value)
    granular_benefits_df <- add_discounted_benefit_col_df(granular_benefits_df, benefit_value, benefit_name, discount_rate_to_use, target_cost_year)
  }
  granular_benefits_df <- granular_benefits_df%>%
    mutate(year = as.numeric(year))
  
  granular_benefits_df <- add_optimism_bias_column(granular_benefits_df, optimism_bias)

  return(granular_benefits_df)
}

#' Calculate Benefits for All Rows
#'
#' This function calculates detailed, discounted benefits for each row in a population data frame,
#' adjusting for health and cost discount rates, monetary value of QALY (Quality-Adjusted Life Years),
#' and optimism bias. It adds specific benefit columns and an optimism bias adjustment column to the output.
#'
#' @param parameter_scenarios A data frame containing the parameter values for coverage, population, and benefits. Required columns are:
#'   - `initial_population_value`: The initial population value.
#'   - `adjustment_for_sub_population_value`: The adjustment for sub-population value.
#'   - `coverage_init_value` and `coverage_end_value`: The value of coverage at the initial and end years.
#'   - `coverage_init_year` and `coverage_end_year`: The initial and end coverage years.
#'   - `...gains_value`: For each gains benefit the gains value.
#'   - `...savings_value`: For each savings benefit the savings value.
#'   - `optimism_bias_benefits_value`: The optimism bias factor this is a value between 0 and 1, where nearer 1 is less bias
#' @param health_discount_rate Numeric value representing the discount rate applied to health-related benefits it should be 0.035 for 3.5%.
#' @param cost_discount_rate Numeric value representing the discount rate applied to cost-related benefits it should be 0.015 for 1.5%.
#' @param monetary_qaly Numeric value representing the monetary value of one QALY (e.g., a conversion rate) e.g. 70k for GreenBook or 20-30k for NICE
#' @param target_cost_year The year to which costs are to be discounted.
#' @param years_of_coverage The number of years of coverage.
benefits_for_all_rows <- function(parameter_scenarios, health_discount_rate, cost_discount_rate, monetary_qaly, target_cost_year, years_of_coverage) {
  granular_benefits_list <- list()
  for (i in 1:nrow(parameter_scenarios)) {
    row_val <- parameter_scenarios[i,]
    coverage_df <- coverage_adjust_row(row_val, years_of_coverage)
    benefitting_pop_df <- estimate_benefitting_pop(init_pop = row_val[["initial_population_value"]],
                                                   sub_pop_adjust = row_val[["adjustment_for_sub_population_value"]],
                                                   coverage_df = coverage_df)
    granular_benefits_df <- create_granular_benefits_df(row_val, 
                                                        benefitting_pop_df, 
                                                        health_discount_rate, 
                                                        cost_discount_rate, 
                                                        monetary_qaly,
                                                        target_cost_year)
    granular_benefits_df$case_study <- row_val[["case_study_number"]]
    granular_benefits_df$scenario <- row_val[["scenario"]]
    granular_benefits_list[[i]] <- granular_benefits_df
  }
  all_granular_benefits_df <- bind_rows(granular_benefits_list)
  return(all_granular_benefits_df)
}

#' @description This function aggregates over years to calculate total benefits for each scenario.
#' @param granular_benefits_df A data frame containing annual granular benefits data.
#' @param research_costs A data frame containing research costs for each scenario.
#' @return A data frame containing total benefits and costs for each scenario
aggregate_to_total_benefits <- function(granular_benefits_df, research_costs) {
  rel_cols <- c("case_study", 
                "scenario", 
                "init_pop", 
                "target_pop", 
                "benefitting_pop", 
                "qaly_gains", 
                "healthcare_cost_savings", 
                "socialcare_cost_savings", 
                "productivity_gains", 
                "optimism_bias_adjustment")
  
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
  
  return(total_benefits_df)    
}



return_on_investment <- function(total_benefit, research_cost) {
  roi <- total_benefit / research_cost
  return(roi)
}

#' @description This function calculates the overall ROI and uncertainty of funding (across case studies) 
#' for a given confidence level.
#' @param total_benefits_df A data frame containing total benefits and costs for each scenario.
#' @param confidence The confidence level for the uncertainty interval.
#' @return A list containing the reference ROI, lower ROI, and upper ROI.
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
  
  list_rois <- list(reference_roi = reference_roi, lower_roi = uncertainty_interval$lower, upper_roi = uncertainty_interval$upper)
  
  return(list_rois)
}