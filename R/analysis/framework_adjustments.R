estimate_benefitting_pop <- function(init_pop, sub_pop_adjust, coverage_df) {
  benefitting_pop_df <- coverage_df %>%
    mutate(init_pop = init_pop) %>%
    mutate(sub_pop_adjust = sub_pop_adjust) %>%
    mutate(target_pop = init_pop * sub_pop_adjust) %>%
    mutate(benefitting_pop = coverage * target_pop) 
  return(benefitting_pop_df)
}

estimate_discounted_benefit_df <- function(benefitting_pop_df, benefit_value, discount_rate, target_cost_year) {
  discounted_benefit_df <- benefitting_pop_df %>%
    mutate(discounted_benefit = benefit_value /( (1 + discount_rate) ^ (year-target_cost_year))) %>%
    mutate(total_discounted_benefit = discounted_benefit * benefitting_pop) %>%
    select(- discounted_benefit)
  return(discounted_benefit_df)
}

estimate_total_benefit <- function(total_benefit_df) {
  total_benefit <- sum(total_benefit_df$total_discounted_benefit)
  return(total_benefit)
}

estimate_mean_benefit <- function(total_benefit_df) {
  mean_benefit <- mean(total_benefit_df$total_discounted_benefit)
  return(mean_benefit)
}

create_benefit_df <- function(benefitting_pop_df, benefit_value, benefit_name, discount_rate, target_cost_year) {
  benefitting_pop_df <- estimate_discounted_benefit_df(benefitting_pop_df, benefit_value, cost_discount_rate, target_cost_year) 
  benefitting_pop_df$benefit_name <- benefit_name
  # aggegrated_outputs <- c(estimate_total_benefit(benefitting_pop_df),estimate_mean_benefit(benefitting_pop_df))
  # aggegrated_df <- as.data.frame(t(aggegrated_outputs))
  # colnames(aggegrated_df) <- c("total", "mean")
  # aggegrated_df$benefit_name <- benefit_name
  # return_list <- list("aggregated_df" = aggegrated_df, 
  #                     "benefitting_pop_df" = benefitting_pop_df)
  # return(return_list)
  return(benefitting_pop_df)
}

#'@title apply_optimism_bias
#'@description Apply an optimism bias to the benefits
#'@param aggregate_benefits_df A dataframe containing the aggregated benefits
#'@param optimism_bias The optimism bias to apply
#'
#'@examples
#'      total     mean       benefit_name
#'   1  100        10        qaly_gains_value
#'   2 -50         -5        healthcare_cost_savings_value
#'   3  0           0        socialcare_cost_savings_value
#'   4  150       150        productivity_gains_value  
apply_optimism_bias_row <- function(aggregate_benefits_df, optimism_bias) {
    total_optimism_bias_adjustment <- -sum(aggregate_benefits_df$total) * (1-optimism_bias)
    mean_optimism_bias_adjustment <- -sum(aggregate_benefits_df$mean) * (1-optimism_bias)
    new_row_df <- as.data.frame(list(
      total = total_optimism_bias_adjustment,
      mean = mean_optimism_bias_adjustment,
      benefit_name = "optimism_bias_adjustment"
    ))
    aggregate_benefits_df <- rbind(aggregate_benefits_df, new_row_df)
    return(aggregate_benefits_df)
}

add_optimism_bias_column <- function(granular_benefits_df, optimism_bias) {
  benefit_cols <- str_subset(names(granular_benefits_df), "gains|savings")
  total_benefit <- granular_benefits_df %>% 
    select(all_of(benefit_cols)) %>%
    rowSums()
      
  granular_benefits_df <- granular_benefits_df %>%
    mutate(optimism_bias_adjustment = - total_benefit * (1-optimism_bias))
  return(granular_benefits_df)
}

create_all_benefit_dfs <- function(row_val, benefitting_pop_df, health_discount_rate, cost_discount_rate, monetary_qaly) {
  benefit_cols <- str_subset(names(row_val), "gains_value|savings_value")
  
  stopifnot("optimism_bias_benefits_value" %in% names(row_val))
  
  optimism_bias <- row_val[["optimism_bias_benefits_value"]]
  
  granular_benefits_df_list <- list()
  # aggregate_benefits_df_list <- list()
  for (benefit in benefit_cols) {
    benefit_value <- row_val[[benefit]]
    benefit_name <- benefit
    discount_rate_to_use <- ifelse(grepl("qaly", benefit), health_discount_rate, cost_discount_rate)
    benefit_value <- ifelse(grepl("qaly", benefit), benefit_value * monetary_qaly, benefit_value)
    # benefit_list <- create_benefit_df(benefitting_pop_df, benefit_value, benefit_name, cost_discount_rate, target_cost_year)
    # granular_benefits_df_list[[benefit]] <- benefit_list[["benefitting_pop_df"]]
    granular_benefits_df_list[[benefit]] <- create_benefit_df(benefitting_pop_df, benefit_value, benefit_name, cost_discount_rate, target_cost_year)
    # aggregate_benefits_df_list[[benefit]] <- benefit_list[["aggregated_df"]]
  }
  granular_benefits_df <- bind_rows(granular_benefits_df_list)
  # aggregate_benefits_df <- bind_rows(aggregate_benefits_df_list)
  
  
  cols_to_not_pivot <- names(granular_benefits_df)[!str_detect(names(granular_benefits_df), "benefit_name|total_discounted_benefit")]
  granular_benefits_df <- granular_benefits_df %>%
    mutate(benefit_name = str_remove_all(benefit_name, "_value")) %>%
    pivot_wider(id_cols = all_of(cols_to_not_pivot), names_from = "benefit_name", values_from = "total_discounted_benefit") %>%
    mutate(year = as.numeric(year))
  
  granular_benefits_df <- add_optimism_bias_column(granular_benefits_df, optimism_bias)
  
  rel_cols <- str_subset(names(granular_benefits_df), "_pop$|_savings$|_gains$|^optimism_bias")
  total_benefits_df <- granular_benefits_df %>%
    select(all_of(rel_cols)) %>%
    summarise_all(sum)
  
  mean_benefits_df <- granular_benefits_df %>%
    select(all_of(rel_cols)) %>%
    summarise_all(mean)

  return_list <- list("total_benefits_df" = total_benefits_df,
                      "mean_benefits_df" = mean_benefits_df,
                      "granular_benefits_df" = granular_benefits_df)
  return(return_list)
}

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
    benefitting_pop_df <- estimate_benefitting_pop(init_pop = row_val[["initial_population_value"]],
                                                   sub_pop_adjust = row_val[["adjustment_for_sub_population_value"]],
                                                   coverage_df)
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


return_on_investment <- function(total_benefit, research_cost) {
  roi <- total_benefit / research_cost
  return(roi)
}