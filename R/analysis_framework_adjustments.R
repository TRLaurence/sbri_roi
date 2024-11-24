estimate_benefitting_pop <- function(init_pop, sub_pop_adjust, coverage_df) {
  benefitting_pop_df <- coverage_df %>%
    mutate(init_pop = init_pop) %>%
    mutate(sub_pop_adjust = sub_pop_adjust) %>%
    mutate(target_pop = init_pop * sub_pop_adjust) %>%
    mutate(benefitting_pop = coverage * target_pop) 
  return(benefitting_pop_df)
}

add_discounted_benefit_col_df <- function(benefitting_pop_df, benefit_value, benefit_name, discount_rate, target_cost_year) {
  discounted_benefit_df <- benefitting_pop_df %>%
    mutate(discounted_benefit = benefit_value / ((1 + discount_rate) ^ (year - target_cost_year))) %>%
    mutate(!!sym(benefit_name) := discounted_benefit * benefitting_pop) %>%
    select(-discounted_benefit)
  
  return(discounted_benefit_df)
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
  granular_benefits_df <- benefitting_pop_df
  benefit_cols <- str_subset(names(row_val), "gains_value|savings_value")
  
  stopifnot("optimism_bias_benefits_value" %in% names(row_val))
  
  optimism_bias <- row_val[["optimism_bias_benefits_value"]]
  
  for (benefit in benefit_cols) {
    benefit_value <- row_val[[benefit]]
    benefit_name <- str_replace(benefit, "_value", "")
    discount_rate_to_use <- ifelse(grepl("qaly", benefit), health_discount_rate, cost_discount_rate)
    benefit_value <- ifelse(grepl("qaly", benefit), benefit_value * monetary_qaly, benefit_value)
    granular_benefits_df <- add_discounted_benefit_col_df(granular_benefits_df, benefit_value, benefit_name, cost_discount_rate, target_cost_year)
  }
  granular_benefits_df <- granular_benefits_df%>%
    mutate(year = as.numeric(year))
  
  granular_benefits_df <- add_optimism_bias_column(granular_benefits_df, optimism_bias)

  return(granular_benefits_df)
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
  granular_benefits_list <- list()
  for (i in 1:nrow(parameter_scenarios)) {
    row_val <- parameter_scenarios[i,]
    coverage_df <- coverage_adjust_row(row_val, years_of_coverage)
    benefitting_pop_df <- estimate_benefitting_pop(init_pop = row_val[["initial_population_value"]],
                                                   sub_pop_adjust = row_val[["adjustment_for_sub_population_value"]],
                                                   coverage_df)
    granular_benefits_df <- create_all_benefit_dfs(row_val, benefitting_pop_df, health_discount_rate, cost_discount_rate, monetary_qaly)
    granular_benefits_df$case_study <- row_val[["case_study_number"]]
    granular_benefits_df$scenario <- row_val[["scenario"]]
    granular_benefits_list[[i]] <- granular_benefits_df
  }
  all_granular_benefits_df <- bind_rows(granular_benefits_list)
  return(all_granular_benefits_df)
}


return_on_investment <- function(total_benefit, research_cost) {
  roi <- total_benefit / research_cost
  return(roi)
}