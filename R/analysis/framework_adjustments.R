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

create_benefit_df <- function(benefitting_pop_df, benefit_value, benefit_name, discount_rate, target_cost_year) {
  benefitting_pop_df <- estimate_discounted_benefit_df(benefitting_pop_df, benefit_value, cost_discount_rate, target_cost_year) 
  benefitting_pop_df$benefit_name <- benefit_name
  return(benefitting_pop_df)
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
  for (benefit in benefit_cols) {
    benefit_value <- row_val[[benefit]]
    benefit_name <- benefit
    discount_rate_to_use <- ifelse(grepl("qaly", benefit), health_discount_rate, cost_discount_rate)
    benefit_value <- ifelse(grepl("qaly", benefit), benefit_value * monetary_qaly, benefit_value)
    granular_benefits_df_list[[benefit]] <- create_benefit_df(benefitting_pop_df, benefit_value, benefit_name, cost_discount_rate, target_cost_year)
  }
  granular_benefits_df <- bind_rows(granular_benefits_df_list)

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