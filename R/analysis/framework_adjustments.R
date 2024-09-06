estimate_target_population <- function(init_pop, sub_pop_adjust) {
  target_pop <- init_pop * sub_pop_adjust
  return(target_pop)
}

estimate_benefitting_pop <- function(target_pop, coverage_df) {
  benefitting_pop_df <- coverage_df %>%
    mutate(benefitting_pop = coverage * target_pop) 
  return(benefitting_pop_df)
}

estimate_discounted_benefit_df <- function(benefitting_pop_df, benefit_value, discount_rate, target_cost_year) {
  discounted_benefit_df <- benefitting_pop_df %>%
    mutate(discounted_benefit = benefit_value /( (1 + discount_rate) ^ (year-target_cost_year))) %>%
    mutate(total_discounted_benefit = discounted_benefit * benefitting_pop)
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

return_on_investment <- function(total_benefit, research_cost) {
  roi <- total_benefit / research_cost
  return(roi)
}