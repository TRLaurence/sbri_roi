estimate_target_population <- function(init_pop, sub_pop_adjust) {
  target_pop <- init_pop * sub_pop_adjust
  return(target_pop)
}

estimate_benefitting_pop <- function(target_pop, coverage_df) {
  benefitting_pop_df <- coverage_df %>%
    mutate(benefitting_pop_df = coverage * target_pop) 
  return(benefitting_pop_df)
}

#' Does this need to be benefit column by benefit columns?
join_discounted_benefit <- function(benefitting_pop_df, discounted_benefit_df) {
  total_benefit_df <- discounted_benefit_df %>%
    left_join(benefitting_pop_df, by = "year") %>%
    mutate(total_discounted_benefit = discounted_benefit * benefitting_pop)
  return(total_benefit_df)
}

estimate_total_benefit <- function(total_benefit_df) {
  total_benefit <- sum(total_benefit_df$total_discounted_benefit)
  return(total_benefit)
}

return_on_investment <- function(total_benefit, research_cost) {
  roi <- total_benefit / research_cost
  return(roi)
}