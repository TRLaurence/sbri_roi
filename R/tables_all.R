

write_inidividual_case_study_rois <- function(total_benefits_df, case_study_mapping, table_path, confidence = 0.9) {
  table_to_use <-  format_to_ci(total_benefits_df, case_study_mapping, confidence)
  
  write_csv(table_to_use, file.path(table_path, "summary_rois.csv"))
  
}

write_aggregated_benefits_table <- function(total_benefits_df, table_path) {
  aggregated_benefits <- total_benefits_df %>%
    filter(scenario == "reference")
  
  total_agg_benefits <- aggregated_benefits %>%
    summarise_if(is.numeric, sum) %>%
    mutate(case_study = "Total") %>%
    mutate(scenario = "reference")
  
  aggregated_benefits <- rbind(aggregated_benefits, total_agg_benefits)
  
  # Format as millions
  aggregated_benefits <- aggregated_benefits %>%
    mutate(across(
      -c(case_study, scenario, roi),
      ~ .x / 1e6
    ))
  
  write_csv(aggregated_benefits, file.path(table_path, "aggregated_benefits.csv"))
  
}

write_karl_table <- function(total_benefits_df, table_path) {
  karl_table <- total_benefits_df %>%
    filter(scenario == "reference") %>%
    select(case_study, research_costs, applied_adjustment, benefitting_pop, total_benefits, roi) %>%
    mutate(core_research_costs = research_costs * (1/applied_adjustment)) %>%
    mutate(related_research_costs = research_costs - core_research_costs) %>%
    select(case_study, core_research_costs, related_research_costs, benefitting_pop, total_benefits, roi)
  
  write_csv(karl_table, file.path(table_path, "karl_table.csv"))
  
}
