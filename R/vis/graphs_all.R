source("R/vis/graph_utils.R")
library(ggplot2)
library(tibble)
library(waterfalls)

# graph_template <- function(fig_path, case_study_num, chart_df) {
#   chart_name <- "default"
#   
#   p <- ggplot(chart_df, aes(x = x, y = y)) +
#     geom_point() +
#     geom_line() +
#     ggtitle("Default Chart") +
#     xlab("X Axis") +
#     ylab("Y Axis")
#   
#   p <- add_theme_and_save(p, fig_path, case_study_num, chart_name)
#   
#   return(p)
# }
##### FUNCTIONS TO DRAW GRAPHS ######
graph_probability_histogram <- function(fig_path, case_study_num, probabilistic_df) {
  chart_name <- "prob_histogram"
  p <- ggplot(probabilistic_df, aes(x = roi)) +
    geom_histogram(aes(y = ..count../sum(..count..)), bins = 20, fill = "lightblue", color = "black") +
    # At vertical line for reference
    geom_vline(aes(xintercept = reference), color = "red", linetype = "dashed") +
    labs(title = "Probability Histogram of ROIs", x = "ROI", y = "Probability")
  
  p <- add_theme_and_save(p, fig_path, case_study_num, chart_name)
  return(p)
}

graph_comparison_roi_ci <- function(fig_path, case_study_num = "comparison", summary_ci_df) {
  chart_name <- "roi_summary_ci"
  # Example plot
  p <- ggplot(summary_ci_df, aes(x = factor(case_study), y = reference)) +
    geom_point(size = 3) +  # Points for ROI
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +  # Error bars
    labs(
      title = "ROI by Case Study with Confidence Intervals",
      x = "Case Study",
      y = "ROI"
    )  
  
  p <- add_theme_and_save(p, fig_path, case_study_num, chart_name)
  
  return(p)
}

graph_boxplot_probability_comparison <- function(fig_path, case_study_num = "comparison", comparison_probability_df) {
  chart_name <- "comparison_boxplot"
  p <- ggplot(comparison_probability_df, aes(x = case_study, y = roi, group = case_study)) +
    geom_boxplot() +
    coord_flip() +
    labs(title = "Comparison of ROIs", x = "Case Study", y = "ROI")
  
  p <- add_theme_and_save(p, fig_path, case_study_num, chart_name)
  return(p)
}

graph_uptake_projection <- function(fig_path, case_study_num, uptake_df) {
  chart_name <- "uptake_projection"
  p <- ggplot(uptake_df, aes(x = year, y = coverage, group = scenario)) +
    geom_line(alpha = 0.1) +
    labs(title = "Uptake Projection", x = "Year", y = "Uptake")
  
  p <- add_theme_and_save(p, fig_path, case_study_num, chart_name)
  return(p)
}

graph_uptake_confidence_interval <- function(fig_path, case_study_num, uptake_ci_df) {
  chart_name <- "uptake_confidence_interval"
  p <- ggplot(uptake_ci_df, aes(x = year, y = reference_coverage)) +
    # Add a line just for reference_case
    geom_line() +
    # Add a swathe for the confidence interval
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    labs(title = "Uptake Confidence Interval", x = "Year", y = "Uptake")
  
  p <- add_theme_and_save(p, fig_path, case_study_num, chart_name)
  return(p)
}


graph_tornado_determ <- function(fig_path, case_study_num, graph_data) {
  chart_name <- "tornado"

  base_value <- graph_data$base[1]  # Assuming a single base value for the chart
  
  # Dynamically calculate the x-axis limits based on data
  x_min <- min(graph_data$lower)
  x_max <- max(graph_data$upper)
  
  # Create the plot with ggplot
  p <- ggplot() +
    # Add bars for each variable
    geom_rect(data = graph_data, aes(xmin = pmin(lower, base), 
                                     xmax = pmax(lower, base),
                                     ymin = index - 0.4, ymax = index + 0.4),
              fill = 'red', color = 'black') +
    geom_rect(data = graph_data, aes(xmin = pmin(upper, base), 
                                     xmax = pmax(upper, base),
                                     ymin = index - 0.4, ymax = index + 0.4),
              fill = 'green', color = 'black') +
    
    # Add a dashed vertical line for the base value
    geom_vline(xintercept = base_value, linetype = "dashed", color = "black") +
    
    # Set up axis ticks and labels
    scale_y_continuous(breaks = graph_data$index, 
                       labels = graph_data$variable,
                       limits = c(-1, nrow(graph_data)+1)) # +
    # 
    # # Adjust the axis limits based on the calculated range
    # xlim(x_min - 10, x_max + 10)
  
  p <- add_theme_and_save(p, fig_path, case_study_num, chart_name)
  
  return(p)
}




graph_pop_waterfall <- function(fig_path, case_study_num, pop_waterfall_chart_df) {
  chart_name <- "pop_waterfall"
  
  p <- waterfall(pop_waterfall_chart_df, calc_total = TRUE)
  
  p <- add_theme_and_save(p, fig_path, case_study_num, chart_name)
  
  return(p)
}


graph_benefit_waterfall <- function(fig_path, case_study_num, benefit_waterfall_chart_df) {
  chart_name <- "benefit_waterfall"
  
  p <- waterfall(benefit_waterfall_chart_df, calc_total = TRUE)
  
  p <- add_theme_and_save(p, fig_path, case_study_num, chart_name, avoid_overlap_x_axis = TRUE)
  
  return(p)
}


graph_stacked_benefits_over_costs <- function(fig_path, case_study_num, stacked_roi_chart_df) {
  chart_name <- "stacked_benefits_over_costs"
  
  # Define specific colors for each benefit type and research costs
  colors <- c(
    "research_costs" = "#e31a1c",            # Red for costs
    "qaly_gains" = "#66c2a5",        # Light green
    "healthcare_cost_savings" = "#99d8c9",  # Pale green
    "socialcare_cost_savings" = "#41ae76",  # Medium green
    "productivity_gains" = "#238b45"       # Dark green
  )
  
  
  # Calculate the total benefits and research costs
  summary_df <- stacked_roi_chart_df %>%
    group_by(position) %>%
    summarise(values = sum(values))
  
  total_benefits <- summary_df$values[summary_df$position == "Benefits"]
  total_costs <- summary_df$values[summary_df$position == "Costs"]
  
  roi_val <-  total_benefits/ total_costs
  roi_val <- round(roi_val, 1)
  
  cost_max <- max(total_costs, total_benefits)*1.1
  
  # Modify the plot to have research costs on the left and stacked benefits on the right
  p <- ggplot(stacked_roi_chart_df, aes(x = position, y = values, fill = category)) +
    geom_bar(stat = "identity", position = "stack", width = 0.6) +
    # Add text at the top with the ROI
    annotate("text", x = 1.5, y = cost_max , label = paste("ROI: ", roi_val), vjust = 1.5, hjust = 0.5, size = 5) +
    labs(title = "Research Costs vs. Stacked Benefits", x = "", y = "Value") +
    scale_fill_manual(values = colors) 
  
  p <- add_theme_and_save(p, fig_path, case_study_num, chart_name, avoid_overlap_x_axis = TRUE)
  
  return(p)
}


##### FUNCTIONS TO FORMAT DATA FOR GRAPHS######
format_total_to_prob <- function(total_benefits_df, case_study_num) {
  reference_val <- total_benefits_df %>%
    filter(case_study == case_study_num) %>%
    filter(str_detect(scenario, "reference")) %>%
    pull(roi)
  
  probabilistic_df <- total_benefits_df %>%
    filter(case_study == case_study_num) %>%
    filter(str_detect(scenario, "prob")) %>%
    mutate(reference = reference_val) %>%
    select(case_study, scenario, roi, reference)
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

format_to_ci <- function(total_benefits_df, confidence = 0.95) {
  uncertainty <- 1-confidence
  
  reference_df <- total_benefits_df %>%
    group_by(case_study) %>%
    filter(str_detect(scenario, "reference")) %>%
    select(case_study, reference = roi) 
  
  summary_ci_df <- total_benefits_df %>%
    group_by(case_study) %>%
    filter(str_detect(scenario, "probabilistic")) %>%
    select(case_study, roi) %>%
    summarise(lower = quantile(roi, probs = uncertainty/2), 
              upper = quantile(roi, probs = 1 - uncertainty/2))
  
  summary_ci_df <- left_join(reference_df, summary_ci_df, by = "case_study")
  
  return(summary_ci_df)
}


format_total_to_comparison_prob <- function(total_benefits_df) {
  probabilistic_df <- total_benefits_df %>%
    filter(str_detect(scenario, "prob")) %>%
    select(case_study, scenario, roi)
  return(probabilistic_df)
}

format_granular_to_coverage_df <- function(granular_benefits_df, case_study_num) {
  coverage_df <- granular_benefits_df %>%
    filter(str_detect(scenario, "prob|reference")) %>%
    mutate(reference = ifelse(str_detect(scenario, "reference"), "reference", "probabilistic"))%>%
    filter(case_study == case_study_num) %>%
    select(year, scenario, reference, coverage) %>%
    return(coverage_df)
}

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
  uptake_ci_df <- left_join(uptake_ci_df, reference_vals, by = "year")
  return(uptake_ci_df)
}

format_to_waterfall <- function(granular_benefits_df, parameter_scenarios, case_study_num, expected_bridge_cols = c("init_pop", "target_pop", "benefitting_pop", "qaly_gains", "healthcare_cost_savings", "socialcare_cost_savings", "productivity_gains")) {
  stopifnot(all(c(expected_bridge_cols %in% colnames(granular_benefits_df))))
  
  reference_only <- granular_benefits_df %>%
    filter(scenario == "reference") %>%
    filter(case_study == case_study_num) 
  
  years_benefits_assumed_to_accrue <- c("min_year" = min(granular_benefits_df$year), 
                                        "max_year" = max(granular_benefits_df$year))
  
  research_costs_val <- parameter_scenarios %>%
    filter(scenario == "reference") %>%
    filter(case_study_number == case_study_num ) %>%
    select(research_costs_value) %>%
    pull()
  
  all_waterfall_df <- reference_only %>%
    summarise(across(all_of(expected_bridge_cols), sum)) %>%
    mutate(research_costs = research_costs_val) %>%
    mutate(total_benefits = qaly_gains + healthcare_cost_savings + socialcare_cost_savings + productivity_gains) 
  
  
  pop_waterfall_chart_df <- all_waterfall_df %>%
    select(all_of(c("init_pop", "target_pop", "benefitting_pop"))) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column("category") %>%
    rename(value = 2) %>%
    mutate(lag_value = lag(value, default = 0)) %>%
    mutate(values = value - lag_value) %>%
    select(category, values)
  
  benefitting_pop <- all_waterfall_df$benefitting_pop
  
  benefit_waterfall_chart_df <- all_waterfall_df %>%
    select(all_of(c("qaly_gains", "healthcare_cost_savings", "socialcare_cost_savings", "productivity_gains"))) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column("category") %>%
    rename(values = 2) %>%
    mutate(values = values / benefitting_pop) %>%
    mutate(values = round(values, 0)) 
  
  stacked_roi_chart_df <- all_waterfall_df %>%
    select(all_of(c("research_costs", "qaly_gains", "healthcare_cost_savings", "socialcare_cost_savings", "productivity_gains"))) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column("category") %>%
    rename(values = 2) %>%
    mutate(values = round(values, 0)) %>%
    mutate(category = factor(category, levels = c("qaly_gains", "healthcare_cost_savings", "socialcare_cost_savings", "productivity_gains", "research_costs"))) %>%
    mutate(position = ifelse(category == "research_costs", "Costs", "Benefits")) %>%
    mutate(position = factor(position, levels = c("Costs", "Benefits")))
  
  return_list <- list(years_benefits_assumed_to_accrue = years_benefits_assumed_to_accrue,
                      all_waterfall_df = all_waterfall_df,
                      pop_waterfall_chart_df = pop_waterfall_chart_df,
                      benefit_waterfall_chart_df = benefit_waterfall_chart_df,
                      stacked_roi_chart_df = stacked_roi_chart_df)
  return(return_list)
}

##### FUNCTIONS TO PRODUCE ALL THE GRAPHS######
overall_graphs <- function(total_benefits_df) {
  comparison_probability_df <- format_total_to_comparison_prob(total_benefits_df)
  print(graph_boxplot_probability_comparison(fig_path, case_study_num = "comparison", comparison_probability_df))
  
  summary_ci_df <- format_to_ci(total_benefits_df, confidence = 0.95)
  print(graph_comparison_roi_ci(fig_path, case_study_num = "comparison", summary_ci_df))
  
}

case_study_specific_graphs <- function(total_benefits_df, granular_benefits_df, case_study_num= 99999) {
  probabilistic_df <- format_total_to_prob(total_benefits_df, case_study_num) 
  print(graph_probability_histogram(fig_path, case_study_num, probabilistic_df))
  
  determ_df <- format_total_to_determ(total_benefits_df, case_study_num)
  print(graph_tornado_determ(fig_path,  case_study_num, determ_df))
  
  coverage_df <- format_granular_to_coverage_df(granular_benefits_df, case_study_num)
  uptake_ci_df <- format_uptake_to_confidence_interval(coverage_df, confidence = 0.95)
  print(graph_uptake_projection(fig_path, case_study_num, coverage_df))
  print(graph_uptake_confidence_interval(fig_path, case_study_num, uptake_ci_df))
  
  all_waterfall_list <- format_to_waterfall(granular_benefits_df, parameter_scenarios, case_study_num)
  print(graph_stacked_benefits_over_costs(fig_path, case_study_num, all_waterfall_list$stacked_roi_chart_df))
  print(graph_pop_waterfall(fig_path, case_study_num, all_waterfall_list$pop_waterfall_chart_df))
  print(graph_benefit_waterfall(fig_path, case_study_num, all_waterfall_list$benefit_waterfall_chart_df))
}

