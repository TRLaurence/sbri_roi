source("R/vis/graph_utils.R")
library(ggplot2)
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

graph_probability_histogram <- function(fig_path, case_study_num, probabilistic_df) {
  chart_name <- "prob_histogram"
  p <- ggplot(probabilistic_df, aes(x = roi)) +
    geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 0.1, fill = "lightblue", color = "black") +
    labs(title = "Probability Histogram of ROIs", x = "ROI", y = "Probability")
  
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
  p <- ggplot(uptake_df, aes(x = year, y = coverage, group = scenario), alpha = 0.01) +
    geom_line() +
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


tornado_ggplot <- function(fig_path, case_study_num, graph_data) {
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




tornado_ggplot(fig_path, 99999, graph_data)
