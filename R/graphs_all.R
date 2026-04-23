source("R/graph_utils.R")
library(ggplot2)
library(tibble)
# library(waterfalls)
library(scales)

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

estimate_max_y <- function(probabilistic_df, bins = 20) {
  # Generate the histogram counts with probabilities
  hist_data <- hist(probabilistic_df$roi, breaks = bins, plot = FALSE)
  
  # Convert counts to probabilities
  probabilities <- hist_data$counts / sum(hist_data$counts)
  
  # Get the maximum probability for setting max_y
  max_y <- max(probabilities)
  
  return(max_y)
}

##### FUNCTIONS TO DRAW GRAPHS ######
graph_probability_histogram <- function(fig_path, case_study_num, probabilistic_df) {
  chart_name <- "prob_histogram"
  
  # Estimate max_y based on the data
  max_y <- estimate_max_y(probabilistic_df) + 0.1  # Adding a small buffer for visual clarity
  
  upper_val <- probabilistic_df$upper[1]
  reference_val <- probabilistic_df$reference[1]
  lower_val <- probabilistic_df$lower[1]
  
  # 1 DP
  upper_lab <- paste("Upper:", format(round(upper_val, 1), nsmall = 1))
  ref_lab <- paste("Ref:", format(round(reference_val, 1), nsmall = 1))
  lower_lab <- paste("Lower:", format(round(lower_val, 1), nsmall = 1))
  
  # Define a small buffer for text positioning on the x-axis
  x_buffer <- 0.02 * (max(probabilistic_df$roi) - min(probabilistic_df$roi))
  
  p <- ggplot(probabilistic_df, aes(x = roi)) +
    geom_histogram(aes(y = after_stat(count)/sum(after_stat(count))), bins = 20, fill = COLOR_CATEGORICAL['Dark Blue'], color = "black") +
    
    # Vertical lines with labels to the right
    geom_vline(aes(xintercept = reference), color = COLOR_CATEGORICAL['Red'], linetype = "dashed") +
    geom_text(aes(x = reference + x_buffer, y = max_y, label = ref_lab), 
              color = COLOR_CATEGORICAL['Dark Blue'], 
              # vjust = -0.5,
              hjust = 0,
              family = FONT_FAMILY, 
              size = 3) +
    
    geom_vline(aes(xintercept = upper), color = COLOR_CATEGORICAL['Orange'], linetype = "dashed") +
    geom_text(aes(x = upper + x_buffer, y = max_y, label = upper_lab), 
              color = COLOR_CATEGORICAL['Dark Blue'], 
              # vjust = -0.5,
              hjust = 0,
              family = FONT_FAMILY, 
              size = 3) +
    
    geom_vline(aes(xintercept = lower), color = COLOR_CATEGORICAL['Orange'], linetype = "dashed") +
    geom_text(aes(x = lower + x_buffer, y = max_y, label = lower_lab), 
              color = COLOR_CATEGORICAL['Dark Blue'], 
              # vjust = -0.5, 
              hjust = 0,
              family = FONT_FAMILY, 
              size = 3) +
    
    labs(title = "", x = "ROI", y = "Probability")
  
  p <- add_theme_and_save(p, fig_path, case_study_num, chart_name)
  return(p)
}

log_tranform_graph <- function(p, log_transform = TRUE) {
  if (log_transform) {
    p <- p + scale_y_log10()
  } else {
    p <- p + scale_y_continuous()
  }
  
  return(p)
}

graph_comparison_roi_ci <- function(fig_path, case_study_num = "comparison", summary_ci_df, case_study_mapping, log_transform = FALSE) {
  # dev.off()
  chart_name <- "roi_summary_ci"
  # Example plot
  summary_ci_df$case_study <- factor(summary_ci_df$case_study, levels = case_study_mapping)
  
  p <- ggplot(summary_ci_df, aes(x = case_study, y = reference), color = COLOR_CATEGORICAL['Dark Blue']) +
    geom_point(size = 3) +  # Points for ROI
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +  # Error bars
    labs(
      title = "",
      x = "Case Study",
      y = "ROI"
    )
    
  p <- log_tranform_graph(p, log_transform) 
  
  if(log_transform) {
    chart_name <- paste(chart_name, "_log", sep = "")
  } 
  
  p <- add_theme_and_save(p, fig_path, case_study_num, chart_name)
  
  return(p)
}

graph_boxplot_probability_comparison <- function(fig_path, case_study_num = "comparison", comparison_probability_df) {
  # dev.off()
  chart_name <- "comparison_boxplot"
  p <- ggplot(comparison_probability_df, aes(x = case_study, y = roi, group = case_study)) +
    geom_boxplot(color = COLOR_CATEGORICAL['Dark Blue']) +
    coord_flip() +
    labs(title = "Comparison of ROIs", x = "Case Study", y = "ROI")
  
  p <- add_theme_and_save(p, fig_path, case_study_num, chart_name)
  return(p)
}

graph_uptake_projection <- function(fig_path, case_study_num, uptake_df) {
  # dev.off()
  chart_name <- "uptake_projection"
  
  p <- ggplot(uptake_df, aes(x = year, y = coverage, group = scenario)) +
    geom_line(alpha = 0.1, color = COLOR_CATEGORICAL['Dark Blue']) +
    labs(title = "", x = "Year", y = "Uptake") +
    scale_y_continuous(labels = percent_format()) +  # Format y-axis as a percentage
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))  
  
  p <- add_theme_and_save(p, fig_path, case_study_num, chart_name)
  return(p)
}

graph_uptake_confidence_interval <- function(fig_path, case_study_num, uptake_ci_df) {
  chart_name <- "uptake_confidence_interval"
  p <- ggplot(uptake_ci_df, aes(x = year, y = reference_coverage)) +
    # Add a swathe for the confidence interval
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = COLOR_SEQUENTIAL[['80Dark Blue']], color = "white") +
    # Add a line just for reference_case
    geom_line(color = COLOR_SEQUENTIAL[['100Dark Blue']]) +
    labs(title = "", x = "Year", y = "Uptake") +
    scale_y_continuous(labels = percent_format())  +  # Format y-axis as a percentage
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 
  
  p <- add_theme_and_save(p, fig_path, case_study_num, chart_name)
  return(p)
}

graph_tornado_determ <- function(fig_path, case_study_num, graph_data) {
  # dev.off()
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
              fill = COLOR_STOPLIGHT['Stop'], color = 'black') +
    geom_rect(data = graph_data, aes(xmin = pmin(upper, base), 
                                     xmax = pmax(upper, base),
                                     ymin = index - 0.4, ymax = index + 0.4),
              fill = COLOR_STOPLIGHT['Go'], color = 'black') +
    
    # Add a dashed vertical line for the base value
    geom_vline(xintercept = base_value, linetype = "dashed", color = "black") +
    
    # Set up axis ticks and labels
    scale_y_continuous(breaks = graph_data$index, 
                       labels = graph_data$variable,
                       limits = c(-1, nrow(graph_data)+1)) +
    
    labs(y = "Model variable", x = "ROI Estimate")
  
  p <- add_theme_and_save(p, fig_path, case_study_num, chart_name)
  
  return(p)
}

waterfall_chart <- function(data, show_values = TRUE, units = "none", log_y = FALSE, ppt_version = FALSE) {
  
  if (ppt_version) {
    FONT_FAMILY <- "Arial"
  }
  
  # Custom y-axis label formatter
  y_formatter <- function(x) {
    format_units(x, units)
  }
  log_formatter <- function(p, log_y) {
    if (log_y) {
      p <- p +
        scale_y_log10(labels = y_formatter) 
    } else {
      p <- p +
        scale_y_continuous(labels = y_formatter)
    }
  }

  # Create the plot
  p <- ggplot(data) +
    geom_rect(aes(xmin = min, xmax = max, ymin = start, ymax = end, fill = sign), color = "black") +
    scale_fill_manual(values = c("Positive" = COLOR_STOPLIGHT[['Go']], "Negative" = COLOR_STOPLIGHT[['Stop']])) +
    scale_x_continuous(breaks = (data$min + data$max) / 2, labels = data$category)
  
  p <- log_formatter(p, log_y)       
  
  # Create data frame for segments (connecting lines)
  if (nrow(data) > 1) {
    segment_data <- data.frame(
      x = data$max[-nrow(data)],
      xend = data$min[-1],
      y = data$end[-nrow(data)],
      yend = data$start[-1]
    )
    p <- p + geom_segment(data = segment_data, aes(x = x, xend = xend, y = y, yend = yend), linetype = "dotted")
  }
  
  # Add value labels if show_values is TRUE
  if (show_values) {
    data$mid_x <- (data$min + data$max) / 2
    data$mid_y <- (data$start + data$end) / 2
    data$formatted_value <- format_units(data$value, units)
    
    
    scale_color_vals <-c("#000000", "#FFFFFF")
    unique_vals <- data$text_color %>% unique()
    # Reverse the order of the colors so that the negative values are red
    
    scale_color_vals <- scale_color_vals[scale_color_vals %in% unique_vals] 
    
    stopifnot(length(scale_color_vals) > 0)
    
    # Explicitly specify the data in geom_text
    if (log_y) {
      p <- p + geom_text(data = data, 
                         aes(x = mid_x, 
                             y = mid_y + text_offset, 
                             label = formatted_value, 
                             color = text_color), 
                         size = 3, 
                         family = FONT_FAMILY)   + 
        scale_color_manual(values = scale_color_vals)
    } else {
      p <- p + geom_text(data = data, 
                         aes(x = mid_x, 
                             y = mid_y + text_offset, 
                             label = formatted_value, 
                             color = text_color), 
                         size = 3, 
                         family = FONT_FAMILY)  + 
        scale_color_manual(values =scale_color_vals)
    }
  }
  
  # Display the plot
  return(p)
}

graph_pop_waterfall <- function(fig_path, case_study_num, pop_waterfall_chart_df, ppt_version = FALSE) {
  # dev.off()
  chart_name <- "pop_waterfall"
  
  p <- waterfall_chart(pop_waterfall_chart_df, ppt_version = ppt_version)
  
  if (ppt_version) {
    x_lab <- ""
    y_lab <- "Modelled population (people)"  
  } else {
    x_lab <- "Adjustment"
    y_lab <- "Modelled population (people)"
  }
  
  
  p <- p +
    labs(x = x_lab,
         y = y_lab)
    
  p <- add_theme_and_save(p, fig_path, case_study_num, chart_name, legend_pos = "none", ppt_version = ppt_version)
  
  return(p)
}


graph_benefit_waterfall <- function(fig_path, case_study_num, benefit_waterfall_chart_df, ppt_version = FALSE) {
  # dev.off()
  chart_name <- "benefit_waterfall"
  
  p <- waterfall_chart(benefit_waterfall_chart_df, ppt_version = ppt_version)
  
  if (ppt_version) {
    x_lab <- ""
    y_lab <- "Net benefit (GBP)"    
  } else {
    x_lab <- "Benefit type"
    y_lab <- "Net benefit (GBP)"
  }
  
  p <- p +
    labs(x = x_lab,
         y = y_lab)
  
  p <- add_theme_and_save(p, fig_path, case_study_num, chart_name, legend_pos = "none", ppt_version = ppt_version)
  
  return(p)
}


graph_stacked_benefits_over_costs <- function(fig_path, case_study_num, stacked_roi_chart_df) {
  # dev.off()
  chart_name <- "stacked_benefits_over_costs"
  
  # Define specific colors for each benefit type and research costs
  colors <- c(
    "Research costs" = COLOR_STOPLIGHT[['Stop']],            # Red for costs
    "QALY gains" = COLOR_CATEGORICAL[["Dark Blue"]],        
    "Healthcare cost savings" = COLOR_CATEGORICAL[["Purple"]],  
    "Social care cost savings" = COLOR_CATEGORICAL[["Teal"]],  
    "Productivity gains" = COLOR_CATEGORICAL[["Green"]],
    "Optimism bias adjustment" = COLOR_CATEGORICAL[["Orange"]]
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
    annotate("text", x = 1.5, y = cost_max , label = paste("ROI: ", roi_val), vjust = 1.5, hjust = 0.5, size = 4, family = FONT_FAMILY) +
    labs(title = "", x = "", y = "Monetary value") +
    scale_y_continuous(labels = label_comma()) +
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
  
  upper_and_lower_ci <- total_benefits_df %>%
    filter(case_study == case_study_num) %>%
    filter(str_detect(scenario, "probabilistic")) %>%
    summarise(upper = quantile(roi, 0.95), 
              lower = quantile(roi, 0.05))
  
  probabilistic_df <- total_benefits_df %>%
    filter(case_study == case_study_num) %>%
    filter(str_detect(scenario, "prob")) %>%
    mutate(reference = reference_val) %>%
    mutate(upper = upper_and_lower_ci$upper) %>% 
    mutate(lower = upper_and_lower_ci$lower) %>%
    select(case_study, scenario, roi, reference, upper, lower)
  return(probabilistic_df)
}

format_variable_names <- function(variable) {
  names_at_start <- names(variable)
  capitalise_first_letter <- function(string_val) {
    string_val <- paste(str_to_upper(str_sub(string_val, 1, 1)),str_sub(string_val, 2, -1), sep = "")
    return(string_val)
  }
  
  variable <- str_replace_all(variable, "_", " ")
  # Capitalise QALY and the first letter of the entire string
  variable <- sapply(variable, capitalise_first_letter)
  variable <- str_replace_all(variable, "Qaly|qaly", "QALY")
  variable <- str_replace_all(variable, "Socialcare", "Social care")
  names(variable) <- names_at_start
  return(variable)
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
  
  determ_df$variable <- format_variable_names(determ_df$variable)
  
  return(determ_df)
}

format_to_ci <- function(total_benefits_df, case_study_mapping, confidence = 0.95) {
  uncertainty <- 1-confidence
  
  reference_df <- total_benefits_df %>%
    group_by(case_study) %>%
    filter(str_detect(scenario, "reference")) %>%
    select(case_study, reference = roi) %>%
    ungroup()
  
  summary_ci_df <- total_benefits_df %>%
    group_by(case_study) %>%
    filter(str_detect(scenario, "probabilistic")) %>%
    select(case_study, roi) %>%
    summarise(lower = quantile(roi, probs = uncertainty/2), 
              upper = quantile(roi, probs = 1 - uncertainty/2)) %>%
    ungroup()
  
  summary_ci_df <- left_join(reference_df, summary_ci_df, by = "case_study")
  
  summary_ci_df$case_study <- as.character(summary_ci_df$case_study)
  

  summary_ci_df$case_study <- case_study_mapping[summary_ci_df$case_study]
  
  return(summary_ci_df)
}


format_total_to_comparison_prob <- function(total_benefits_df) {
  probabilistic_df <- total_benefits_df %>%
    filter(str_detect(scenario, "prob")) %>%
    select(case_study, scenario, roi)
  
  # Format the case study (currently an integer) to a factor for the chart
  probabilistic_df$case_study <- as.factor(probabilistic_df$case_study)
  
  return(probabilistic_df)
}

format_granular_to_coverage_df <- function(granular_benefits_df, case_study_num) {
  coverage_df <- granular_benefits_df %>%
    filter(str_detect(scenario, "prob|reference")) %>%
    mutate(reference = ifelse(str_detect(scenario, "reference"), "reference", "probabilistic"))%>%
    filter(case_study == case_study_num) %>%
    select(year, scenario, reference, coverage) 
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


remap_to_fancy_categories <- function(variable, fancy_categories) {
  # If fancy categories is a dataframe or tibble, convert to a named vector
  if (is.data.frame(fancy_categories) | is_tibble(fancy_categories)) {
    names(fancy_categories) <- str_remove(names(fancy_categories), "_name")
    # Convert first row of the dataframe to named vector
    fancy_categories <- setNames(as.vector(unlist(fancy_categories[1, ])), names(fancy_categories))
  }
  
  # Remap the variable to the fancy categories
  remapped_variable <- as.vector(fancy_categories[variable])
  
  return(remapped_variable)
}

format_df_for_waterfall <- function(data, fancy_categories = NULL, str_wrap_val = 20) {
  # Ensure data has 'category' and 'value' columns
  if (!all(c("category", "value") %in% names(data))) {
    stop("Data must contain 'category' and 'value' columns")
  }
  
  # Calculate total
  total_value <- sum(data$value)
  total_category <- "total"
  
  # Add total row to data
  total_row <- data.frame(
    category = total_category,
    value = total_value
  )
  
  # Combine data and total_row
  data <- rbind(data, total_row)
  
  # Calculate cumulative sums for start and end positions
  data$start <- c(1, head(cumsum(data$value), -1))
  data$end <- c(head(cumsum(data$value), -1), 1)
  
  # Determine if the value is positive or negative
  data$sign <- ifelse(data$value >= 0, "Positive", "Negative")
  
  if (!is.null(fancy_categories)) {
    data$category <- remap_to_fancy_categories(data$category, fancy_categories)
    # TODO switch back to 20
    data$category <- str_wrap(data$category, width = str_wrap_val)
  } 
  data$category <- factor(data$category, levels = data$category)
  # Position for rectangles
  data$min <- as.numeric(data$category) - 0.4
  data$max <- as.numeric(data$category) + 0.4
  
  data$text_offset <- ifelse(abs(data$value) / max(abs(data$value)) < 0.05, 0.05*max(data$value), 0)
  # Hex for white or black text
  data$text_color <- ifelse(data$text_offset == 0, "#FFFFFF", "#000000")
  
  return(data)
}

format_to_waterfall <- function(granular_benefits_df, reference_research_costs_only, case_study_num, name_vals, expected_bridge_cols = c("init_pop", "target_pop", "benefitting_pop", "qaly_gains", "healthcare_cost_savings", "socialcare_cost_savings", "productivity_gains", "optimism_bias_adjustment")) {
  stopifnot(all(c(expected_bridge_cols %in% colnames(granular_benefits_df))))
  
  reference_only <- granular_benefits_df %>%
    filter(scenario == "reference") %>%
    filter(case_study == case_study_num) 
  
  years_benefits_assumed_to_accrue <- c("min_year" = min(granular_benefits_df$year), 
                                        "max_year" = max(granular_benefits_df$year))
  
  research_costs_val <- reference_research_costs_only %>%
    filter(case_study == case_study_num ) %>%
    select(research_costs) %>%
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
    mutate(value = value - lag_value) %>%
    select(category, value)
  
  pop_fancy_categories <- filter(name_vals, case_study_number == case_study_num) %>%
    select(-case_study_number) 
  
  #TODO Switch str_wrap_val back to 20
  pop_waterfall_chart_df <- format_df_for_waterfall(pop_waterfall_chart_df, pop_fancy_categories, str_wrap_val = 20) # 18 for ppt
  
  benefitting_pop <- all_waterfall_df$benefitting_pop
  
  benefit_waterfall_chart_df <- all_waterfall_df %>%
    select(all_of(c("qaly_gains", "healthcare_cost_savings", "socialcare_cost_savings", "productivity_gains", "optimism_bias_adjustment"))) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column("category") %>%
    rename(value = 2) %>%
    mutate(value = value / benefitting_pop) %>%
    mutate(value = round(value, 0)) 
  
  benefit_fancy_categories <- c("qaly_gains" = "QALY gains", 
                                "healthcare_cost_savings" = "Healthcare cost savings", 
                                "socialcare_cost_savings" = "Social care cost savings", 
                                "productivity_gains" = "Productivity gains",
                                "optimism_bias_adjustment" = "Optimism bias adjustment",
                                "research_costs" = "Research costs",
                                "total" = "Total benefits")
  #TODO Switch str_wrap_val back to 20
  benefit_waterfall_chart_df <- format_df_for_waterfall(benefit_waterfall_chart_df, benefit_fancy_categories, str_wrap_val = 20) # 15 for ppt
  
    
  stacked_roi_chart_df <- all_waterfall_df %>%
    select(all_of(c("research_costs", "qaly_gains", "healthcare_cost_savings", "socialcare_cost_savings", "productivity_gains","optimism_bias_adjustment"))) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column("category") %>%
    rename(values = 2) %>%
    mutate(values = round(values, 0)) %>%
    mutate(position = ifelse(category == "research_costs", "Costs", "Benefits")) %>%
    mutate(position = factor(position, levels = c("Costs", "Benefits"))) %>%
    mutate(category = remap_to_fancy_categories(category, benefit_fancy_categories)) %>%
    mutate(category = factor(category, levels = category)) 
  
  return_list <- list(years_benefits_assumed_to_accrue = years_benefits_assumed_to_accrue,
                      all_waterfall_df = all_waterfall_df,
                      pop_waterfall_chart_df = pop_waterfall_chart_df,
                      benefit_waterfall_chart_df = benefit_waterfall_chart_df,
                      stacked_roi_chart_df = stacked_roi_chart_df)
  return(return_list)
}

##### FUNCTIONS TO PRODUCE ALL THE GRAPHS######
overall_graphs <- function(total_benefits_df, case_study_mapping, dummy= FALSE) {
  
  case_study_mapping_wrapped <- str_wrap(case_study_mapping, width = 15)
  names(case_study_mapping_wrapped) <- names(case_study_mapping)
  
  comparison_probability_df <- format_total_to_comparison_prob(total_benefits_df)
  print(graph_boxplot_probability_comparison(fig_path, case_study_num = "comparison", comparison_probability_df))
  

  summary_ci_df <- format_to_ci(total_benefits_df, case_study_mapping_wrapped, confidence = 0.90)
  if (dummy){
    min_roi <- min(summary_ci_df$reference)
    mean_roi <- mean(summary_ci_df$reference)
    summary_ci_df <- summary_ci_df %>%
      bind_rows(data.frame(case_study = "Remaining\nAwards", reference = 1, lower = 0, upper = min_roi)) %>%
      bind_rows(data.frame(case_study = "Overall\nProgramme", reference = (mean_roi + 1)/2, lower = 1, upper = (mean_roi + min_roi)/2))
  }
  print(graph_comparison_roi_ci(fig_path, case_study_num = "comparison", summary_ci_df, case_study_mapping_wrapped, log_transform = TRUE))
  print(graph_comparison_roi_ci(fig_path, case_study_num = "comparison", summary_ci_df, case_study_mapping_wrapped, log_transform = FALSE))
  
}

case_study_specific_graphs <- function(total_benefits_df, reference_research_costs_only, granular_benefits_df, name_vals, case_study_num= 99999) {
  probabilistic_df <- format_total_to_prob(total_benefits_df, case_study_num)
  print(graph_probability_histogram(fig_path, case_study_num, probabilistic_df))

  determ_df <- format_total_to_determ(total_benefits_df, case_study_num)
  print(graph_tornado_determ(fig_path,  case_study_num, determ_df))

  coverage_df <- format_granular_to_coverage_df(granular_benefits_df, case_study_num)
  uptake_ci_df <- format_uptake_to_confidence_interval(coverage_df, confidence = 0.90)
  print(graph_uptake_projection(fig_path, case_study_num, coverage_df))
  print(graph_uptake_confidence_interval(fig_path, case_study_num, uptake_ci_df))

  all_waterfall_list <- format_to_waterfall(granular_benefits_df, reference_research_costs_only, case_study_num, name_vals)
  print(graph_stacked_benefits_over_costs(fig_path, case_study_num, all_waterfall_list$stacked_roi_chart_df))
  print(graph_pop_waterfall(fig_path, case_study_num, all_waterfall_list$pop_waterfall_chart_df))
  print(graph_benefit_waterfall(fig_path, case_study_num, all_waterfall_list$benefit_waterfall_chart_df))
  print(graph_pop_waterfall(fig_path, case_study_num, all_waterfall_list$pop_waterfall_chart_df, ppt_version = TRUE))
  print(graph_benefit_waterfall(fig_path, case_study_num, all_waterfall_list$benefit_waterfall_chart_df, ppt_version = TRUE))
}




