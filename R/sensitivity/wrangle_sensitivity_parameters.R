library(stringr)
library(dplyr)
library(tidyr)

set_up_reference <- function(parameter_vals) {
  parameter_vals <-  select(parameter_vals, 
                               !contains("upper") & !contains("lower"))
  parameter_vals <- mutate(parameter_vals, 
                           scenario = "reference")
  return(parameter_vals)
}


set_up_deterministic <-  function(parameter_vals) {
  parameters_to_change <- names(parameter_vals)[str_detect(names(parameter_vals), "_value")]
  sensitivity_versions <- c("upper", "lower")
  combinations <- expand.grid(param = parameters_to_change, 
                              sens = sensitivity_versions,
                              stringsAsFactors = FALSE)
  combinations$var_name <- str_remove(parameters_to_change, "_value")  
  combinations$var_to_use <- paste0(combinations$var_name, "_", combinations$sens)
  
  # Order combinations by parameter name
  combinations <- arrange(combinations, param)
  
  
  list_of_sensitivities <- list()
  for (i in 1:nrow(combinations)) {
    param <- combinations$param[i]
    var_name <- combinations$var_name[i]
    var_to_use <- combinations$var_to_use[i]
    sens <- combinations$sens[i]
    
    new_sensitivities <- parameter_vals %>%
      mutate(!!param := !!sym(var_to_use)) %>%
      mutate(scenario = paste0(var_name, "_", sens))
    
    new_sensitivities <-  select(new_sensitivities, 
                                 !contains("upper") & !contains("lower"))
    list_of_sensitivities[[i]] <- new_sensitivities
  }
  df_of_sensitivities <- bind_rows(list_of_sensitivities)
  return(df_of_sensitivities)
}

distribution_mapping <- function(parameter_val) {
  distributions_vector <- c(
    "initial_population" = "gamma", # This value is always positive
    "adjustment_for_sub_population" = "beta", # This value is bounded by 0 and 1
    "coverage_init" = "beta", # This value is bounded by 0 and 1
    "coverage_end" = "beta", # This value is bounded by 0 and 1
    "qaly_gains" = "normal", # This value can be positive or negative
    "healthcare_cost_savings" = "normal", # This cost can be positive or negative because it's net
    "productivity_gains" = "normal", # This cost can be positive or negative because it's net
    "socialcare_cost_savings" = "normal", # This cost can be positive or negative because it's net
    "research_funding" = "gamma", # This cost is always positive
    "optimism_bias_benefits" = "uniform",
    "applied_adjustment" = "normal"
  )

  #Check if the parameter is in the mapping, return error if not found
  if (!(parameter_val %in% names(distributions_vector))) {
    stop(paste("No distribution found for", parameter_val))
  }
  
  return(distributions_vector[parameter_val])
}

# Helper function for Gamma distribution
gamma_params <- function(mean_value, std_dev) {
  beta <- mean_value / (std_dev^2)
  alpha <- mean_value * beta
  return(list(alpha = alpha, beta = beta))
}

# Helper function for Beta distribution
beta_params <- function(mean_value, std_dev) {
  var_value <- std_dev^2
  alpha <- ((mean_value * (1 - mean_value)) / var_value - 1) * mean_value
  beta <- alpha * (1 / mean_value - 1)
  return(list(alpha = alpha, beta = beta))
}


# Main function to generate random samples based on the distribution
generate_distribution <- function(mean_value, lower_bound, upper_bound, n, distribution = c("normal", "gamma", "beta", "uniform")) {
  set.seed(1)
  z_value <- 1.96  # for 95% confidence interval
  ci_width <- abs(upper_bound - lower_bound)
  
  if (ci_width <= 0) {
    return(rep(mean_value, n))
  }
  
  std_dev <- ci_width / (2 * z_value)  # Calculate the standard deviation
  
  distribution <- match.arg(distribution)  # Ensure the distribution is one of the valid options
  
  if (distribution == "normal") {
    # Normal distribution
    samples <- rnorm(n, mean = mean_value, sd = std_dev)
    
  } else if (distribution == "gamma") {
    # Gamma distribution
    params <- gamma_params(mean_value, std_dev)
    samples <- rgamma(n, shape = params$alpha, rate = params$beta)
    
  } else if (distribution == "beta") {
    # Beta distribution
    if (mean_value < 0) {
      stop("Mean for Beta distribution must be between 0 and 1")
    } else if (mean_value > 1) {
      stop("Mean for Beta distribution must be between 0 and 1")
    } else if (mean_value == 0) {
      mean_value = 0.001
    } else if (mean_value == 1) {
      mean_value = 0.999
    }
    
    params <- beta_params(mean_value, std_dev)
    samples <- rbeta(n, shape1 = params$alpha, shape2 = params$beta)
    
  } else if (distribution == "uniform") {
    # Uniform distribution
    if (lower_bound > upper_bound) {
      lower_val_temp <- lower_bound
      lower_bound <- upper_bound
      upper_bound <- lower_val_temp
    }

    samples <- runif(n, min = lower_bound, max = upper_bound)
    
  } else {
    stop("Unsupported distribution type.")
  }
  
  return(samples)
}

create_matching_probabilistic_df <- function(case_study_vals, parameters_to_change, number_of_samples) {
  data_frame_of_vals <- data.frame(scenario = paste0("probabilistic", as.character(1:number_of_samples)))
  for (j in 1:length(parameters_to_change)) {
    param <- parameters_to_change[j]
    var_name <- str_remove(param, "_value")

    distribution <- distribution_mapping(var_name)
    mean_value <- case_study_vals[[param]]
    lower_bound <- case_study_vals[[paste0(var_name, "_lower")]]
    upper_bound <- case_study_vals[[paste0(var_name, "_upper")]]
    samples <- generate_distribution(mean_value, lower_bound, upper_bound, number_of_samples, distribution)
    data_frame_of_vals[[param]] <- samples
  }
  for (col in names(case_study_vals)) {
    if (!(col %in% names(data_frame_of_vals))) {
      data_frame_of_vals[[col]] <- rep(case_study_vals[[col]], number_of_samples)
    }
  }
  
  data_frame_of_vals <- select(data_frame_of_vals, all_of(c(names(case_study_vals), "scenario")))
  return(data_frame_of_vals)
}

set_up_probabilistic <-  function(parameter_vals, number_of_samples) {
  parameters_to_change <- names(parameter_vals)[str_detect(names(parameter_vals), "_value")]
  print("parameters_to_change")
  print(parameters_to_change)
  
  case_studies <- unique(parameter_vals$case_study_number)
  
  list_of_dfs <- list()
  for (i in 1:length(case_studies)) {
    case_study <- case_studies[i]
    case_study_vals <- filter(parameter_vals, case_study_number == case_study)
    
    data_frame_of_vals <- create_matching_probabilistic_df(case_study_vals, parameters_to_change, number_of_samples)
    
    list_of_dfs[[i]] <- data_frame_of_vals
  }
  df_of_sensitivities <- bind_rows(list_of_dfs)
  
  df_of_sensitivities <- df_of_sensitivities %>%
    select(!contains("upper") & !contains("lower"))
}

set_up_all_sensitivities <- function(parameter_vals, deterministic_sensitivity, probabilistic_sensitivity, number_of_samples) {
  reference_df <- set_up_reference(parameter_vals)
  all_df <- reference_df
  if(deterministic_sensitivity) {
    deterministic_df <- set_up_deterministic(parameter_vals)
    all_df <- bind_rows(all_df, deterministic_df)
  }
  if(probabilistic_sensitivity) {
    probabilistic_df <- set_up_probabilistic(parameter_vals, number_of_samples)
    all_df <- bind_rows(all_df, probabilistic_df)
  }
   

  return(all_df)
}

