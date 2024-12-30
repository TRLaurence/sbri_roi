library(stringr)
library(dplyr)
library(tidyr)

#' @title Set up reference scenario
#' @description This function sets up the reference scenario
#' @param parameter_vals A dataframe containing the parameter values
#' @return A dataframe containing the reference scenario
#' 
#' @examples
#' 
#' parameter_vals <- data.frame(
#'  initial_population_value = 1000,
#'  initial_population_lower = 900,
#'  initial_population_upper = 1100,
#'  healthcare_cost_savings_value = 1000,
#'  healthcare_cost_savings_lower = 900,
#'  healthcare_cost_savings_upper = 1100,
#'  healthcare_cost_savings_year = 2020
#'  )
#' 
#' set_up_reference(parameter_vals)
#' 
#' 
#' @export
set_up_reference <- function(parameter_vals) {
  parameter_vals <-  select(parameter_vals, 
                               !contains("upper") & !contains("lower"))
  parameter_vals <- mutate(parameter_vals, 
                           scenario = "reference")
  return(parameter_vals)
}

#' @title Sets up deterministic parameter values
#' @description This creates a parameter value row for each upper and lower value of each parameter
#' @param parameter_vals A dataframe containing the parameter values, the lower and upper values, and the year
#' @return A dataframe with a row for each parameter value and sensitivity
#' 
#' @examples
#' 
#' parameter_vals <- data.frame(
#'  initial_population_value = 1000,
#'  initial_population_lower = 900,
#'  initial_population_upper = 1100,
#'  healthcare_cost_savings_value = 1000,
#'  healthcare_cost_savings_lower = 900,
#'  healthcare_cost_savings_upper = 1100,
#'  healthcare_cost_savings_year = 2020
#'  )
#' 
#' set_up_reference(parameter_vals)
#' 
#' 
#' @export
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

#' @title Finds the appropriate distribution for a parameter
#' @param parameter_val The name of the parameter
#' @return str - The name of the distribution
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
  
  return(distributions_vector[[parameter_val]])
}

#' @title Obtain the alpha and beta parameters from a gamma distribution
#' @param mean_value The mean value of the distribution
#' @param std_dev The standard deviation of the distribution
#' @return A list containing the alpha and beta parameters
gamma_params <- function(mean_value, std_dev) {
  stopifnot(mean_value > 0, std_dev > 0)
  beta <- mean_value / (std_dev^2)
  alpha <- mean_value * beta
  return(list(alpha = alpha, beta = beta))
}

#' @title Obtain the alpha and beta parameters from a beta distribution
#' @param mean_value The mean value of the distribution
#' @param std_dev The standard deviation of the distribution
#' @return A list containing the alpha and beta parameters
beta_params <- function(mean_value, std_dev) {
  stopifnot(mean_value > 0, mean_value < 1, std_dev > 0, std_dev < 1)
  var_value <- std_dev^2
  alpha <- ((mean_value * (1 - mean_value)) / var_value - 1) * mean_value
  beta <- alpha * (1 / mean_value - 1)
  return(list(alpha = alpha, beta = beta))
}

#' @title Generate a distribution of parameter values for a given distrbution
#' @param mean_value The mean value of the distribution
#' @param lower_bound The lower bound of the distribution
#' @param upper_bound The upper bound of the distribution
#' @param n The number of samples to generate
#' @param distribution The distribution to use (normal, gamma, beta, uniform)
#' @return A vector of n samples from the distribution
#' @export
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
#' @title Create a probabilistic data frame
#' @param case_study_vals A data frame of parameter values filtered to the case study
#' @param parameters_to_change A vector of parameter names to change these should match exactly to the column names in case_study_vals
#' e.g initial_population_value or coverage_init_value
#' @param number_of_samples The number of samples to generate
#' @return A data frame of probabilistic parameter values
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

#' @title Set up a probabilistic data frame
#' @param parameter_vals A data frame of parameter values
#' @param number_of_samples The number of samples to generate
#' @return A data frame of probabilistic parameter values
set_up_probabilistic <-  function(parameter_vals, number_of_samples) {
  parameters_to_change <- names(parameter_vals)[str_detect(names(parameter_vals), "_value")]
  
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
  
  return(df_of_sensitivities)
}

#' @title Set up all of the sensitivities for the analysis
#' @param parameter_vals A data frame of parameter values, anything with _value in the name will be considered a parameter
#' and requires a _lower and _upper value even if they're the same
#' @param deterministic_sensitivity A boolean to determine if deterministic sensitivity should be included
#' @param probabilistic_sensitivity A boolean to determine if probabilistic sensitivity should be included
#' @param number_of_samples The number of samples to generate for probabilistic sensitivity
#' 
#' @return A data frame of all of the sensitivities without the upper and lower bounds
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

