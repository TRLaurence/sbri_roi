# Define the sigmoid function for modeling uptake
sigmoid <- function(x) {
  1 / (1 + exp(-x))
}


# Define the function to extrapolate coverage over time
extrapolate_sigmoid <- function(init_coverage, final_coverage, years) {
  if (years < 1) {
    return(data.frame(year = numeric(0), coverage = numeric(0)))
  } else {
    # Time points (years)
    time <- seq(0, years-1, by = 1)
    # Parameters for the sigmoid function
    midpoint <- years / 2  # Midpoint of the transition
    growth_rate <- 10 / years  # Growth rate for the transition curve (adjustable)
    
    # Calculate the normalized coverage using the sigmoid function
    norm_coverage <- sigmoid(growth_rate * (time - midpoint))
    # Rescale coverage to fit between init_coverage and final_coverage
    coverage_vec <- init_coverage + (final_coverage - init_coverage) * norm_coverage
    coverage_vec[1] <- init_coverage
    if (years > 1) {
      coverage_vec[years] <- final_coverage  
    }
  }
  # Return a data frame with time and coverage
  return(data.frame(year = time, coverage = coverage_vec))
}

estimate_coverage <- function(init_coverage, final_coverage, year_start_coverage, year_final_coverage, n_years) {
  num_years_sigmoid <- year_final_coverage - year_start_coverage + 1
  
  
  if (n_years > num_years_sigmoid) {
    initial_coverage_data <- extrapolate_sigmoid(init_coverage, final_coverage, num_years_sigmoid)
    remaining_years <- n_years - num_years_sigmoid
    if (n_years == 1) {
      remaining_coverage_data <- data.frame(year = seq(num_years_sigmoid, n_years-1, by = 1), 
                                            coverage = rep(init_coverage, remaining_years))
    } else {
      remaining_coverage_data <- data.frame(year = seq(num_years_sigmoid, n_years-1, by = 1), 
                                            coverage = rep(final_coverage, remaining_years))
    }
    coverage_data <- rbind(initial_coverage_data, remaining_coverage_data)
  } else {
    coverage_data <- extrapolate_sigmoid(init_coverage, final_coverage, num_years_sigmoid)
    coverage_data <- coverage_data[1:n_years, ]
  } 
  
  coverage_data$year <- coverage_data$year + year_start_coverage
  
  return(coverage_data)
}

coverage_adjust_row <- function(row_val, years_of_coverage) {
  init_coverage <- row_val[["coverage_init_value"]]
  final_coverage <- row_val[["coverage_end_value"]]
  year_start_coverage <- row_val[["coverage_init_year"]]
  year_final_coverage <- row_val[["coverage_end_year"]]
  coverage_df <- estimate_coverage(init_coverage, final_coverage, year_start_coverage, year_final_coverage, n_years = years_of_coverage)
  
  return(coverage_df)
}

