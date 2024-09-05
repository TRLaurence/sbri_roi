# Define the sigmoid function for modeling uptake
sigmoid <- function(x) {
  1 / (1 + exp(-x))
}

# Define the function to extrapolate coverage over time
extrapolate_sigmoid <- function(init_coverage, final_coverage, years) {
  
  # Time points (years)
  time <- seq(0, years, by = 1)
  # Parameters for the sigmoid function
  midpoint <- years / 2  # Midpoint of the transition
  growth_rate <- 10 / years  # Growth rate for the transition curve (adjustable)
  
  # Calculate the normalized coverage using the sigmoid function
  norm_coverage <- sigmoid(growth_rate * (time - midpoint))
  # Rescale coverage to fit between init_coverage and final_coverage
  coverage_vec <- init_coverage + (final_coverage - init_coverage) * norm_coverage
  coverage_vec[1] <- init_coverage
  coverage_vec[years+1] <- final_coverage
  # Return a data frame with time and coverage
  data.frame(year = time, coverage = coverage_vec)
}

estimate_coverage <- function(init_coverage, final_coverage, years, year_for_final_coverage) {
  if (years > year_for_final_coverage) {
    initial_coverage_data <- extrapolate_sigmoid(init_coverage, final_coverage, year_for_final_coverage)
    remaining_years <- years - year_for_final_coverage
    remaining_coverage_data <- data.frame(year = seq(year_for_final_coverage+1, years, by = 1), 
                                          coverage = rep(final_coverage, remaining_years))
    coverage_data <- rbind(initial_coverage_data, remaining_coverage_data)
  } else {
    coverage_data <- extrapolate_sigmoid(init_coverage, final_coverage, year_for_final_coverage)
    coverage_data <- coverage_data[0:years+1, ]
  } 
  return(coverage_data)
}

# # Example usage:
# # Initial coverage of 10%, final coverage of 90%, over 10 years
# coverage_data <- estimate_coverage(0.1, 0.5, 10, 5)
# 
# # Print the result
# print(coverage_data)
