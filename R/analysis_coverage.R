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

#' @title Estimate Coverage Over Time
#' @description This function estimates the coverage over a specified number of years using a sigmoid function. 
#'              It interpolates between an initial and final coverage value and can extrapolate coverage values 
#'              beyond the specified sigmoid range if needed.
#' 
#' @param init_coverage Numeric - Initial coverage value (proportion between 0 and 1).
#' @param final_coverage Numeric - Final coverage value (proportion between 0 and 1).
#' @param year_start_coverage Integer - Starting year corresponding to the initial coverage value.
#' @param year_final_coverage Integer - Target year when the final coverage value will be reached.
#' @param n_years Integer - Total number of years to estimate coverage for. If this exceeds the duration of 
#'                          the sigmoid range (year_final_coverage - year_start_coverage + 1), the function 
#'                          extrapolates coverage for the remaining years.
#' 
#' @return A data frame with two columns:
#' \itemize{
#'   \item \code{year}: Integer - The year for each coverage estimate.
#'   \item \code{coverage}: Numeric - The estimated coverage value for the corresponding year.
#' }
#' 
#' @details 
#' The function first calculates coverage within the sigmoid range defined by \code{year_start_coverage} 
#' and \code{year_final_coverage}. If \code{n_years} exceeds this range, coverage values for the remaining 
#' years are extrapolated as either constant at the final coverage value or using alternative rules 
#' depending on the implementation.
#' 
#' @examples
#' # Example 1: Estimate coverage from 2020 to 2024 with a sigmoid transition from 0.1 to 0.5
#' estimate_coverage(0.1, 0.5, 2020, 2024, 5)
#' # Output:
#' #   year   coverage
#' # 1 2020 0.1000000
#' # 2 2021 0.1189703
#' # 3 2022 0.2075766
#' # 4 2023 0.3924234
#' # 5 2024 0.5000000
#'
#' # Example 2: Estimate coverage from 2020 to 2029 with a sigmoid transition from 0.1 to 0.5 
#' #            and extrapolated coverage for additional years
#' estimate_coverage(0.1, 0.5, 2020, 2024, 10)
#' # Output:
#' #   year   coverage
#' # 1  2020 0.1000000
#' # 2  2021 0.1189703
#' # 3  2022 0.2075766
#' # 4  2023 0.3924234
#' # 5  2024 0.5000000
#' # 6  2025 0.5000000
#' # 7  2026 0.5000000
#' # 8  2027 0.5000000
#' # 9  2028 0.5000000
#' # 10 2029 0.5000000
#' 
#' @export
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

#' @title Adjust Coverage for a Single Row
#' @description A helper function to estimate coverage over time for a single row of input data using the 
#'              \code{estimate_coverage} function.
#'
#' @param row_val List - A list or named vector containing the following elements:
#'   \itemize{
#'     \item \code{coverage_init_value}: Numeric - Initial coverage value.
#'     \item \code{coverage_end_value}: Numeric - Final coverage value.
#'     \item \code{coverage_init_year}: Integer - Year corresponding to the initial coverage value.
#'     \item \code{coverage_end_year}: Integer - Year when the final coverage value is achieved.
#'   }
#' @param years_of_coverage Integer - Number of years for which coverage is to be estimated.
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item \code{year}: Integer - Year for each estimated coverage value.
#'     \item \code{coverage}: Numeric - Estimated coverage value for each year.
#'   }
#'
#' @examples
#' row <- list(
#'   coverage_init_value = 0.1,
#'   coverage_end_value = 0.5,
#'   coverage_init_year = 2020,
#'   coverage_end_year = 2024
#' )
#' coverage_adjust_row(row, years_of_coverage = 10)
#'
#' @export
coverage_adjust_row <- function(row_val, years_of_coverage) {
  init_coverage <- row_val[["coverage_init_value"]]
  final_coverage <- row_val[["coverage_end_value"]]
  year_start_coverage <- row_val[["coverage_init_year"]]
  year_final_coverage <- row_val[["coverage_end_year"]]
  coverage_df <- estimate_coverage(init_coverage, final_coverage, year_start_coverage, year_final_coverage, n_years = years_of_coverage)
  
  return(coverage_df)
}

