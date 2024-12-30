
#' Apply Discount to Value Columns in a Data Frame
#'
#' This function applies a discount rate to specified value columns in a data frame,
#' adjusting them based on their associated year columns and a target year. The discount
#' is applied as a compound discount over the difference in years between each entry's
#' year and the target year.
#'
#' @param df_val A data frame containing the value and year columns to be adjusted.
#' @param value_year_mapping A named character vector where the names are value column names
#'   (e.g., "healthcare_cost_savings_value") and the values are their corresponding year column names
#'   (e.g., "healthcare_costs_year").
#' @param target_year An integer representing the year to which the values are being adjusted.
#' @param discount_rate A numeric value representing the annual discount rate to be applied.
#'   Must be between 0 and 0.1 (inclusive).
#'
#' @return A data frame with the same structure as `df_val`, where the specified value columns
#'   have been adjusted for the given discount rate and target year.
#'
#' @examples
#' # Example input data
#' df_val <- data.frame(
#'   scenario = c("cost_medium", "cost_lower", "cost_upper"),
#'   healthcare_cost_savings_value = c(1000, 900, 1100),
#'   healthcare_costs_year = c(2020, 2020, 2020),
#'   socialcare_cost_savings_value = c(1000, 800, 1200),
#'   socialcare_costs_year = c(2020, 2020, 2020)
#' )
#'
#' value_year_mapping <- c(
#'   healthcare_cost_savings_value = "healthcare_costs_year",
#'   socialcare_cost_savings_value = "socialcare_costs_year"
#' )
#'
#' # Apply a 5% discount, adjusting to the year 2018
#' adjusted_df <- apply_discount(df_val, value_year_mapping, target_year = 2018, discount_rate = 0.05)
#' print(adjusted_df)
#'
#' @throws Error if:
#' - `discount_rate` is negative or greater than 0.1.
#' - Any column names specified in `value_year_mapping` are missing from `df_val`.
#'
#'
#' @export
apply_discount <- function(df_val, value_year_mapping, target_year, discount_rate) {
  stopifnot(discount_rate >= 0)
  stopifnot(discount_rate <= 0.1) # Discount rate should be between 0 and 10%
  stopifnot(all(value_year_mapping %in% names(df_val)))
  stopifnot(all(names(value_year_mapping) %in% names(df_val)))
  
  for (i in 1:length(value_year_mapping)) {
    value_name <- names(value_year_mapping)[i]
    year_name <- value_year_mapping[value_name]
    df_val[[value_name]] <- df_val[[value_name]] / (1 + discount_rate)^(df_val[[year_name]]-target_year)
  }
  
  return(df_val)
}


#' Apply inflation to a dataframe
#' @param val A numeric vector
#' @param cols_to_inflate A character vector of column names to inflate
#' @param inflation_df A dataframe with columns "year" and "gdp_deflator"
#' @param target_year The target year to inflate to
#' @return A dataframe with inflated columns
#' 
#' @examples
#' 
#' inflation_df <- data.frame(year = 2010:2020, gdp_deflator = seq(1, 1.1, length.out = 11))
#' val <- 100
#' inflated_df <- apply_inflation(val, inflation_df, 2010, 2020)
#' 
#' @export
apply_inflation <- function(val, inflation_df, cost_year, target_year) {
  if (!((cost_year %in% inflation_df$year) & (target_year %in% inflation_df$year))) {
    stop(paste0("Cost year or target year not found in inflation data."))
  }
  
  inflated_val <- val * (inflation_df$gdp_deflator[inflation_df$year == target_year] / inflation_df$gdp_deflator[inflation_df$year == cost_year])
  
  return(inflated_val)
}

#' Apply discount to a value
#' 
#' @param val A numeric value
#' @param discount_rate A numeric value representing the annual discount rate to be applied.
#' @param cost_year The year of the cost
#' @param target_year The target year to discount to
#' @return A numeric value with the discount applied
#' 
#' @examples
#' 
#' apply_discount_basic(100, 0.05, 2020, 2018)
#' 
#' @export
apply_discount_basic <- function(val, discount_rate, cost_year, target_year) {
  return(val / (1 + discount_rate)^(cost_year - target_year))
}

#' Standardise a dataframe to a target year
#' 
#' @param parameter_scenarios A dataframe with columns to be standardised
#' @param inflation_df A dataframe with columns "year" and "gdp_deflator"
#' @param value_year_mapping A named character vector where the names are value column names
#'  (e.g., "healthcare_cost_savings_value") and the values are their corresponding year column names
#'  (e.g., "healthcare_costs_year").
#' @param target_cost_year The target year to standardise to
#' @param discount_rate A numeric value representing the annual discount rate to be applied. We're not discounting health benefits here so use the cost discount rate
standardise_df_to_target <- function(parameter_scenarios, inflation_df, value_year_mapping, target_cost_year, discount_rate) {
  inflation_adjust_row <- function(row_val, inflation_df, value_year_mapping, target_cost_year) {
    for (i in 1:length(value_year_mapping)) {
      col_name <- names(value_year_mapping)[i]
      val <- row_val[[col_name]]
      observed_year <- row_val[[value_year_mapping[i]]]
      row_val[[col_name]] <- apply_inflation(val, inflation_df, cost_year = observed_year, target_year = target_cost_year)
    }
    return(row_val)
  }
  
  for (i in 1:nrow(parameter_scenarios)) {
    parameter_scenarios[i,] <- inflation_adjust_row(parameter_scenarios[i,], inflation_df, value_year_mapping, target_cost_year)
  }
  
  parameter_scenarios <- apply_discount(parameter_scenarios, value_year_mapping, target_cost_year, discount_rate)
  
  return(parameter_scenarios)
}

#' Create a mapping between value columns and year columns
#' 
#' @param parameter_scenarios A dataframe with columns representing different scenarios and values
#' @param n How many of the first characters of the ...value and ...year columns have to match to be valid
#' @return A named vector with the value columns as names and the year columns as values
#' 
#' @examples
#' 
#' parameter_scenarios <- data.frame(
#'  healthcare_costs_value = c(1000, 800, 1200),
#'  healthcare_costs_year = c(2020, 2020, 2020),
#'  socialcare_costs_value = c(2000, 1800, 2200),
#'  socialcare_costs_year = c(2020, 2020, 2020)
#'  )
#'
#' value_year_mapping <- create_value_year_mapping(parameter_scenarios)
#' 
#' @export
create_value_year_mapping <- function(parameter_scenarios, n = 10) {
  # Check for valid input
  stopifnot(is.data.frame(parameter_scenarios))
  
  # Filter columns matching cost or productivity
  cost_cols <- str_subset(colnames(parameter_scenarios), "cost|productivity")
  
  # Extract columns ending with "_value"
  value_cols <- str_subset(cost_cols, "_value")
  
  # Extract columns ending with "_year"
  year_cols <- str_subset(cost_cols, "_year")
  
  if (length(value_cols) != length(year_cols)) {
    stop("The number of value and year columns don't match, so there might be a mistake in the mapping.")
  }
  
    
  value_year_mapping <- setNames(year_cols, value_cols)


  # Ensure the first n characters of the names and values match
  if (!all(substr(names(value_year_mapping), 1, n) == substr(value_year_mapping, 1, n))) {
    stop("The first ", n, " characters of the year and value parameters don't match, so there might be a mistake in the mapping.")
  }
  
  return(value_year_mapping)
}

