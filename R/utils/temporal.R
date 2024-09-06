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
apply_inflation <- function(val, inflation_df, cost_year, target_year) {

  
  inflated_val <- val * (inflation_df$gdp_deflator[inflation_df$year == target_year] / inflation_df$gdp_deflator[inflation_df$year == cost_year])
  
  return(inflated_val)
}