apply_discount <- function(df_val, cols_to_discount, discount_rate) {
  stopifnot(discount_rate >= 0)
  stopifnot(discount_rate <= 0.1) # Discount rate should be between 0 and 10%
  stopifnot("year" %in% names(df_val))
  
  for (col in cols_to_discount) {
    df_val[[paste0("discounted_",col)]] <- df_val[[col]] / (1 + discount_rate)^df_val$year
  }
  
  return(df_val)
}

data <- data.frame(year = 1:5, value = 100)
apply_discount(data, c("value"), 0.035) # 100 / (1 + 0.1)^5

#' Apply inflation to a dataframe
#' @param val A numeric vector
#' @param cols_to_inflate A character vector of column names to inflate
#' @param inflation_df A dataframe with columns "year" and "inflation_index"
#' @param target_year The target year to inflate to
#' @return A dataframe with inflated columns
apply_inflation <- function(val, inflation_df, cost_year, target_year) {
  stopifnot(inflation_rate >= 0)
  stopifnot(inflation_rate <= 0.1) # Inflation rate should be between 0 and 10%
  
  inflated_val <- val * (inflation_df$inflation_index[inflation_df$year == target_year] / inflation_df$inflation_index[inflation_df$year == cost_year])
  
  return(inflated_val)
}