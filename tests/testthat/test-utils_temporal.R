
library(testthat)
# Test: Valid Input
test_that("create_value_year_mapping returns correct mapping for valid input", {
  parameter_scenarios <- data.frame(
    cost_A_value = c(1, 2, 3),
    cost_A_year = c(2020, 2021, 2022),
    productivity_B_value = c(4, 5, 6),
    productivity_B_year = c(2020, 2021, 2022)
  )
  
  result <- create_value_year_mapping(parameter_scenarios, n = 5)
  
  expect_named(result, c("cost_A_value", "productivity_B_value"))
  expect_equal(result[["cost_A_value"]], "cost_A_year")
  expect_equal(result[["productivity_B_value"]], "productivity_B_year")
})

# Test: Unequal `_value` and `_year` Columns
test_that("create_value_year_mapping throws error for unequal value and year columns", {
  parameter_scenarios <- data.frame(
    cost_A_value = c(1, 2, 3),
    cost_B_value = c(2020, 2021, 2022)
  )
  
  expect_error(
    create_value_year_mapping(parameter_scenarios),
    "The number of value and year columns don't match"
  )
})

# Test: Invalid Data Frame
test_that("create_value_year_mapping throws error for invalid input type", {
  parameter_scenarios <- list(
    cost_A_value = c(1, 2, 3),
    cost_A_year = c(2020, 2021, 2022)
  )
  
  expect_error(
    create_value_year_mapping(parameter_scenarios),
    "is.data.frame"
  )
})

# Test: Mismatched First n Characters
test_that("create_value_year_mapping throws error for mismatched first n characters", {
  parameter_scenarios <- data.frame(
    cost_A_value = c(1, 2, 3),
    productivity_B_year = c(2020, 2021, 2022)
  )
  
  expect_error(
    create_value_year_mapping(parameter_scenarios, n = 5),
    "The first 5 characters of the year and value parameters don't match"
  )
})

# Test: No Matching Columns
test_that("create_value_year_mapping returns empty mapping for no matching columns", {
  parameter_scenarios <- data.frame(
    unrelated_col1 = c(1, 2, 3),
    unrelated_col2 = c(2020, 2021, 2022)
  )
  
  result <- create_value_year_mapping(parameter_scenarios)
  
  expect_equal(length(result), 0)
})

# Test: Default n Value
test_that("create_value_year_mapping works with default n value", {
  parameter_scenarios <- data.frame(
    health_care_cost_value = c(1, 2, 3),
    health_care_cost_year = c(2020, 2021, 2022)
  )
  
  result <- create_value_year_mapping(parameter_scenarios)
  
  expect_equal(result[["health_care_cost_value"]], "health_care_cost_year")
})


# Test: Valid Input
test_that("apply_discount correctly applies discount for valid input", {
  # Example input data frame
  df_val <- data.frame(
    healthcare_cost_savings_value = c(1000),
    healthcare_costs_year = c(2020),
    socialcare_cost_savings_value = c(500),
    socialcare_costs_year = c(2020),
    productivity_gains_value = c(200),
    productivity_gains_year = c(2020)
  )
  
  value_year_mapping <- c(
    healthcare_cost_savings_value = "healthcare_costs_year",
    socialcare_cost_savings_value = "socialcare_costs_year",
    productivity_gains_value = "productivity_gains_year"
  )
  
  target_year <- 2019
  discount_rate <- 0.05
  
  # Apply the discount
  result <- apply_discount(df_val, value_year_mapping, target_year, discount_rate)
  
  # Expected output
  expected_output <- data.frame(
    healthcare_cost_savings_value = c(1000 / (1 + discount_rate)),
    healthcare_costs_year = c(2020),
    socialcare_cost_savings_value = c(500 / (1 + discount_rate)),
    socialcare_costs_year = c(2020),
    productivity_gains_value = c(200 / (1 + discount_rate)),
    productivity_gains_year = c(2020)
  )
  
  expect_equal(result, expected_output)
  
})

# Test: Invalid Discount Rate (too high)
test_that("apply_discount throws an error for invalid discount rate (too high)", {
  df_val <- data.frame(
    healthcare_cost_savings_value = c(1000, 2000, 3000),
    healthcare_costs_year = c(2020, 2021, 2022)
  )
  
  value_year_mapping <- c(healthcare_cost_savings_value = "healthcare_costs_year")
  
  expect_error(
    apply_discount(df_val, value_year_mapping, 2020, discount_rate = 0.15),
    "discount_rate <= 0.1"
  )
})

# Test: Invalid Discount Rate (negative)
test_that("apply_discount throws an error for invalid discount rate (negative)", {
  df_val <- data.frame(
    healthcare_cost_savings_value = c(1000, 2000, 3000),
    healthcare_costs_year = c(2020, 2021, 2022)
  )
  
  value_year_mapping <- c(healthcare_cost_savings_value = "healthcare_costs_year")
  
  expect_error(
    apply_discount(df_val, value_year_mapping, 2020, discount_rate = -0.01),
    "discount_rate >= 0"
  )
})

# Test: Missing Columns in Data Frame
test_that("apply_discount throws an error for missing columns in data frame", {
  df_val <- data.frame(
    healthcare_cost_savings_value = c(1000, 2000, 3000)
    # Missing healthcare_costs_year column
  )
  
  value_year_mapping <- c(healthcare_cost_savings_value = "healthcare_costs_year")
  
  expect_error(
    apply_discount(df_val, value_year_mapping, 2020, discount_rate = 0.05),
    regexp = "all\\(value_year_mapping %in% names\\(df_val\\)\\) is not TRUE"
  )
})


# Test: No Discount (discount_rate = 0)
test_that("apply_discount works correctly with no discount (discount_rate = 0)", {
  df_val <- data.frame(
    healthcare_cost_savings_value = c(1000, 2000, 3000),
    healthcare_costs_year = c(2020, 2021, 2022)
  )
  
  value_year_mapping <- c(healthcare_cost_savings_value = "healthcare_costs_year")
  
  result <- apply_discount(df_val, value_year_mapping, 2020, discount_rate = 0)
  
  expect_equal(result$healthcare_cost_savings_value, c(1000, 2000, 3000))
})

# Test: Target Year in Future
test_that("apply_discount works correctly with a target year in the future", {
  df_val <- data.frame(
    healthcare_cost_savings_value = c(1000, 2000, 3000),
    healthcare_costs_year = c(2020, 2021, 2022)
  )
  
  value_year_mapping <- c(healthcare_cost_savings_value = "healthcare_costs_year")
  
  result <- apply_discount(df_val, value_year_mapping, 2023, discount_rate = 0.05)
  
  # Expected values
  expect_equal(
    round(result$healthcare_cost_savings_value, 2),
    round(c(1000/(1.05)^-3, 2000/(1.05)^-2, 3000/(1.05)^-1), 2)
  )
})

# Test: Valid Input
test_that("apply_inflation correctly inflates a single value for valid input", {
  # Example input values and inflation data
  val <- 1000
  inflation_df <- data.frame(
    year = c(2020, 2021, 2022),
    gdp_deflator = c(100, 105, 110)
  )
  cost_year <- 2020
  target_year <- 2022
  
  # Apply inflation
  result <- apply_inflation(val, inflation_df, cost_year, target_year)
  
  # Expected result
  expected <- 1000 * (110 / 100)
  expect_equal(result, expected)
})

# Test: No Inflation Needed (Target Year = Cost Year)
test_that("apply_inflation returns the original value when target year equals cost year", {
  val <- 1000
  inflation_df <- data.frame(
    year = c(2020, 2021, 2022),
    gdp_deflator = c(100, 105, 110)
  )
  cost_year <- 2020
  target_year <- 2020
  
  # Apply inflation
  result <- apply_inflation(val, inflation_df, cost_year, target_year)
  
  # Expected result
  expect_equal(result, val)
})

test_that("apply_inflation throws an error when target year is missing in inflation data", {
  val <- 1000
  inflation_df <- data.frame(
    year = c(2020, 2021), # Missing 2022
    gdp_deflator = c(100, 105)
  )
  cost_year <- 2020
  target_year <- 2022
  
  # Expect an error
  expect_error(
    apply_inflation(val, inflation_df, cost_year, target_year),
    regexp = "Cost year or target year not found in inflation data."
  )
})


test_that("apply_inflation throws an error when target year is missing in inflation data", {
  val <- 1000
  inflation_df <- data.frame(
    year = c(2020, 2021), # Missing 2022
    gdp_deflator = c(100, 105)
  )
  cost_year <- 2020
  target_year <- 2022
  
  # Expect an error
  expect_error(
    apply_inflation(val, inflation_df, cost_year, target_year),
    regexp = "Cost year or target year not found in inflation data."
  )
})

test_that("apply_inflation throws an error when cost year is missing in inflation data", {
  val <- 1000
  inflation_df <- data.frame(
    year = c(2021, 2022), # Missing 2020
    gdp_deflator = c(105, 110)
  )
  cost_year <- 2020
  target_year <- 2022
  
  # Expect an error
  expect_error(
    apply_inflation(val, inflation_df, cost_year, target_year),
    regexp = "Cost year or target year not found in inflation data."
  )
})

test_that("apply_discount_basic works for the three main cases", {
  # Example input values and discount data
  val <- 1000
  discount_rate <- 0.05
  cost_year <- 2020
  target_year <- 2022

  # Apply discount
  result <- apply_discount_basic(val, discount_rate, cost_year, target_year)
  
  # Expected result
  expected <- 1000 / (1 + 0.05)^(-2)
  expect_equal(result, expected)
  
  # Reverse cost year and target year
  
  cost_year <- 2022
  target_year <- 2020
  
  # Apply discount
  result <- apply_discount_basic(val, discount_rate, cost_year, target_year)
  
  # Expected result
  expected <- 1000 / (1 + 0.05)^2
  expect_equal(result, expected)
  
  # Cost year equals target year
  cost_year <- 2020
  target_year <- 2020
  
  # Apply discount
  result <- apply_discount_basic(val, discount_rate, cost_year, target_year)
  
  expect_equal(result, val)
})

test_that("Test the overall function that adjusts costs to the right time period", {
  parameter_scenarios <- data.frame(
    scenario = c("A", "B"),
    healthcare_cost_savings_value = c(1000),
    healthcare_costs_year = c(2019),
    productivity_gains_value = c(2000),
    productivity_gains_year = c(2018)
  )
  
  inflation_df <- data.frame(
    year = c(2018, 2019, 2020),
    gdp_deflator = c(100, 102, 105)
  )
  
  value_year_mapping <- c(
    healthcare_cost_savings_value = "healthcare_costs_year",
    productivity_gains_value = "productivity_gains_year"
  )
  
  target_cost_year <- 2020
  
  discount_rate <- 0.05
  
  result <- standardise_df_to_target(parameter_scenarios, inflation_df, value_year_mapping, target_cost_year, discount_rate)
  
  # Expected result
  
  expected <- data.frame(
    scenario = c("A", "B"),
    healthcare_cost_savings_value = 1000 * (105 / 102) / (1 + 0.05)^(-1),
    healthcare_costs_year = 2019,
    productivity_gains_value = 2000 * (105 / 100) / (1 + 0.05)^(-2),
    productivity_gains_year = 2018
  )
  
  expect_equal(result, expected)  
})


