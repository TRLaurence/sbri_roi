library(testthat)
source("R/analysis/framework_adjustments.R")

# Sample input data frames for testing
test_df1 <- data.frame(
  total = c(100, -50, 0, 150),
  mean = c(10, -5, 0, 15),
  benefit_name = c("qaly_gains_value", "healthcare_cost_savings_value", "socialcare_cost_savings_value", "productivity_gains_value")
)

test_df2 <- data.frame(
  total = c(200, 300, -100),
  mean = c(20, 30, -10),
  benefit_name = c("benefit1", "benefit2", "benefit3")
)

test_df3 <- data.frame(
  total = c(0, 0, 0),
  mean = c(0, 0, 0),
  benefit_name = c("zero1", "zero2", "zero3")
)

# Test cases
test_that("apply_optimism_bias_row adds correct optimism bias adjustment row", {
  # Apply optimism bias of 0.8 to test_df1
  result_df <- apply_optimism_bias_row(test_df1, optimism_bias = 0.8)
  print(result_df)
  
  # Expected total and mean adjustments
  expected_total_adjustment <- -sum(test_df1$total) * (1 - 0.8)
  expected_mean_adjustment <- -sum(test_df1$mean) * (1 - 0.8)
  
  # Check the last row for accuracy
  expect_equal(result_df$total[nrow(result_df)], expected_total_adjustment)
  expect_equal(result_df$mean[nrow(result_df)], expected_mean_adjustment)
  expect_equal(result_df$benefit_name[nrow(result_df)], "optimism_bias_adjustment")
})

test_that("apply_optimism_bias_row handles positive and negative totals correctly", {
  # Apply optimism bias of 0.9 to test_df2
  result_df <- apply_optimism_bias_row(test_df2, optimism_bias = 0.9)
  
  # Expected total and mean adjustments
  expected_total_adjustment <- -sum(test_df2$total) * (1 - 0.9)
  expected_mean_adjustment <- -sum(test_df2$mean) * (1 - 0.9)
  
  # Verify that the calculated values match expectations
  expect_equal(result_df$total[nrow(result_df)], expected_total_adjustment)
  expect_equal(result_df$mean[nrow(result_df)], expected_mean_adjustment)
  expect_equal(result_df$benefit_name[nrow(result_df)], "optimism_bias_adjustment")
})

test_that("apply_optimism_bias_row handles zero totals and means without error", {
  # Apply optimism bias of 0.5 to test_df3
  result_df <- apply_optimism_bias_row(test_df3, optimism_bias = 0.5)
  
  # Expected total and mean adjustments should be zero due to zero sums
  expected_total_adjustment <- -sum(test_df3$total) * (1 - 0.5)
  expected_mean_adjustment <- -sum(test_df3$mean) * (1 - 0.5)
  
  # Check if the last row is as expected
  expect_equal(result_df$total[nrow(result_df)], expected_total_adjustment)
  expect_equal(result_df$mean[nrow(result_df)], expected_mean_adjustment)
  expect_equal(result_df$benefit_name[nrow(result_df)], "optimism_bias_adjustment")
})

test_that("apply_optimism_bias_row returns an additional row", {
  initial_row_count <- nrow(test_df1)
  result_df <- apply_optimism_bias_row(test_df1, optimism_bias = 0.75)
  
  # Check that one row is added
  expect_equal(nrow(result_df), initial_row_count + 1)
})

test_that("apply_optimism_bias_row works with extreme optimism biases", {
  # Apply a high optimism bias close to 1 (e.g., 0.99)
  result_df_high <- apply_optimism_bias_row(test_df1, optimism_bias = 0.99)
  expected_total_high <- -sum(test_df1$total) * (1 - 0.99)
  expected_mean_high <- -sum(test_df1$mean) * (1 - 0.99)
  
  expect_equal(result_df_high$total[nrow(result_df_high)], expected_total_high)
  expect_equal(result_df_high$mean[nrow(result_df_high)], expected_mean_high)
  
  # Apply a low optimism bias close to 0 (e.g., 0.01)
  result_df_low <- apply_optimism_bias_row(test_df1, optimism_bias = 0.01)
  expected_total_low <- -sum(test_df1$total) * (1 - 0.01)
  expected_mean_low <- -sum(test_df1$mean) * (1 - 0.01)
  
  expect_equal(result_df_low$total[nrow(result_df_low)], expected_total_low)
  expect_equal(result_df_low$mean[nrow(result_df_low)], expected_mean_low)
})
