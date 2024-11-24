library(testthat)
library(dplyr)
library(tidyr)
# source("R/analysis_coverage.R")



####### TEST SIGMOID ########

# Test: No years specified (edge case)
test_that("No years specified", {
  result <- extrapolate_sigmoid(init_coverage = 0.1, final_coverage = 0.9, years = 0)
  expect_equal(nrow(result), 0)
  expect_named(result, c("year", "coverage"))
})

# Test: One year specified (minimum viable case)
test_that("One year specified", {
  result <- extrapolate_sigmoid(init_coverage = 0.1, final_coverage = 0.9, years = 1)
  expect_equal(nrow(result), 1)
  expect_equal(result$coverage[1], 0.1)
})

# Test: Basic sigmoid transition over multiple years
test_that("Sigmoid transition over multiple years", {
  years <- 10
  init_coverage <- 0.2
  final_coverage <- 0.8
  result <- extrapolate_sigmoid(init_coverage, final_coverage, years)
  
  expect_equal(nrow(result), years)
  expect_equal(result$coverage[1], init_coverage)
  expect_equal(result$coverage[years], final_coverage)
  
  # Check that the coverage increases monotonically (no backward steps)
  expect_true(all(diff(result$coverage) >= 0))
})

# Test: Midpoint check
test_that("Sigmoid function approaches midpoint", {
  years <- 10
  init_coverage <- 0.3
  final_coverage <- 0.9
  result <- extrapolate_sigmoid(init_coverage, final_coverage, years)
  midpoint_coverage <- result$coverage[round(years / 2) + 1]
  
  # Midpoint coverage should be around halfway between init_coverage and final_coverage
  expect_true(midpoint_coverage > init_coverage && midpoint_coverage < final_coverage)
  expect_true(midpoint_coverage > (init_coverage + final_coverage) / 2 - 0.1)
  expect_true(midpoint_coverage < (init_coverage + final_coverage) / 2 + 0.1)
})

# Test: Large number of years
test_that("Function handles large number of years gracefully", {
  years <- 100
  init_coverage <- 0.1
  final_coverage <- 1.0
  result <- extrapolate_sigmoid(init_coverage, final_coverage, years)
  
  expect_equal(nrow(result), years)
  expect_equal(result$coverage[1], init_coverage)
  expect_equal(result$coverage[years], final_coverage)
  
  # Check for monotonic increase
  expect_true(all(diff(result$coverage) >= 0))
})

# Test: Decreasing coverage (final_coverage < init_coverage)
test_that("Decreasing coverage scenario", {
  years <- 10
  init_coverage <- 0.9
  final_coverage <- 0.1
  result <- extrapolate_sigmoid(init_coverage, final_coverage, years)
  
  expect_equal(result$coverage[1], init_coverage)
  expect_equal(result$coverage[years], final_coverage)
  
  # Check for monotonic decrease
  expect_true(all(diff(result$coverage) <= 0))
})

# Test: Check for correct names of output dataframe
test_that("Output dataframe has correct names", {
  result <- extrapolate_sigmoid(0.2, 0.8, 10)
  expect_named(result, c("year", "coverage"))
})


####### TEST COVERAGE ########

coverage_data_1 <- estimate_coverage(0.1, 0.2, 2015,  2015, 1)
expected_coverage_1 <- data.frame(year = 2015, coverage = 0.1)

test_that("Test 1 element", {
  expect_equal(
    coverage_data_1, 
    expected_coverage_1
  )
})

coverage_data_2 <- estimate_coverage(0.1, 0.2, 2015,  2016, 2)

expected_coverage_2 <- data.frame(year = c(2015, 2016), coverage = c(0.1, 0.2))

test_that("Test 2 elements", {
  expect_equal(
    coverage_data_2, 
    expected_coverage_2
  )
})


coverage_data_3 <- estimate_coverage(0.1, 0.5, 2015,  2020, 10)
expected_coverage_3 <- data.frame(year = c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024), 
                                  coverage = c(0.1, NA, NA, NA, NA, 0.5, 0.5, 0.5, 0.5, 0.5))

final_coverage_3 <- unique(filter(coverage_data_3, year >= 2020)$coverage)
initial_coverage_3 <- unique(filter(coverage_data_3, year == 2015)$coverage)

test_that("Coverage 3 final uptake", {
  expect_equal(
    final_coverage_3,
    0.5
  )
})

test_that("Coverage 3 initial uptake", {
  expect_equal(
    initial_coverage_3,
    0.1
  )
})

test_that("Coverage 3 max year", {
  expect_equal(
    max(coverage_data_3$year), 
    2024
  )
})


test_that("Coverage 3 max year", {
  expect_equal(
    max(table(coverage_data_3$year)), 
    1
  )
})

test_that("Coverage 3 values in range  max", {
  expect_equal(
    max(coverage_data_3$coverage), 
    0.5
  )
})

test_that("Coverage 3 values in range  min", {
  expect_equal(
    min(coverage_data_3$coverage), 
    0.1
  )
})


coverage_data_4 <- estimate_coverage(0.1, 0.5, 2015,  2034, 11)


test_that("Coverage 4 final uptake", {
  expect_equal(
    coverage_data_4$coverage[length(coverage_data_4$coverage)],
    0.3
  )
})

test_that("Coverage 4 initial uptake", {
  expect_equal(
    coverage_data_4$coverage[1],
    0.1
  )
})

test_that("Coverage 4 max year", {
  expect_equal(
    max(coverage_data_4$year), 
    2025
  )
})


test_that("Coverage 4 max year count", {
  expect_equal(
    max(table(coverage_data_4$year)), 
    1
  )
})

test_that("Coverage 4 values in range  max", {
  expect_equal(
    max(coverage_data_4$coverage), 
    0.3
  )
})

test_that("Coverage 4 values in range  min", {
  expect_equal(
    min(coverage_data_4$coverage), 
    0.1
  )
})


# Define a test case where the input matches a simple example
test_that("coverage_adjust_row replicates single year functionality", {
  row <- list(
    coverage_init_value = 0.1,
    coverage_end_value = 0.2,
    coverage_init_year = 2015,
    coverage_end_year = 2015
  )
  years_of_coverage <- 1
  
  result <- coverage_adjust_row(row, years_of_coverage)
  expected <- data.frame(year = 2015, coverage = 0.1)
  
  expect_equal(result, expected)
})

# Define a test case where the input spans multiple years
test_that("coverage_adjust_row replicates multi-year functionality", {
  row <- list(
    coverage_init_value = 0.1,
    coverage_end_value = 0.2,
    coverage_init_year = 2015,
    coverage_end_year = 2016
  )
  years_of_coverage <- 2
  
  result <- coverage_adjust_row(row, years_of_coverage)
  expected <- data.frame(year = c(2015, 2016), coverage = c(0.1, 0.2))
  
  expect_equal(result, expected)
})