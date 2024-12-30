

library(testthat)

test_that("estimate_benefitting_pop works as expected one row", {
  coverage_df <- data.frame(year = c(2015), coverage = c(0.5))
  sub_population_adjustment <- 0.5
  initial_population <- 1000
  expected_benefitting_pop <- data.frame(year = c(2015), 
                                         coverage = c(0.5), 
                                         init_pop = c(1000), 
                                         sub_pop_adjust = c(0.5), 
                                         target_pop = c(500), 
                                         benefitting_pop = c(250))
  expect_equal(
    estimate_benefitting_pop(initial_population, sub_population_adjustment, coverage_df), 
    expected_benefitting_pop
  )
})


test_that("estimate_benefitting_pop works as expected multi row", {
  coverage_df <- data.frame(year = c(2015, 2016), coverage = c(0.1, 0.2))
  sub_population_adjustment <- 0.5
  initial_population <- 1000
  expected_benefitting_pop <- data.frame(year = c(2015, 2016), 
                                         coverage = c(0.1, 0.2), 
                                         init_pop = c(1000, 1000), 
                                         sub_pop_adjust = c(0.5, 0.5), 
                                         target_pop = c(500, 500), 
                                         benefitting_pop = c(50, 100))
  expect_equal(
    estimate_benefitting_pop(initial_population, sub_population_adjustment, coverage_df), 
    expected_benefitting_pop
  )
})


add_discounted_benefit_col_df <- function(benefitting_pop_df, benefit_value, benefit_name, discount_rate, target_cost_year) {
  discounted_benefit_df <- benefitting_pop_df %>%
    mutate(discounted_benefit = benefit_value / ((1 + discount_rate) ^ (year - target_cost_year))) %>%
    mutate(!!sym(benefit_name) := discounted_benefit * benefitting_pop) %>%
    select(-discounted_benefit)
  
  return(discounted_benefit_df)
}
test_that("add_discounted_benefit_col_df works as expected", {
  benefitting_pop_df <- data.frame(year = c(2015, 2016), 
                           coverage = c(1, 1), 
                           init_pop = c(1, 1), 
                           sub_pop_adjust = c(1, 1), 
                           target_pop = c(1, 1), 
                           benefitting_pop = c(1, 1))
  benefit_value <- 10
  benefit_name <- "benefit_example"
  discount_rate <- 0.05
  target_cost_year <- 2015
  expected_discounted_benefit_df <- data.frame(year = c(2015, 2016), 
                                               coverage = c(1, 1), 
                                               init_pop = c(1, 1), 
                                               sub_pop_adjust = c(1, 1), 
                                               target_pop = c(1, 1), 
                                               benefitting_pop = c(1, 1), 
                                               benefit_example = c(10, 10/(1.05)))
  expect_equal(
    add_discounted_benefit_col_df(benefitting_pop_df, benefit_value, benefit_name, discount_rate, target_cost_year), 
    expected_discounted_benefit_df
  )
})

test_that("add_discounted_benefit_col_df works as expected several rows", {
  benefitting_pop_df <- data.frame(year = c(2015, 2016, 2017, 2018), 
                                   coverage = c(1, 1 ,1, 1), 
                                   init_pop = c(1, 1 ,1, 1),
                                   sub_pop_adjust = c(1, 1, 1, 1), 
                                   target_pop = c(1, 1, 1, 1),
                                   benefitting_pop = c(1, 1, 1, 1))
  benefit_value <- 10
  benefit_name <- "benefit_example"
  discount_rate <- 0.05
  target_cost_year <- 2015
  expected_discounted_benefit_df <- data.frame(year = c(2015, 2016, 2017, 2018), 
                                               coverage = c(1, 1 ,1, 1), 
                                               init_pop = c(1, 1 ,1, 1),
                                               sub_pop_adjust = c(1, 1, 1, 1), 
                                               target_pop = c(1, 1, 1, 1),
                                               benefitting_pop = c(1, 1, 1, 1), 
                                               benefit_example = c(10, 10/(1.05), 10/(1.05^2), 10/(1.05^3)))
  expect_equal(
    add_discounted_benefit_col_df(benefitting_pop_df, benefit_value, benefit_name, discount_rate, target_cost_year), 
    expected_discounted_benefit_df
  )
})

test_that("add_discounted_benefit_col_df works year out of range", {
  benefitting_pop_df <- data.frame(year = c(2015, 2016, 2017, 2018), 
                                   coverage = c(1, 1 ,1, 1), 
                                   init_pop = c(1, 1 ,1, 1),
                                   sub_pop_adjust = c(1, 1, 1, 1), 
                                   target_pop = c(1, 1, 1, 1),
                                   benefitting_pop = c(1, 1, 1, 1))
  benefit_value <- 10
  benefit_name <- "benefit_example"
  discount_rate <- 0.05
  target_cost_year <- 2015
  expected_discounted_benefit_df <- data.frame(year = c(2015, 2016, 2017, 2018), 
                                               coverage = c(1, 1 ,1, 1), 
                                               init_pop = c(1, 1 ,1, 1),
                                               sub_pop_adjust = c(1, 1, 1, 1), 
                                               target_pop = c(1, 1, 1, 1),
                                               benefitting_pop = c(1, 1, 1, 1), 
                                               benefit_example = c(10, 10/(1.05), 10/(1.05^2), 10/(1.05^3)))
  expect_equal(
    add_discounted_benefit_col_df(benefitting_pop_df, benefit_value, benefit_name, discount_rate, target_cost_year), 
    expected_discounted_benefit_df
  )
})

test_that("add_discounted_benefit_col_df handles zero discount rate correctly", {
  benefitting_pop_df <- data.frame(year = c(2015, 2016, 2017), 
                                   coverage = c(1, 1 ,1), 
                                   init_pop = c(1, 1 ,1),
                                   sub_pop_adjust = c(1, 1, 1), 
                                   target_pop = c(1, 1, 1),
                                   benefitting_pop = c(1, 1, 1))
  benefit_value <- 20
  benefit_name <- "no_discount_benefit"
  discount_rate <- 0
  target_cost_year <- 2015
  expected_discounted_benefit_df <- data.frame(year = c(2015, 2016, 2017), 
                                               coverage = c(1, 1 ,1), 
                                               init_pop = c(1, 1 ,1),
                                               sub_pop_adjust = c(1, 1, 1), 
                                               target_pop = c(1, 1, 1),
                                               benefitting_pop = c(1, 1, 1), 
                                               no_discount_benefit = c(20, 20, 20))
  expect_equal(
    add_discounted_benefit_col_df(benefitting_pop_df, benefit_value, benefit_name, discount_rate, target_cost_year), 
    expected_discounted_benefit_df
  )
})

test_that("add_discounted_benefit_col_df handles non-sequential years correctly", {
  benefitting_pop_df <- data.frame(year = c(2015, 2017, 2018, 2020), 
                                   coverage = c(1, 1 ,1, 1), 
                                   init_pop = c(1, 1 ,1, 1),
                                   sub_pop_adjust = c(1, 1, 1, 1), 
                                   target_pop = c(1, 1, 1, 1),
                                   benefitting_pop = c(1, 1, 1, 1))
  benefit_value <- 25
  benefit_name <- "non_sequential_benefit"
  discount_rate <- 0.04
  target_cost_year <- 2015
  expected_discounted_benefit_df <- data.frame(year = c(2015, 2017, 2018, 2020), 
                                               coverage = c(1, 1 ,1, 1), 
                                               init_pop = c(1, 1 ,1, 1),
                                               sub_pop_adjust = c(1, 1, 1, 1), 
                                               target_pop = c(1, 1, 1, 1),
                                               benefitting_pop = c(1, 1, 1, 1), 
                                               non_sequential_benefit = c(25, 25 / (1.04^2), 25 / (1.04^3), 25 / (1.04^5)))
  expect_equal(
    add_discounted_benefit_col_df(benefitting_pop_df, benefit_value, benefit_name, discount_rate, target_cost_year), 
    expected_discounted_benefit_df
  )
})

test_that("add_discounted_benefit_col_df works when target cost year is out of range with two years", {
  benefitting_pop_df <- data.frame(year = c(2015, 2016), 
                                   coverage = c(1, 1), 
                                   init_pop = c(1, 1),
                                   sub_pop_adjust = c(1, 1), 
                                   target_pop = c(1, 1),
                                   benefitting_pop = c(1, 1))
  benefit_value <- 10
  benefit_name <- "benefit_example"
  discount_rate <- 0.05
  target_cost_year <- 2025
  expected_discounted_benefit_df <- data.frame(year = c(2015, 2016), 
                                               coverage = c(1, 1), 
                                               init_pop = c(1, 1),
                                               sub_pop_adjust = c(1, 1), 
                                               target_pop = c(1, 1),
                                               benefitting_pop = c(1, 1), 
                                               benefit_example = c(10 * (1.05^10), 
                                                                   10 * (1.05^9)))
  expect_equal(
    add_discounted_benefit_col_df(benefitting_pop_df, benefit_value, benefit_name, discount_rate, target_cost_year), 
    expected_discounted_benefit_df
  )
})


test_that("add_optimism_bias_column works with positive optimism bias", {
  granular_benefits_df <- data.frame(
    qaly_gains_value = c(10, 20, 30),
    healthcare_cost_savings_value = c(5, 10, 15),
    socialcare_cost_savings_value = c(2, 4, 6),
    productivity_gains_value = c(3, 6, 9)
  )
  optimism_bias <- 0.8
  expected_granular_benefits_df <- data.frame(
    qaly_gains_value = c(10, 20, 30),
    healthcare_cost_savings_value = c(5, 10, 15),
    socialcare_cost_savings_value = c(2, 4, 6),
    productivity_gains_value = c(3, 6, 9),
    optimism_bias_adjustment = c(-4, -8, -12)
  )
  expect_equal(
    add_optimism_bias_column(granular_benefits_df, optimism_bias),
    expected_granular_benefits_df
  )
})

test_that("create_granular_benefits with a single row", {
  row_val <- data.frame(
    qaly_gains_value = c(1),
    healthcare_savings_value = c(500),
    optimism_bias_benefits_value = 0.8
  )
  
  cost_discount_rate <- 0.035
  health_discount_rate <- 0.015
  
  monetary_qaly <- 70000
  target_cost_year <- 2015
  
  benefitting_pop_df <- data.frame(
    year = c(2015),
    coverage = c(1),
    init_pop = c(100),
    sub_pop_adjust = c(1),
    target_pop = c(100),
    benefitting_pop = c(100)
  )
  
  expected_granular_benefits_df <- data.frame(
    year = c(2015),
    coverage = c(1),
    init_pop = c(100),
    sub_pop_adjust = c(1),
    target_pop = c(100),
    benefitting_pop = c(100),
    qaly_gains = c(70000*100),
    healthcare_savings = c(500*100),
    optimism_bias_adjustment = - 14100*100
  )
  
  resulting_granular_bias_df <- create_granular_benefits_df(row_val, benefitting_pop_df, health_discount_rate, cost_discount_rate, monetary_qaly, target_cost_year)
  
  expect_equal(resulting_granular_bias_df, expected_granular_benefits_df)
})

test_that("create_granular_benefits with a two rows", {
  row_val <- data.frame(
    qaly_gains_value = c(1),
    healthcare_savings_value = c(500),
    optimism_bias_benefits_value = 0.8
  )
  
  cost_discount_rate <- 0.035
  health_discount_rate <- 0.015
  
  monetary_qaly <- 70000
  target_cost_year <- 2015
  
  benefitting_pop_df <- data.frame(
    year = c(2015, 2016),
    coverage = c(1, 1),
    init_pop = c(100, 100),
    sub_pop_adjust = c(1, 1),
    target_pop = c(100, 100),
    benefitting_pop = c(100, 100)
  )
  
  expected_granular_benefits_df <- data.frame(
    year = c(2015, 2016),
    coverage = c(1, 1),
    init_pop = c(100, 100),
    sub_pop_adjust = c(1, 1),
    target_pop = c(100, 100),
    benefitting_pop = c(100, 100),
    qaly_gains = c(70000*100, 70000*100/(1+health_discount_rate)),
    healthcare_savings = c(500*100, 500*100/(1+cost_discount_rate)),
    optimism_bias_adjustment = c(- 14100*100, - 14000*100/(1+health_discount_rate) - 100*100/(1+cost_discount_rate))
  )
  
  resulting_granular_bias_df <- create_granular_benefits_df(row_val, benefitting_pop_df, health_discount_rate, cost_discount_rate, monetary_qaly, target_cost_year)
  
  expect_equal(resulting_granular_bias_df, expected_granular_benefits_df)
})

test_that("Test that the overall pipeline works", {
  # Define input data
  parameter_scenarios <- data.frame(
    initial_population_value = c(1000, 1000),
    adjustment_for_sub_population_value = c(0.5, 0.5),
    coverage_init_value = c(0.1, 0.1),
    coverage_end_value = c(0.1, 0.5),
    coverage_init_year = c(2020, 2020),
    coverage_end_year = c(2021, 2021),
    qaly_gains_value = c(0.1, 0.1),
    healthcare_cost_savings_value = c(1000, 2000),
    optimism_bias_benefits_value = c(1, 0.9),
    case_study_number = c(1, 1),
    scenario = c("Scenario A", "Scenario B")
  )
  
  # Set additional parameters
  health_discount_rate <- 0.035
  cost_discount_rate <- 0.015
  monetary_qaly <- 70000
  target_cost_year <- 2020
  years_of_coverage <- 2
  
  # Define expected output
  expected_output_scenario_A <- data.frame(
    year = c(2020, 2021),
    coverage = c(0.1, 0.1),
    init_pop = c(1000, 1000),
    sub_pop_adjust = c(0.5, 0.5),
    target_pop = c(500, 500),
    benefitting_pop = c(50, 50),
    qaly_gains = c(0.1*70000*50, 0.1*70000*50/(1+health_discount_rate)),
    healthcare_cost_savings = c(1000*50, 1000*50/(1+cost_discount_rate)),
    optimism_bias_adjustment = c(0,0),
    case_study = c(1, 1),
    scenario = c("Scenario A", "Scenario A")
  )
  
  # Define expected output
  expected_output_scenario_B <- data.frame(
    year = c(2020, 2021),
    coverage = c(0.1, 0.5),
    init_pop = c(1000, 1000),
    sub_pop_adjust = c(0.5, 0.5),
    target_pop = c(500, 500),
    benefitting_pop = c(50, 250),
    qaly_gains = c(0.1*70000*50, 0.1*70000*250/(1+health_discount_rate)),
    healthcare_cost_savings = c(2000*50, 2000*250/(1+cost_discount_rate)),
    optimism_bias_adjustment = c(- 35000 - 200*50, - 175000/(1+health_discount_rate) - 200*250/(1+cost_discount_rate)),
    case_study = c(1, 1),
    scenario = c("Scenario B", "Scenario B")
  )
  
  expected_return_all <- bind_rows(
    expected_output_scenario_A,
    expected_output_scenario_B
  )
  
  return_value <- benefits_for_all_rows(parameter_scenarios, health_discount_rate, cost_discount_rate, monetary_qaly, target_cost_year, years_of_coverage)
  
  expect_equal(
    return_value,
    expected_return_all
  )
  
})

test_that("Test that changing modelling period feeds through pipeline works", {
  # Define input data
  parameter_scenarios <- data.frame(
    initial_population_value = c(1000, 1000),
    adjustment_for_sub_population_value = c(0.5, 0.5),
    coverage_init_value = c(0.1, 0.1),
    coverage_end_value = c(0.1, 0.5),
    coverage_init_year = c(2020, 2020),
    coverage_end_year = c(2021, 2021),
    qaly_gains_value = c(0.1, 0.1),
    healthcare_cost_savings_value = c(1000, 2000),
    optimism_bias_benefits_value = c(1, 0.9),
    case_study_number = c(1, 1),
    scenario = c("Scenario A", "Scenario B")
  )
  
  # Set additional parameters
  health_discount_rate <- 0.035
  cost_discount_rate <- 0.015
  monetary_qaly <- 70000
  target_cost_year <- 2020
  years_of_coverage <- 10
  
  

  return_value <- benefits_for_all_rows(parameter_scenarios, health_discount_rate, cost_discount_rate, monetary_qaly, target_cost_year, years_of_coverage)
  
  # Test that there are the right number of rows
  expect_equal(
    nrow(return_value),
    2 * 10 # 2 scenarios, 10 years
  )
  
})

