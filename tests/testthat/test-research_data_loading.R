library(testthat)

test_that("fill_missing_funding_data", {
  
  funding_data <- data.frame(
    case_study_number = c(1, 2, 3, 4),
    split_salaries = c(0.5, NA, 0.7, NA),
    fec_markup = c(1.1, 1.2, NA, NA)
  )
  
  expected_result <- data.frame(
    case_study_number = c(1, 2, 3, 4),
    split_salaries = c(0.5, 0.6, 0.7, 0.6),
    fec_markup = c(1.1, 1.2, 1.125, 1.125)
  )
  
  result <- fill_missing_funding_data(funding_data, baseline_split_salaries = 0.6, baseline_fec_markup = 1.125)
  
  expect_equal(result, expected_result)
})

test_that("estimate_n_years", {
  
  funding_data <- data.frame(
    case_study_number = c(1, 2, 3, 4),
    award_start_year = c(2012, 2011, 2012, 2013),
    award_end_year = c(2012, 2013, 2013, 2015)
  )
  
  expected_result <-  c(1, 3, 2, 3)
  
  
  result <- estimate_n_years(funding_data)$n_years
  
  expect_equal(result, expected_result)
})

test_that("annualise_funding_to_spend", {
  
  funding_data <- data.frame(
    case_study_number = c(1, 2, 3, 4),
    award_start_year = c(2012, 2011, 2012, 2013),
    award_end_year = c(2012, 2013, 2013, 2015),
    n_years = c(1, 3, 2, 3),
    funding_amount = c(1000, 3000, 3000, 6000)
  )
  
  expected_result <- tibble(
    case_study_number = c(1, 2, 2, 2, 3, 3, 4, 4, 4),
    spend_year = c(2012, 2011, 2012, 2013, 2012, 2013, 2013, 2014, 2015),
    annualised_spend = c(1000, 1000,1000,1000,  1500, 1500, 2000, 2000, 2000)
  )
  
  result <- annualise_funding_to_spend(funding_data)
  
  expect_equal(result, expected_result)
})

test_that("adjust_for_fec_and_ni", {
  
  funding_data <- data.frame(
    case_study_number = c(1, 2, 3, 4),
    award_number = c(1, 2, 3, 4),
    split_salaries = c(0.5, 0.6, 0.7, 0.6),
    fec_markup = c(1, 1.2, 1.125, 1.25),
    spend_year = c(2012, 2011, 2012, 2013),
    annualised_spend = c(1000, 1000, 1500, 2000)
  )
  
  employer_ni <- 0.138
  expected_result <- data.frame(
    case_study_number = c(1, 2, 3, 4),
    award_number = c(1, 2, 3, 4),
    spend_year = c(2012, 2011, 2012, 2013),
    adjusted_annual_spend = c((0.5/(1+employer_ni) + 0.5)*1000*1,
                              (0.6/(1+employer_ni) + 0.4)*1000*1.2,
                              (0.7/(1+employer_ni) + 0.3)*1500*1.125,
                              (0.6/(1+employer_ni) + 0.4)*2000*1.25)
  )
  
  result <- adjust_for_fec_and_ni(funding_data, employer_ni)
  
  expect_equal(result, expected_result)
})

