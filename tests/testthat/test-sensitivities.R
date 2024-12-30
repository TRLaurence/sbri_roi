library("testthat")


# Helper function for Gamma distribution
gamma_params <- function(mean_value, std_dev) {
  stopifnot(mean_value > 0, std_dev > 0)
  beta <- mean_value / (std_dev^2)
  alpha <- mean_value * beta
  return(list(alpha = alpha, beta = beta))
}

# Helper function for Beta distribution
beta_params <- function(mean_value, std_dev) {
  stopifnot(mean_value > 0, mean_value < 1, std_dev > 0, std_dev < 1)
  var_value <- std_dev^2
  alpha <- ((mean_value * (1 - mean_value)) / var_value - 1) * mean_value
  beta <- alpha * (1 / mean_value - 1)
  return(list(alpha = alpha, beta = beta))
}

test_that("gamma_params", {
  expect_equal(gamma_params(5, 2), list(alpha = 25/4, beta = 5/4))
  expect_equal(gamma_params(10, 3), list(alpha = 100/9, beta = 10/9))
  expect_equal(gamma_params(1, 1), list(alpha = 1, beta = 1))
  
  expect_error(gamma_params(0, 1))
  
  params <- gamma_params(5, 2)
  
  gamma_rv <- rgamma(1000, shape = params$alpha, rate = params$beta)
  expect_equal(mean(gamma_rv), 5, tolerance = 0.1)
  expect_equal(var(gamma_rv), 2^2, tolerance = 0.5)
  
})

test_that("beta_params", {
  expect_equal(beta_params(0.5, 0.1), list(alpha = (0.5*(1-0.5)/(0.1^2) -1) * 0.5, beta = (0.5*(1-0.5)/(0.1^2) -1) * 0.5), tolerance = 1e-6)
  expect_equal(beta_params(0.5, 0.2), list(alpha = (0.5*(1-0.5)/(0.2^2) -1) * 0.5, beta = (0.5*(1-0.5)/(0.2^2) -1) * 0.5), tolerance = 1e-6)
  expect_equal(beta_params(0.3, 0.1), list(alpha = (0.3*(1-0.3)/(0.1^2) -1) * 0.3, beta = (0.3*(1-0.3)/(0.1^2) -1) * 0.7), tolerance = 1e-6)
  
  expect_error(beta_params(0, 1))
  expect_error(beta_params(1, 1))
  expect_error(beta_params(0.5, 1.1))
  expect_error(beta_params(0.5, 0))
  
  params <- beta_params(0.5, 0.1)
  beta_rv <- rbeta(1000, shape1 = params$alpha, shape2 = params$beta)
  expect_equal(mean(beta_rv), 0.5, tolerance = 0.01)
  expect_equal(var(beta_rv), 0.1^2, tolerance = 0.01)
  
  #' Test with the largest standard deviation possible
  params <- beta_params(0.8, 0.25)
  beta_rv <- rbeta(10000, shape1 = params$alpha, shape2 = params$beta)
  expect_equal(mean(beta_rv), 0.8, tolerance = 0.03)
  expect_equal(var(beta_rv), 0.25^2, tolerance = 0.03)
  
  #' Test with a number that's close to 0
  
  params <- beta_params(0.01, 0.02)
  beta_rv <- rbeta(10000, shape1 = params$alpha, shape2 = params$beta)
  expect_equal(mean(beta_rv), 0.01, tolerance = 0.001)
  expect_equal(var(beta_rv), 0.02^2, tolerance = 0.001)
})

test_that("test set_up_reference", {
  parameter_vals <- data.frame(
   initial_population_value = c(1000),
   initial_population_lower = c(900),
   initial_population_upper = c(1100),
   healthcare_cost_savings_value = c(1000),
   healthcare_cost_savings_lower =c(900),
   healthcare_cost_savings_upper = c(1100),
   healthcare_cost_savings_year = c(2020)
  )
    
  result <- set_up_reference(parameter_vals)
  
  expected_result <- data.frame(
    initial_population_value = c(1000),
    healthcare_cost_savings_value = c(1000),
    healthcare_cost_savings_year = c(2020),
    scenario = c("reference") 
  )
  
  expect_equal(result, expected_result)
})

test_that("test set_up_reference multiple case studies", {
  parameter_vals <- data.frame(
    case_study = c("case_study_1", "case_study_2"),
    initial_population_value = c(1000, 2000),
    initial_population_lower = c(900, 1900),
    initial_population_upper = c(1100, 2100),
    healthcare_cost_savings_value = c(1000, 2000),
    healthcare_cost_savings_lower =c(900, 1900),
    healthcare_cost_savings_upper = c(1100, 2100),
    healthcare_cost_savings_year = c(2020, 2021)
  )
  
  result <- set_up_reference(parameter_vals)
  
  expected_result <- data.frame(
    case_study = c("case_study_1", "case_study_2"),
    initial_population_value = c(1000, 2000),
    healthcare_cost_savings_value = c(1000, 2000),
    healthcare_cost_savings_year = c(2020, 2021),
    scenario = c("reference", "reference") 
  )
  
  expect_equal(result, expected_result)
})

test_that("test set_up_reference", {
  parameter_vals <- data.frame(
    initial_population_value = c(1000),
    initial_population_lower = c(900),
    initial_population_upper = c(1100),
    healthcare_cost_savings_value = c(1000),
    healthcare_cost_savings_lower =c(900),
    healthcare_cost_savings_upper = c(1100),
    healthcare_cost_savings_year = c(2020)
  )
  
  result <- set_up_reference(parameter_vals)
  
  expected_result <- data.frame(
    initial_population_value = c(1000),
    healthcare_cost_savings_value = c(1000),
    healthcare_cost_savings_year = c(2020),
    scenario = c("reference") 
  )
  
  expect_equal(result, expected_result)
})

test_that("test set_up_reference multiple case studies", {
  parameter_vals <- data.frame(
    case_study = c("case_study_1", "case_study_2"),
    initial_population_value = c(1000, 2000),
    initial_population_lower = c(900, 1900),
    initial_population_upper = c(1100, 2100),
    healthcare_cost_savings_value = c(1000, 2000),
    healthcare_cost_savings_lower =c(900, 1900),
    healthcare_cost_savings_upper = c(1100, 2100),
    healthcare_cost_savings_year = c(2020, 2021)
  )
  
  result <- set_up_deterministic(parameter_vals)
  
  expected_result <- data.frame(
    case_study = c("case_study_1", "case_study_2", "case_study_1", "case_study_2", 
                   "case_study_1", "case_study_2", "case_study_1", "case_study_2"),
    initial_population_value = c(1000, 2000, 1000, 2000, 1100, 2100, 900, 1900),
    healthcare_cost_savings_value = c(1100, 2100, 900, 1900,  1000, 2000, 1000, 2000),
    healthcare_cost_savings_year = c(2020, 2021, 2020, 2021, 2020, 2021, 2020, 2021),
    scenario = c( "healthcare_cost_savings_upper", "healthcare_cost_savings_upper", "healthcare_cost_savings_lower", "healthcare_cost_savings_lower",
                  "initial_population_upper", "initial_population_upper", "initial_population_lower", "initial_population_lower") 
  )
  
  expect_equal(result, expected_result)
})

test_that("test distribution_mapping", {
  
  expect_equal(distribution_mapping("initial_population"), "gamma")
  expect_equal(distribution_mapping("adjustment_for_sub_population"), "beta")
  expect_equal(distribution_mapping("coverage_init"), "beta")
  expect_equal(distribution_mapping("coverage_end"), "beta")
  expect_equal(distribution_mapping("qaly_gains"), "normal")
  expect_equal(distribution_mapping("healthcare_cost_savings"), "normal")
  expect_equal(distribution_mapping("productivity_gains"), "normal")
  expect_equal(distribution_mapping("socialcare_cost_savings"), "normal")
  expect_equal(distribution_mapping("research_funding"), "gamma")
  expect_equal(distribution_mapping("optimism_bias_benefits"), "uniform")
  expect_equal(distribution_mapping("applied_adjustment"), "normal")
  
  expect_error(distribution_mapping("not_a_parameter"))
  
})

generate_distribution <- function(mean_value, lower_bound, upper_bound, n, distribution = c("normal", "gamma", "beta", "uniform"))
  
test_that("test generate_distribution", {
  
  normal_vals <- generate_distribution(1000, 900, 1100, 1000, "normal")
  
  expect_true(mean(normal_vals >= 900) > 0.95)
  expect_true(mean(normal_vals <= 1100) > 0.95)
  expect_equal(mean(normal_vals), 1000, tolerance = 10)
  expect_equal(sd(normal_vals), 50, tolerance = 10)
  
  gamma_vals <- generate_distribution(1000, 900, 1100, 1000, "gamma")
  
  expect_true(mean(gamma_vals >= 800) > 0.99)
  expect_true(mean(gamma_vals <= 1200) > 0.99)
  expect_equal(mean(gamma_vals), 1000, tolerance = 10)
  expect_equal(sd(gamma_vals), 50, tolerance = 10)
  
  beta_vals <- generate_distribution(0.5, 0.4, 0.6, 1000, "beta")
  expect_true(all(beta_vals >= 0.1))
  expect_true(all(beta_vals <= 0.9))
  expect_equal(mean(beta_vals), 0.5, tolerance = 0.1)
  expect_equal(sd(beta_vals), 0.05, tolerance = 0.05)
  
  uniform_vals <- generate_distribution(1000, 900, 1100, 1000, "uniform")
  
  expect_true(all(uniform_vals >= 900))
  expect_true(all(uniform_vals <= 1100))
  expect_equal(mean(uniform_vals), 1000, tolerance = 10)
  expect_equal(sd(uniform_vals), 50, tolerance = 10)
  
  uniform_vals <- generate_distribution(1000, 900, 1500, 1000, "uniform")
  
  expect_true(all(uniform_vals >= 900))
  expect_true(all(uniform_vals <= 1500))
  expect_equal(mean(uniform_vals), 1200, tolerance = 10)
  expect_equal(sd(uniform_vals), 150, tolerance = 10)
  
    
  expect_error(generate_distribution(1000, 900, 1100, 1000, "not_a_distribution"))
  
})

test_that("Test create_matching_probabilistic_df", {
  case_study_vals <- data.frame(
    initial_population_value = c(1000),
    initial_population_lower = c(900),
    initial_population_upper = c(1100),
    healthcare_cost_savings_value = c(1000),
    healthcare_cost_savings_lower =c(900),
    healthcare_cost_savings_upper = c(1100),
    healthcare_cost_savings_year = c(2020)
  )
  
  parameters_to_change <- c("initial_population_value", "healthcare_cost_savings_value")
  number_of_samples <- 5
  
  result <- create_matching_probabilistic_df(case_study_vals, parameters_to_change, number_of_samples)
  
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), ncol(case_study_vals) + 1)
  
  expect_equal(result$scenario, c("probabilistic1", "probabilistic2", "probabilistic3", "probabilistic4", "probabilistic5"))
  
  expect_true(all(result$initial_population_value >= 700))
  expect_true(all(result$initial_population_value <= 1300))
  expect_true(all(result$healthcare_cost_savings_value >= 700))
  expect_true(all(result$healthcare_cost_savings_value <= 1300))
})

test_that("Test create_matching_probabilistic_df", {
  parameter_vals <- data.frame(
    case_study_number = c("case_study1", "case_study2"),
    initial_population_value = c(1000, 2000),
    initial_population_lower = c(900, 1900),
    initial_population_upper = c(1100, 2100),
    healthcare_cost_savings_value = c(1000, 2000),
    healthcare_cost_savings_lower =c(900, 1900),
    healthcare_cost_savings_upper = c(1100, 2100),
    healthcare_cost_savings_year = c(2020, 2021)
  )
  
    
  result <- set_up_probabilistic(parameter_vals, number_of_samples = 5)
  
  expect_equal(nrow(result), 10)
  expect_equal(ncol(result), 5) # case_study, scenario, initial_population_value, healthcare_cost_savings_value, healthcare_cost_savings_year
  
  expect_equal(result$scenario, rep(c("probabilistic1", "probabilistic2", "probabilistic3", "probabilistic4", "probabilistic5"), 2))
  
  expect_true(all(result$initial_population_value >= 700))
  expect_true(all(result$initial_population_value <= 2300))
  expect_true(all(result$healthcare_cost_savings_value >= 700))
  expect_true(all(result$healthcare_cost_savings_value <= 2300))
})

test_that("Test setting up all the scenarios", {
  parameter_vals <- data.frame(
    case_study_number = c("case_study1", "case_study2"),
    initial_population_value = c(1000, 2000),
    initial_population_lower = c(900, 1900),
    initial_population_upper = c(1100, 2100),
    healthcare_cost_savings_value = c(1000, 2000),
    healthcare_cost_savings_lower =c(900, 1900),
    healthcare_cost_savings_upper = c(1100, 2100),
    healthcare_cost_savings_year = c(2020, 2021)
  )
  result <- set_up_all_sensitivities(parameter_vals, deterministic_sensitivity = TRUE, probabilistic_sensitivity  = TRUE, number_of_samples = 5)
  
  # 2 for reference, 8 for deterministic, 10 for probabilistic
  expect_equal(nrow(result), 20)
  expect_equal(ncol(result), 5) # case_study, scenario, initial_population_value, healthcare_cost_savings_value, healthcare_cost_savings_year
  
  expect_equal(result$scenario, c("reference", "reference", 
                                  "healthcare_cost_savings_upper", 
                                  "healthcare_cost_savings_upper", 
                                  "healthcare_cost_savings_lower", 
                                  "healthcare_cost_savings_lower", 
                                  "initial_population_upper",
                                  "initial_population_upper",
                                  "initial_population_lower",
                                  "initial_population_lower",
                                  "probabilistic1", "probabilistic2", "probabilistic3", "probabilistic4", "probabilistic5",
                                  "probabilistic1", "probabilistic2", "probabilistic3", "probabilistic4", "probabilistic5"))
  
  expect_true(all(result$initial_population_value >= 700))
  expect_true(all(result$initial_population_value <= 2300))
  expect_true(all(result$healthcare_cost_savings_value >= 700))
  expect_true(all(result$healthcare_cost_savings_value <= 2300))
  
})

