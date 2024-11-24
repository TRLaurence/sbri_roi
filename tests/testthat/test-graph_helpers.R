library(testthat)
source("R/vis/graphs_all.R")

df <- data.frame(
  variable = c("cost_value", "productivity_value", "cost_year", "productivity_year")
)

fancy_categories <- c(
  "cost_value" = "Cost Value",
  "productivity_value" = "Productivity Value",
  "cost_year" = "Cost Year",
  "productivity_year" = "Productivity Year"
)

fancy_categories_2 <- data.frame(
  cost_value = c("Cost Value"),
  productivity_value = c("Productivity Value"),
  cost_year = c("Cost Year"),
  productivity_year = c("Productivity Year")
)

fancy_categories_3 <- tibble::tibble(
  cost_value = c("Cost Value"),
  productivity_value = c("Productivity Value"),
  cost_year = c("Cost Year"),
  productivity_year = c("Productivity Year")
)

test_that("Test vector", {
  expect_equal(
    remap_to_fancy_categories(df$variable, fancy_categories), 
    c("Cost Value", "Productivity Value", "Cost Year", "Productivity Year")
  )
})

test_that("Test dataframe", {
  expect_equal(
    remap_to_fancy_categories(df$variable, fancy_categories_2), 
    c("Cost Value", "Productivity Value", "Cost Year", "Productivity Year")
  )
})

test_that("Test tibble", {
  expect_equal(
    remap_to_fancy_categories(df$variable, fancy_categories_3), 
    c("Cost Value", "Productivity Value", "Cost Year", "Productivity Year")
  )
})



test_vec <- c("Qaly", "qaly", "test", "test and test")

test_that("Test vec of variables",
          expect_equal(
            format_variable_names(test_vec),
            c("QALY", "QALY", "Test", "Test and test")
          )
  
)