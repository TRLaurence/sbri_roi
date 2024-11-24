library(testthat)

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

pop_fancy_categories <- filter(name_vals, case_study_number == 12) %>%
  select(-case_study_number) 

variable <- c("init_pop_name", 
  "target_pop_name",
  "benefitting_pop_name",
  "total_pop_name")

remap_to_fancy_categories(variable, pop_fancy_categories)

