library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(janitor)
source("R/utils_paths.R")


read_funding_data <- function(file_name = "funding_data.csv") {
  funding_data <- read_csv(file.path(raw_path, file_name)) %>%
    clean_names() %>%
    filter(award_type != "Excluded")   %>%
    mutate(funding_amount = funding_amount * proportion_of_funding) %>%
    select("case_study_number" = "case_study_name", "award_number", "award_start_year", "award_end_year", "funding_amount", "split_salaries", "fec_markup") 
  
  return(funding_data)  
}

#' @title fill_missing_funding_data
#' @description Fill missing values in the funding data
#' @param funding_data The funding data has columns split_salaries and fec_markup
#' @param baseline_split_salaries The default value for split_salaries
#' @param baseline_fec_markup The default value for fec_markup
#' @return funding_data with missing values filled
fill_missing_funding_data <- function(funding_data, baseline_split_salaries, baseline_fec_markup) {
  funding_data <- funding_data %>%
    mutate(split_salaries = ifelse(is.na(split_salaries), baseline_split_salaries, split_salaries),
           fec_markup = ifelse(is.na(fec_markup), baseline_fec_markup, fec_markup))
  return(funding_data)
}

#' @title estimate_n_years
#' @description Estimate the number of years for each award
#' @param funding_data The funding data has columns award_start_year and award_end_year
#' @return funding_data with n_years column added
estimate_n_years <- function(funding_data) {
  funding_data <- funding_data %>%
    mutate(n_years = award_end_year - award_start_year + 1)
  return(funding_data)
}

#' @title annualise_funding_to_spend
#' @description Annualise the funding to spend
#' @param funding_data The funding data has columns award_start_year, award_end_year, funding_amount
#' @return funding_data exploded to yearly spend
annualise_funding_to_spend <- function(funding_data) {
  expanded_data <- funding_data %>%
    rowwise() %>%
    mutate(yearly_data = list(tibble(
      spend_year = seq(award_start_year, award_end_year),
      annualised_spend = funding_amount / n_years
    ))) %>%
    unnest(yearly_data) %>%
    select(-n_years, -award_start_year, -award_end_year, -funding_amount)
  return(expanded_data)  
}

#' @title data_loader_funding
#' @description Load the funding data, fill missing values of split_salaries and fec_markup, estimate n_years and annualise the funding to spend
#' @param file_name The name of the file containing the funding data
#' @param baseline_split_salaries The default value for split_salaries
#' @param baseline_fec_markup The default value for fec_markup
#' @return funding_data with missing values filled, n_years estimated and exploded to annualised spend 
data_loader_funding <- function(file_name = "funding_data.csv", baseline_split_salaries = 0.6, baseline_fec_markup=1.125) {
  funding_data <- read_funding_data()
  funding_data <- fill_missing_funding_data(funding_data, baseline_split_salaries, baseline_fec_markup)
  funding_data <- estimate_n_years(funding_data)
  funding_data <- annualise_funding_to_spend(funding_data)
  return(funding_data)
}  

#' @title adjust_for_fec_and_ni
#' @description Adjust the funding data for FEC and NI
#' @param funding The funding data has columns annualised_spend, split_salaries, fec_markup
#' @param employer_ni The employer NI rate
#' @return funding_data with adjusted annual spend
#' @export
adjust_for_fec_and_ni <- function(funding, employer_ni) {
  funding <- funding %>%
    mutate(fec_adjusted_spend = annualised_spend * fec_markup) %>%
    mutate(salary_spend = fec_adjusted_spend * split_salaries / (1 + employer_ni)) %>%
    mutate(non_salary_spend = fec_adjusted_spend * (1- split_salaries)) %>%
    mutate(adjusted_annual_spend = salary_spend + non_salary_spend) %>%
    select(-fec_adjusted_spend, -split_salaries, -fec_markup, -annualised_spend, - salary_spend, -non_salary_spend)
  return(funding)
}

