source("R/utils/paths.R")
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/ageing/datasets/midyearpopulationestimatesoftheveryoldincludingcentenariansengland
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/annualmidyearpopulationestimates/mid2022/relateddata
# Stitched together the UK estimates for people and broke out 90+ for the UK by assuming it matches the trajectory of the England and Wales data

read_uk_pop_from_file <- function() {
  file_name <- "ons_pop.csv"
  file_path <- file.path(raw_path, file_name) %>%
    str_replace("//", "/")

  uk_pop <- read_csv(file_path)
  
  return(uk_pop)  
}
data_loader_ons_population <- function(age_string) {
  uk_pop <- read_uk_pop_from_file()  
  str_split <- str_split(age_string, "-")
  age_lower <- as.numeric(str_split[[1]][1])
  age_upper <- as.numeric(str_split[[1]][2])
  uk_pop_sum <- uk_pop %>%
    filter(age >= age_lower & age <= age_upper) %>%
    select(population) %>%
    sum()
  
  lower_val <- uk_pop_sum * 0.95
  upper_val <- uk_pop_sum * 1.05
  
  return_vals <- data.frame(init_pop_value = uk_pop_sum, init_pop_lower = lower_val, init_pop_upper = upper_val)
  
  return(return_vals)
}
 
