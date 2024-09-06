library(janitor)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)

inflation_data_loader <- function() {
  ons_link <- "https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/l8gg/qna"
  
  data <- read_csv(ons_link)
  data <- clean_names(data)
  
  data <- data %>%
    filter(str_detect(title, "2[0-9]{3}")) %>%
    mutate(year = as.numeric(str_extract(title, "2[0-9]{3}"))) %>%
    mutate(quarter = str_extract(title, "Q[1-4]")) %>%
    rename(gdp_deflator = implied_gdp_deflator_at_market_prices_sa_index) %>%
    mutate(gdp_deflator = as.numeric(gdp_deflator)) 
  
  # Filter out years without 4 quarters
  data <- data %>%
    group_by(year) %>%
    filter(n() == 4) %>%
    summarise(gdp_deflator = mean(gdp_deflator, na.rm = TRUE)) %>%
    ungroup()
  
  # data <- data %>%
  #   mutate(annual_inflation = (gdp_deflator / lag(gdp_deflator)) - 1)

  return(data)  
}
