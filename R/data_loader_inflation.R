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
    filter(str_detect(title, "2[0-9]{3}|199[0-9]{1}")) %>%
    mutate(year = as.numeric(str_extract(title, "2[0-9]{3}|199[0-9]{1}"))) %>%
    mutate(quarter = str_extract(title, "Q[1-4]")) %>%
    rename(gdp_deflator = implied_gdp_deflator_at_market_prices_sa_index) %>%
    mutate(gdp_deflator = as.numeric(gdp_deflator)) 
  
  # Filter out years without 4 quarters
  data <- data %>%
    group_by(year) %>%
    filter(n() == 4) %>%
    summarise(gdp_deflator = mean(gdp_deflator, na.rm = TRUE)) %>%
    ungroup()
  
  max_year <- max(data$year)
  
  # Add 10 years into the future increasing the deflator by 2%
  future_years <- tibble(year = seq(max_year + 1, max_year + 10),
                         gdp_deflator = 1.02^((1:10) - 1) * data$gdp_deflator[nrow(data)])
  
  data <- bind_rows(data, future_years)
  
  # data <- data %>%
  #   mutate(annual_inflation = (gdp_deflator / lag(gdp_deflator)) - 1)

  return(data)  
}
