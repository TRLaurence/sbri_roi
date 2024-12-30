source("R/graph_utils.R")
source("R/utils_paths.R")

library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

create_uptake_graph <- function() {
  covid_uptake_graph <- read_csv(file.path(raw_path, "covid_vaccine_for_illustrative_graph.csv"))
  
  print(covid_uptake_graph)
  
  covid_uptake_graph <- covid_uptake_graph %>%
    select(date, value) %>%
    mutate(date = as.Date(date, format = "%d/%m/%Y")) %>% 
    group_by(date) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(proportion_uptake = value / (67000000*0.8))
  
  # Convert the date to numeric for fitting
  covid_uptake_graph <- covid_uptake_graph %>%
    mutate(numeric_date = as.numeric(date))
  
  # Fit a sigmoid function using nls
  sigmoid_fit <- nls(proportion_uptake ~ L / (1 + exp(-k * (numeric_date - x0))),
                     data = covid_uptake_graph,
                     start = list(L = 1, k = 0.1, x0 = median(covid_uptake_graph$numeric_date)),
                     control = nls.control(maxiter = 10000))
  
  # Predict the values based on the fitted sigmoid function
  covid_uptake_graph <- covid_uptake_graph %>%
    mutate(fitted_proportion = predict(sigmoid_fit))
  
  p <- covid_uptake_graph %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = proportion_uptake), color = "blue") +
    geom_line(aes(y = fitted_proportion), color = "red", linetype = "dashed") +
    labs(
      x = "Date",
      y = "Proportion of population vaccinated",
      title = "Proportion of population vaccinated against COVID-19 in the UK"
    ) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1))
  
  add_theme_and_save(p, file.path(fig_path, "covid_vaccine_uptake"), "report", "illustrative_uptake")
  
  return(p)  
}
