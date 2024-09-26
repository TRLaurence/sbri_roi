# ihme
source("R/utils/paths.R")
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

perma_link_level_4 <- "https://vizhub.healthdata.org/gbd-results?params=gbd-api-2021-permalink/3759ef80d9db48868d824a9ff3c2f357"
perma_link_level_3 <-"https://vizhub.healthdata.org/gbd-results?params=gbd-api-2021-permalink/17ed0ebec2e5c096e4c0966b82afcb83"

read_gbd_from_file <- function() {
  file_name_level_3 <- "IHME-GBD_2021_DATA-LEVEL-3.csv"
  file_name_level_4 <- "IHME-GBD_2021_DATA-LEVEL-4.csv"
  gbd_level_3 <- read_csv(file.path(raw_path, file_name_level_3))
  gbd_level_3$level <- 3
  gbd_level_4 <- read_csv(file.path(raw_path,file_name_level_4))
  gbd_level_4$level <- 4
  gbd_all <- bind_rows(gbd_level_3, gbd_level_4)
  return(gbd_all)  
}

data_loader_gbd <- function(measure_val, disease_val) {
  gbd_all <- read_gbd_from_file()
  
  disease_val <- str_to_upper(disease_val)
  measure_val <- str_to_upper(measure_val)
  
  return_vals <- gbd_all %>%
    mutate(measure = str_to_upper(measure),
           cause = str_to_upper(cause)) %>%
    filter(measure == measure_val & cause == disease_val) %>%
    select(val, lower, upper) %>%
    unique()
  
  stopifnot(nrow(gbd_all) != 1)
  
  names(return_vals) <- c("init_pop_value", "init_pop_lower", "init_pop_upper")
  return(return_vals)
}