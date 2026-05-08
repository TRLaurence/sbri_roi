source("R/utils_paths.R")
# Reads of cancer registrations from the ONS and returns a data frame with the following columns:

library(dplyr)
library(janitor)
library(tidyr)
library(stringr)
library(readr)

cruk_surv_file <- "CRUK_EDHub_SurvivalIncidenceByStage_DataTable2026-04-24.csv"
path_to_cruk_surv <- file.path(raw_path, cruk_surv_file)

cruk_surv_data <- read_csv(path_to_cruk_surv) %>%
  clean_names() %>%
  group_by(country,cancer_site) %>%
  mutate(num_ten_year_surv =sum(!is.na(ten_year_survival))) %>%
  mutate(cancer_site = str_replace_all(cancer_site, "Bowel", "Colorectal")) %>%
  filter(num_ten_year_surv > 3) %>%
  ungroup() %>%
  select(cancer_site, stage, ten_year_survival) %>%
  pivot_wider(names_from = stage, values_from = ten_year_survival) %>%
  clean_names() %>%
  mutate(incremental_surv_stage_1 = stage_ii - stage_i,
         incremental_surv_stage_2 = stage_iii - stage_ii,
         incremental_surv_stage_3 = stage_iv - stage_iii) %>%
  select(cancer_site, incremental_surv_stage_1, incremental_surv_stage_2, incremental_surv_stage_3) %>%
  pivot_longer(cols = starts_with("incremental_surv"), names_to = "stage", values_to = "ten_year_survival") %>%
  mutate(ten_year_survival = ten_year_survival / 100) %>%
  mutate(stage = str_replace_all(stage, "incremental_surv_stage_", ""))
         

sud_data <- read_csv(file.path(raw_path, "daily_surv_changes.csv")) %>%
  clean_names() %>%
  select(tumour_type, stage, all_ages) 

sud_data$tumour_type %>% unique()
cruk_surv_data$cancer_site %>% unique()


sud_data <- sud_data %>%
  mutate(tumour_type = str_replace_all(tumour_type, "Breast \\(all hormone marker statuses\\)", "Breast")) %>%
  rename(daily_change_surv = all_ages) %>%
  mutate(daily_change_surv = as.numeric(str_replace(daily_change_surv, "%", "")) / 100)


sud_data$tumour_type %>% unique()

sud_data <- sud_data %>%
  filter((tumour_type %in% cruk_surv_data$cancer_site)) %>%
  left_join(cruk_surv_data, by = c("tumour_type" = "cancer_site",
                                    "stage" = "stage")) %>%
  filter(!is.na(ten_year_survival)) %>%
  mutate(ten_year_survival = - ten_year_survival) %>%
  mutate(implied_days_to_progress = ten_year_survival / daily_change_surv) 

write_csv(sud_data, file.path(proc_path, "sud_data_implied_time_to_progression.csv"))




