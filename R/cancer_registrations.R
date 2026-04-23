
source("R/utils_paths.R")
# Reads of cancer registrations from the ONS and returns a data frame with the following columns:


library(readODS)
library(dplyr)
library(janitor)
library(tidyr)
library(stringr)
library(readr)
get_rel_data <- function(icd_10_code_filter, age_filter, sex_filter, raw_path, file_name = "Cancer_Registrations_2021 for publication.ods") {
  path_to_cancer_registrations <- file.path(raw_path, file_name)
  data <- read_ods(path_to_cancer_registrations, sheet = "Table_2", skip =  9) %>%
    clean_names()
  
  allowable_values_icd <- data %>%
    pull(icd10_code) %>%
    unique()
  
  allowable_ages <- data %>%
    pull(age_at_diagnosis) %>%
    unique()
  
  allowable_genders <- data %>%
    pull(gender) %>%
    unique()
  
  if (is.null(icd_10_code_filter)) {
    icd_10_code_filter <- allowable_values_icd
  } else {
    icd_10_code_filter <- match_allowable_icd10(allowable_values_icd, icd_10_code_filter)
  }
  
  if (is.null(age_filter)) {
    age_filter <- allowable_ages
  }
  
  if (is.null(sex_filter)) {
    sex_filter <- allowable_genders
  }
  
  if (!all(icd_10_code_filter %in% allowable_values_icd)) {
    print(icd_10_code_filter)
    stop(paste0("Invalid ICD-10 code filter. Allowed values are: ", paste(allowable_values_icd, collapse = ", ")))
  }
  
  if (!all(age_filter %in% allowable_ages)) {
    stop(paste0("Invalid age filter. Allowed values are: ", paste(allowable_ages, collapse = ", ")))
  }
  
  if (!all(sex_filter %in% allowable_genders)) {
    stop(paste0("Invalid sex filter. Allowed values are: ", paste(allowable_genders, collapse = ", "))) 
  }
  
  filtered_data <- data %>%
    filter(icd10_code %in% icd_10_code_filter,
           age_at_diagnosis %in% age_filter,
           gender %in% sex_filter) 
  
  return(filtered_data)
}           

# Primary intracranial malignant/benign/uncertain tumours
# 
# C70.0, C70.9 — cerebral meninges / meninges unspecified
# C71.* — malignant neoplasm of brain
# D32.0, D32.9 — benign cerebral meninges / meninges unspecified
# D33.0-D33.2 — benign brain (supratentorial, infratentorial, unspecified)
# D42.0, D42.9 — uncertain/unknown behaviour of cerebral meninges / meninges unspecified
# D43.0-D43.2 — uncertain/unknown behaviour of brain (supratentorial, infratentorial, unspecified)
# C79.3 — secondary malignant neoplasm of brain and cerebral meninges, for brain metastases.

# Return TRUE for allowable ICD-10 values where:
# - a 3-character code in the definition (e.g. C70) matches "C70" and any subcode like "C70.0", "C70.1"
# - a 4-character code in the definition (e.g. D35.2) matches only that exact code
match_allowable_icd10 <- function(allowable_values_icd, code_set) {
  allowable_values_icd <- toupper(trimws(allowable_values_icd))
  code_set <- toupper(trimws(code_set))
  
  patterns <- vapply(
    code_set,
    function(x) {
      if (grepl("\\.", x)) {
        # exact 4-character match only
        paste0("^", gsub("\\.", "\\\\.", x), "$")
      } else {
        # match the 3-character code itself or any decimal subcode
        paste0("^", x, "(\\..+)?$")
      }
    },
    character(1)
  )
  
  matched_vals <- Reduce(
    `|`,
    lapply(patterns, grepl, x = allowable_values_icd, perl = TRUE)
  )
  return(allowable_values_icd[matched_vals])
}


orion_brain_cancer_icd_10_codes <- c("C70.0", 
                               "C70.9",
                                "C71.0", 
                               "C71.1", 
                               "C71.2", 
                               "C71.3", 
                               "C71.4", 
                               "C71.5", 
                               "C71.6", 
                               "C71.7", 
                               "C71.8", 
                               "C71.9",
                               "D32.0",
                               "D32.9",
                               "D33.0",
                               "D33.1",
                               "D33.2",
                               "D42.0",
                               "D42.9",
                               "D43.0",
                               "D43.1",
                               "D43.2",
                               "C79.3")

orion_brain_cancer_data <- get_rel_data(icd_10_code_filter = orion_brain_cancer_icd_10_codes,
             age_filter = c("All ages"),
             sex_filter = NULL,
             raw_path, 
             file_name = "Cancer_Registrations_2021 for publication.ods")

all_brain_cancer_icd_10_codes <- c(
  "C70", "C71", "C72",
  "C75.1", "C75.2", "C75.3",
  "D32", "D33",
  "D35.2", "D35.3", "D35.4",
  "D42", "D43",
  "D44.3", "D44.4", "D44.5"
)

all_brain_cancer_data <- get_rel_data(icd_10_code_filter = all_brain_cancer_icd_10_codes,
                                     age_filter = c("All ages"),
                                    sex_filter = NULL,
                                    raw_path,
                                    file_name = "Cancer_Registrations_2021 for publication.ods")

prop_sub_population_orion <- sum(orion_brain_cancer_data$count) / sum(all_brain_cancer_data$count)

not_orion <- all_brain_cancer_data %>%
  filter(!icd10_code %in% orion_brain_cancer_data$icd10_code) 

check_not_orion <- not_orion %>%
  arrange(desc(count))

all_data <- get_rel_data(icd_10_code_filter = NULL,
                         age_filter = NULL,
                         sex_filter = NULL,
                         raw_path,
                         file_name = "Cancer_Registrations_2021 for publication.ods")

epi_data_unique_ages <- all_data %>%
  select(age_at_diagnosis) %>%
  unique() %>%
  filter(!age_at_diagnosis %in% c("All ages", "Unknown")) %>%
  arrange(age_at_diagnosis) %>%
  mutate(age_at_diagnosis =str_replace(age_at_diagnosis, " and over", " to 99")) %>%
  mutate(age_at_diagnosis =str_replace(age_at_diagnosis, "Under", "0 to ")) %>%
  mutate(age_lower = as.integer(str_extract(age_at_diagnosis, "^[0-9]+"))) %>%
  mutate(age_upper = as.integer(str_extract(age_at_diagnosis, "[0-9]+$"))) %>%
  mutate(age_band_epi = paste0(age_lower, "-", age_upper))  %>%
  select(age_at_diagnosis, age_band_epi)

surv_age_categories_df <- tibble( age_band_surv = c("30-39",
                        "40-49",	
                        "50-59",	
                        "60-69",
                        "70-79",	
                        "80+"))
surv_age_categories_df <- surv_age_categories_df %>%
  mutate(age_band_surv = str_replace_all(age_band_surv, "\\+", "-99"))

icd_10_matching <- read_csv(file.path(raw_path, "sud2020_icd_lookup.csv")) 

get_age_band_lookup <- function(df1, age_col_1, df2, age_col_2) {
  df1[['age_band']] <- df1[[age_col_1]]
  df2[['age_band']] <- df2[[age_col_2]]
  
  df2 <- df2 %>%
    select(age_band) %>%
    unique() %>%
    mutate(age_lower = as.integer(str_extract(age_band, "^[0-9]+"))) %>%
    mutate(age_upper = as.integer(str_extract(age_band, "[0-9]+$"))) 
  
  df1 <- df1 %>%
    select(age_band) %>%
    unique()
  
  df1 <- df1 %>%
    mutate(age_lower = as.integer(str_extract(age_band, "^[0-9]+"))) %>%
    mutate(age_upper = as.integer(str_extract(age_band, "[0-9]+$"))) %>%
    mutate(num_years = age_upper - age_lower + 1) %>%
    group_by(age_band, age_lower, age_upper) %>%
    uncount(num_years) %>%
    mutate(n = row_number() - 1) %>%
    ungroup() %>%
    mutate(age_single_year = age_lower + n)
  
  get_rel_age_band <- function(age_num, age_band_df) {
    matched <- age_band_df %>%
      filter(age_lower <= age_num & age_upper >= age_num) %>%
      pull(age_band)
    if (length(matched) == 0) {
      matched <- "Unmatched"
    }
    return(matched)
  }
  matched_ages <- sapply(df1$age_single_year,
                         get_rel_age_band,
                         age_band_df = df2) 
  df1$age_band_matched <- matched_ages 
  get_mode <- function(v) {
    v <- v[!is.na(v)]          # optional: drop NAs
    if (length(v) == 0) return(NA_character_)
    
    tab <- table(v)
    modes <- names(tab)[tab == max(tab)]
    modes[1]                  # return first mode if multiple
  }
  
  df1 <- df1 %>%
    select(age_original = age_band, age_band_matched) %>%
    group_by(age_original) %>%
    summarise(age_band_matched = get_mode(age_band_matched), .groups = "drop")
  
  return(df1)
}
age_band_lookup <- get_age_band_lookup(df1 = epi_data_unique_ages,
                    age_col_1 = "age_band_epi",
                    df2 = surv_age_categories_df,
                    age_col_2 = "age_band_surv")
epi_data_unique_ages <- epi_data_unique_ages %>%
  left_join(age_band_lookup, by = c("age_band_epi" = "age_original")) 

all_data_age_band_corrected <- all_data %>%
  left_join(epi_data_unique_ages, by = c("age_at_diagnosis" = "age_at_diagnosis")) %>%
  rename(age_band_surv = age_band_matched) %>%
  filter(!(age_band_surv == "Unmatched" | is.na(age_band_surv))) 
  
all_data_age_band_corrected <- all_data_age_band_corrected %>%
  left_join(icd_10_matching, by = c("icd10_code" = "icd_code")) %>%
  filter(!is.na(paper_label))

check <- all_data_age_band_corrected %>%
  select(icd10_code, site_description, paper_label) %>%
  unique() 

stopifnot(all(icd_10_matching$icd_code %in% check$icd10_code))

write_csv(check, file.path(proc_path, "check_icd10_code_matching.csv"))

all_data_age_band_corrected <- all_data_age_band_corrected %>%
  group_by(paper_label, age_band_surv) %>%
  summarise(count = sum(count), .groups = "drop")

all_data_sud_wide <- all_data_age_band_corrected %>%
  pivot_wider(names_from = age_band_surv, values_from = count, values_fill = 0)

write_csv(all_data_sud_wide, file.path(proc_path, "diagnoses_consistent_with_sud.csv"))