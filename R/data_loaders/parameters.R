# Check parameter values, ensure they're consistent

source("R/utils/paths.R")
source("R/data_loaders/ihme.R")
source("R/data_loaders/population.R")

library(dplyr)
library(tidyr)
library(readr)

case <- 99998

generate_replacement_init_populations <- function(initial_population_definer, initial_population_label) {
  
  if(initial_population_definer == "disease") {
    measure_val <- str_trim(str_extract(initial_population_label, "Prevalence|Incidence"))
    disease_val <- str_trim(str_remove_all(initial_population_label, "Prevalence|Incidence"))
    
    replacement_values <- data_loader_gbd(measure_val, disease_val)
    
  } else if (initial_population_definer == "population") {
    replacement_values <- data_loader_ons_population(initial_population_label)
  } else {
    stop("Initial population definer not recognised")
  }
  
  stopifnot(nrow(replacement_values) == 1)
  stopifnot(names(replacement_values) == c("init_pop_value", "init_pop_lower", "init_pop_upper"))  
  return(replacement_values)
}

replace_init_populations <- function(parameter_vals) {
  cases_to_replace <- filter(parameter_vals, 
                             is.na(initial_population_value)) %>%
    select(case_study_number) %>%
    pull()
  
  # TOD) include the GBD and population data values here
  for (case in cases_to_replace) {
    initial_population_definer <- parameter_vals[parameter_vals$case_study_number == case, "initial_population_definer"]
    initial_population_label <- parameter_vals[parameter_vals$case_study_number == case, "initial_population_label"]
    
    replacement_values <- generate_replacement_init_populations(initial_population_definer, initial_population_label)
    # Replace values 
    parameter_vals[parameter_vals$case_study_number == case, "initial_population_value"] <- replacement_values$init_pop_value
    parameter_vals[parameter_vals$case_study_number == case, "initial_population_lower"] <- replacement_values$init_pop_lower
    parameter_vals[parameter_vals$case_study_number == case, "initial_population_upper"] <- replacement_values$init_pop_upper
    
  }
  
  parameter_vals <- parameter_vals %>%
    mutate(initial_population_value = as.numeric(initial_population_value),
           initial_population_upper = as.numeric(initial_population_upper),
           initial_population_lower = as.numeric(initial_population_lower))
  
  return(parameter_vals)
}

parameter_loader <- function(raw_path, parameter_file, case_studies = NULL) {
  # Load the parameter file
  parameter_file_path <- file.path(raw_path, parameter_file)
  
  parameter_vals <- read_csv(parameter_file_path)
  
  parameter_vals <- replace_init_populations(parameter_vals)
  
  parameter_vals <- filter(parameter_vals, !is.na(initial_population_value))
  
  if (!is.null(case_studies)) {
    parameter_vals <- filter(parameter_vals, case_study_number %in% case_studies)
  }
  
  return(parameter_vals)
}

graph_names_loader <- function(raw_path, parameter_file) {
  parameter_file_path <- file.path(raw_path, parameter_file)
  
  parameter_vals <- read_csv(parameter_file_path)
  
  rel_param_cols <- names(parameter_vals)[str_detect(names(parameter_vals), "_name|_number")]
  
  name_vals <- parameter_vals %>%
    select(all_of(rel_param_cols))
  
  return(name_vals)
  
           
}