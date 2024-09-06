# Check parameter values, ensure they're consistent

# Import anything from GBD or ONS pop needed for the initial values

library(dplyr)
library(tidyr)
library(readr)

replace_init_populations <- function(parameter_vals) {
  cases_to_replace <- filter(parameter_vals, 
                             initial_population_value == "Not applicable") %>%
    select(case_study_number) %>%
    pull()
  
  
  # TODO include the GBD and population data values here
  for (case in cases_to_replace) {
    parameter_vals[parameter_vals$case_study_number == case, "initial_population_value"] <- "1000"
    parameter_vals[parameter_vals$case_study_number == case, "initial_population_upper"] <- "1200"
    parameter_vals[parameter_vals$case_study_number == case, "initial_population_lower"] <- "800"
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