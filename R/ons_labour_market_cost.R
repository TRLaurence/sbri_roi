library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(janitor)
library(tidyr)
library(readr)
library(httr)
library(tibble)
source("R/utils_paths.R")
source("R/utils_parameter_vals.R")
# =========================================================
# Source
# =========================================================

ONS_CANCER_URL <- paste0(
  "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/",
  "healthandsocialcare/healthandwellbeing/adhocs/3076impactofhealthconditions",
  "requiringhospitalisationonearningsemploymentandreceiptofbenefitsincluding",
  "breakdownsbycancerconditionpluscopdandihdandtypeofbenefitenglandapril2014",
  "todecember2022/cancerdatatables20251013.xlsx"
)

# =========================================================
# Download to data/raw/
# =========================================================

download_cancer_data <- function(
    url = ONS_CANCER_URL,
    dest = "data/raw/cancerdatatables20251013.xlsx",
    refresh = FALSE
) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  
  if (!file.exists(dest) || isTRUE(refresh)) {
    resp <- httr::GET(url, httr::write_disk(dest, overwrite = TRUE))
    httr::stop_for_status(resp)
  }
  
  dest
}

# =========================================================
# Helpers
# =========================================================

standardize_cause <- function(x) {
  x |>
    stringr::str_squish() |>
    stringr::str_to_lower()
}

parse_interval_bounds <- function(x) {
  m <- stringr::str_match(
    as.character(x),
    "^\\s*(-?\\d+)\\s+to\\s+(-?\\d+)\\s*$"
  )
  
  tibble::tibble(
    months_start = as.numeric(m[, 2]),
    months_end = as.numeric(m[, 3])
  )
}

is_interval_string <- function(x) {
  stringr::str_detect(
    as.character(x),
    "^\\s*-?\\d+\\s+to\\s+-?\\d+\\s*$"
  )
}

# =========================================================
# Read table index from Contents
#
# Actual layout:
# row 1: title
# row 2: note
# row 3: headers
# so skip = 2
# =========================================================

read_table_index <- function(path, contents_sheet = "Contents") {
  readxl::read_excel(path, sheet = contents_sheet, skip = 2) |>
    janitor::clean_names() |>
    dplyr::transmute(
      sheet_name = as.character(sheet_name),
      table_description = as.character(table_description),
      table_num = readr::parse_number(sheet_name)
    ) |>
    dplyr::filter(!is.na(sheet_name), !is.na(table_description)) |>
    dplyr::filter(dplyr::between(table_num, 16, 39)) |>
    dplyr::mutate(
      cause = stringr::str_match(
        table_description,
        stringr::regex("Model estimates for the effect of (.+?) on ", ignore_case = TRUE)
      )[, 2],
      cause = standardize_cause(cause)
    )
}

# =========================================================
# Identify columns robustly
# =========================================================

find_time_col <- function(nms) {
  hit <- nms[stringr::str_detect(nms, "^time_since_event")]
  if (length(hit) == 0) {
    stop("Could not find a time-since-event column.")
  }
  hit[1]
}

find_measure_col <- function(nms) {
  excluded_patterns <- c(
    "^time_since_event",
    "^estimate$",
    "^lower_confidence_limit$",
    "^upper_confidence_limit$",
    "^standard_error$",
    "^p_value$",
    "^pvalue$",
    "^months_start$",
    "^months_end$",
    "^interval_months$",
    "^sheet_name$",
    "^cause$"
  )
  
  keep <- nms
  for (pat in excluded_patterns) {
    keep <- keep[!stringr::str_detect(keep, pat)]
  }
  
  if (length(keep) == 0) {
    stop("Could not identify the outcome/measure column.")
  }
  
  keep[1]
}

# =========================================================
# Read one table
# - row 6 is header -> skip 5
# - keep only real interval rows
# - filter out notes/footer rows
# - pivot uncertainty long
# =========================================================

read_one_cancer_table <- function(path, sheet_name, cause_label) {
  raw <- readxl::read_excel(path, sheet = sheet_name, skip = 5) |>
    janitor::clean_names()
  
  nms <- names(raw)
  
  time_col <- find_time_col(nms)
  
  required_uncertainty_cols <- c(
    "estimate",
    "lower_confidence_limit",
    "upper_confidence_limit"
  )
  
  missing_uncertainty <- setdiff(required_uncertainty_cols, nms)
  if (length(missing_uncertainty) > 0) {
    stop(
      "Sheet ", sheet_name, " is missing expected columns: ",
      paste(missing_uncertainty, collapse = ", ")
    )
  }
  
  measure_col <- find_measure_col(nms)
  
  out <- raw |>
    dplyr::rename(
      time_since_event_months = dplyr::all_of(time_col),
      outcome_measure = dplyr::all_of(measure_col)
    ) |>
    dplyr::mutate(
      time_since_event_months = as.character(time_since_event_months),
      outcome_measure = as.character(outcome_measure)
    ) |>
    dplyr::filter(
      !is.na(time_since_event_months),
      is_interval_string(time_since_event_months)
    ) |>
    dplyr::filter(
      !if_all(
        dplyr::all_of(required_uncertainty_cols),
        ~ is.na(readr::parse_number(as.character(.x)))
      )
    )
  
  bounds <- parse_interval_bounds(out$time_since_event_months)
  
  out |>
    dplyr::bind_cols(bounds) |>
    dplyr::mutate(
      interval_months = months_end - months_start,
      sheet_name = sheet_name,
      cause = cause_label
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(required_uncertainty_cols),
        ~ readr::parse_number(as.character(.x))
      )
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(required_uncertainty_cols),
      names_to = "uncertainty",
      values_to = "value"
    ) |>
    dplyr::mutate(
      uncertainty = dplyr::recode(
        uncertainty,
        estimate = "mean",
        lower_confidence_limit = "lower",
        upper_confidence_limit = "upper"
      ),
      cause = standardize_cause(cause)
    ) |>
    janitor::clean_names()
}

# =========================================================
# Build full stacked long data
# =========================================================

load_cancer_dataloader <- function(path, contents_sheet = "Contents") {
  table_index <- read_table_index(path, contents_sheet = contents_sheet)
  
  purrr::map2_dfr(
    table_index$sheet_name,
    table_index$cause,
    ~ read_one_cancer_table(path = path, sheet_name = .x, cause_label = .y)
  ) |>
    janitor::clean_names()
}

# =========================================================
# Tag common periods
# =========================================================

tag_periods <- function(df) {
  df |>
    dplyr::mutate(
      period = dplyr::case_when(
        months_start == -6 & months_end == 0 ~ "pre_6m",
        months_start >= 0 & months_end <= 60 ~ "post_5y",
        TRUE ~ "other"
      )
    )
}

# =========================================================
# Filtering
# =========================================================

filter_cancer_data <- function(
    df,
    outcome_measure_filter = NULL,
    cause_filter = NULL,
    uncertainty_filter = NULL,
    period = NULL,
    start_month = NULL,
    end_month = NULL
) {
  out <- df
  
  if (!is.null(outcome_measure_filter)) {
    out <- out |>
      dplyr::filter(.data$outcome_measure %in% outcome_measure_filter)
  }
  
  if (!is.null(cause_filter)) {
    out <- out |>
      dplyr::filter(.data$cause %in% standardize_cause(cause_filter))
  }
  
  if (!is.null(uncertainty_filter)) {
    out <- out |>
      dplyr::filter(.data$uncertainty %in% uncertainty_filter)
  }
  
  if (!is.null(period)) {
    if (identical(period, "pre_6m")) {
      out <- out |>
        dplyr::filter(months_start == -6, months_end == 0)
    } else if (identical(period, "post_5y")) {
      out <- out |>
        dplyr::filter(months_start >= 0, months_end <= 60)
    } else if (identical(period, "custom")) {
      if (is.null(start_month) || is.null(end_month)) {
        stop("For period = 'custom', supply both start_month and end_month.")
      }
      
      out <- out |>
        dplyr::filter(months_start >= start_month, months_end <= end_month)
    } else {
      stop("period must be one of NULL, 'pre_6m', 'post_5y', or 'custom'.")
    }
  }
  
  out
}

# =========================================================
# Aggregation
#
# period_total:
#   sum(value * interval_months)
#
# period_mean_monthly:
#   weighted mean where each month contributes equally
# =========================================================

aggregate_cancer_data <- function(
    df,
    by = c("cause", "outcome_measure", "uncertainty")
) {
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(
      total_months = sum(interval_months, na.rm = TRUE),
      period_total = sum(value * interval_months, na.rm = TRUE),
      period_mean_monthly = period_total / total_months,
      n_intervals = dplyr::n(),
      intervals_used = paste(time_since_event_months, collapse = ", "),
      .groups = "drop"
    )
}

# =========================================================
# Main wrapper
# =========================================================

get_cancer_estimates <- function(
    path = "data/raw/cancerdatatables20251013.xlsx",
    url = ONS_CANCER_URL,
    refresh = FALSE,
    contents_sheet = "Contents",
    outcome_measure_filter = NULL,
    cause_filter = NULL,
    uncertainty_filter = NULL,
    period = NULL,
    start_month = NULL,
    end_month = NULL,
    aggregate = FALSE
) {
  path <- download_cancer_data(
    url = url,
    dest = path,
    refresh = refresh
  )
  
  df <- load_cancer_dataloader(path, contents_sheet = contents_sheet) |>
    tag_periods()
  
  df <- filter_cancer_data(
    df = df,
    outcome_measure_filter = outcome_measure_filter,
    cause_filter = cause_filter,
    uncertainty_filter = uncertainty_filter,
    period = period,
    start_month = start_month,
    end_month = end_month
  )
  
  if (isTRUE(aggregate)) {
    aggregate_cancer_data(df)
  } else {
    df
  }
}

# =========================================================
# Example usage
# =========================================================

path <- download_cancer_data()

all_data <- load_cancer_dataloader(path) |>
  tag_periods()

all_data$outcome_measure %>% unique()

all_cancers_5y <- get_cancer_estimates(
  path = path,
  cause_filter = NULL,
  uncertainty_filter = "mean",
  outcome_measure_filter = "Monthly employee pay (£)",
  period = "post_5y",
  aggregate = TRUE
)

all_cancers_pre_6m <- get_cancer_estimates(
  path = path,
  cause_filter = NULL,
  uncertainty_filter = "mean",
  outcome_measure_filter = "Monthly employee pay (£)",
  period = "pre_6m",
  aggregate = FALSE
) %>%
  mutate(
    value = ifelse(p_value > 0.05, 0, value)
  )

total_cost_over_pre_6m <- all_cancers_pre_6m %>%
  group_by(cause) %>%
  summarise(
    mean_cost_per_month_pre = mean(value, na.rm = TRUE),
    total_cost_pre_6_months = sum(value * interval_months, na.rm = TRUE)
  )

all_cancers_5y_for_join <- all_cancers_5y %>%
  select(cause, period_total) %>%
  rename(total_cost_post_5y = period_total)

costs_for_study <- total_cost_over_pre_6m %>%
  left_join(all_cancers_5y_for_join, by = "cause") %>%
  mutate(
    total_cost_pre_6_months = ifelse(is.na(total_cost_pre_6_months), 0, total_cost_pre_6_months),
    total_cost_post_5y = ifelse(is.na(total_cost_post_5y), 0, total_cost_post_5y)
  ) 
  
write_csv(costs_for_study, "data/processed/cancer_costs_employee_pay_summary.csv")

custom_window <- get_cancer_estimates(
  path = path,
  cause_filter = NULL,
  outcome_measure_filter = "Monthly employee pay (£)",
  period = "custom",
  start_month = 0,
  end_month = 60,
  aggregate = FALSE
)

productivity_loss_by_site_and_year <- custom_window %>%
  filter(uncertainty == "mean") %>%
  filter(! cause %in% c("chronic obstructive pulmonary disease", "ischemic heart disease" )) %>%
  mutate(cause = str_replace_all(cause, " cancer", "")) %>%
  mutate(year = 1 + months_start %/% 12) %>%
  group_by(cause, year) %>%
  summarise(
    total_cost = -sum(value * interval_months, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(discounted_total_cost = total_cost / ((1+cost_discount_rate) ^ year)) %>%
  select(cause, year, productivity_loss = discounted_total_cost)

dummy_brain_category <- productivity_loss_by_site_and_year %>%
  filter(cause == "pancreatic") %>%
  mutate(cause = "brain")

dummy_melanoma_category <- productivity_loss_by_site_and_year %>%
  group_by(year) %>%
  summarise(productivity_loss = mean(productivity_loss, na.rm = TRUE)) %>%
  mutate(cause = "melanoma")

productivity_loss_by_site_and_year <- bind_rows(
  productivity_loss_by_site_and_year,
  dummy_brain_category,
  dummy_melanoma_category
)

lookup <- read_csv("data/processed/cancer_site_lookup.csv")

loss_per_site_per_year_death <- read_csv("data/processed/loss_per_death_by_cancer_site_by_year.csv") %>%
  select(cause = Cause, year = WhichLifeLostYear, loss_per_death) %>%
  mutate(cause = str_to_lower(cause)) 



loss_per_site_per_year_death <- loss_per_site_per_year_death %>%
  left_join(lookup, by = c("cause" = "pvflp_site")) %>%
  full_join(productivity_loss_by_site_and_year, by = c("ons_site" = "cause", "year")) %>%
  filter(match_type != "unmatched")
