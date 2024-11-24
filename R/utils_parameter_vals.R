optimism_bias <- 0.15
monetary_qaly <- 70000
cost_discount_rate <- 0.035
health_discount_rate <- 0.015
target_cost_year <- 2023
years_of_coverage <- 10
# TODO change back
number_of_samples <- 1000

employer_ni <- 0.138
baseline_split_salaries <- 0.6
baseline_fec_markup <- 1.125

# The uplift to research costs from mapping basic research to applied research
applied_adjustment <- 2.0
applied_adjustment_lower <- 2.5 # This is higher despite being called lower because more basic research mapped to applied research leads to a lower ROI
applied_adjustment_upper <- 1.5

# The uplift to research costs from under-ascertainment of related research that should be included 
under_ascertainment_bias <- 1.2
under_ascertaintment_lower <- 1.5 # This is higher despite being called lower because if more research is missed, the ROI is lower
under_ascertaintment_upper <- 0.9