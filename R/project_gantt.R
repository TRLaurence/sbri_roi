# Load necessary libraries
library(ggplot2)
library(dplyr)
library(forcats)
source("R/utils_paths.R")
# Define tasks, start dates, and end dates
tasks <- c("Project identification", "Methods design and coding", 
           "Literature review", "Expert interviews", 
           "First draft case studies", "Final draft case studies", 
           "Wrap up")

start_dates <- as.Date(c("2024-06-01", "2024-07-01", "2024-07-01", 
                         "2024-09-01", "2024-09-15", "2024-10-15", 
                         "2024-12-01"))

end_dates <- as.Date(c("2024-07-15", "2024-09-01", "2024-10-31", 
                       "2024-10-15", "2024-10-31", "2024-11-30", 
                       "2025-01-01"))

# Create a data frame
gantt_data <- data.frame(tasks = factor(tasks, levels = tasks),
                         start = start_dates,
                         end = end_dates)

# Reverse the order of tasks for better visualization  by changing the factor levels
gantt_data <- gantt_data %>% 
  mutate(tasks = forcats::fct_rev(tasks))


# Create the Gantt chart
p <- ggplot(gantt_data, aes(y = tasks)) +
  geom_segment(aes(x = start, xend = end, yend = tasks), size = 6) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
  labs(x = "Timeline", y = "Tasks", title = "NIHR Impact Case Studies Plan") +
  theme_minimal()

print(p)

# Save the plot as a PNG file
ggsave(file.path(fig_path,"gantt_chart.jpeg"), plot = p, width = 18, height = 14, units = "cm", dpi = 300) 

