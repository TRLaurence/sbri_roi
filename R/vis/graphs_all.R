source("R/vis/graph_utils.R")

# graph_template <- function(fig_path, case_study, chart_df) {
#   chart_name <- "default"
#   
#   p <- ggplot(chart_df, aes(x = x, y = y)) +
#     geom_point() +
#     geom_line() +
#     ggtitle("Default Chart") +
#     xlab("X Axis") +
#     ylab("Y Axis")
#   
#   p <- add_theme_and_save(p, fig_path, case_study, chart_name)
#   
#   return(p)
# }