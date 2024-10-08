
# TODO get NIHR colour scheme
COLOR_CATEGORICAL <- c(
  "Dark Blue" = "#193E72", 
  "Red" = "#EA5D4E", 
  "Orange" = "#F29330", 
  "Yellow" = "#FED47A", 
  "Purple" = "#6667AD", 
  "Teal" = "#2EA9B0", 
  "Green" = "#46A86C", 
  "Light Grayish Blue" = "#ACBCC3"
)

COLOR_STOPLIGHT <- c(
  "Stop" = "#EA5D4E", 
  "Wait" = "#F29330", 
  "Go" = "#193E72"
)

COLOR_SEQUENTIAL <- c(
  "100Dark Blue" = "#193E72", 
  "80Dark Blue" = "#475989", 
  "60Dark Blue" = "#747CA3", 
  "40Dark Blue" = "#A2A4C1", 
  "20Dark Blue" = "#D0D0E0", 
  "20Dark Orange" = "#FDEBD8", 
  "40Dark Orange" = "#FCD6B0", 
  "60Dark Orange" = "#F9C187", 
  "80Dark Orange" = "#F6AB5D", 
  "100Dark Orange" = "#F29330"
)

FONT_FAMILY <- "serif"

# Helper function to format numbers with units
format_units <- function(x, units) {
  format_with_commas <- function(num, digits = 0) {
    formatC(num, format = "f", big.mark = ",", digits = digits, drop0trailing = FALSE)
  }
  
  switch(units,
         k = paste0(format_with_commas(x / 1e3, 1), "k"),
         mn = paste0(format_with_commas(x / 1e6, 1), "mn"),
         bn = paste0(format_with_commas(x / 1e9, 1), "bn"),
         tn = paste0(format_with_commas(x / 1e12, 1), "tn"),
         none = format_with_commas(x, 0),
         stop("Invalid unit specified"))
}



add_theme_and_save <- function(p, fig_path, case_study, chart_name, avoid_overlap_x_axis = FALSE) {

  
  output_file <- paste0(fig_path, case_study, "_", chart_name, ".png")
  
  if (avoid_overlap_x_axis) {
    x_axis_theme <- element_text(family = FONT_FAMILY, size = 10, angle = 45, hjust = 1)
  } else { 
    x_axis_theme <- element_text(family = FONT_FAMILY, size = 10)
  }
  
  p <- p +  
    theme_classic() +
    theme(plot.title = element_text(family = FONT_FAMILY, face = "bold", size = 14),
          axis.title.x = element_text(family = FONT_FAMILY, size = 12),
          axis.title.y = element_text(family = FONT_FAMILY, size = 12),
          axis.text.x = x_axis_theme,
          axis.text.y = element_text(family = FONT_FAMILY, size = 10),
          legend.position = "top",
          legend.justification = "right",
          legend.title = element_text(family = FONT_FAMILY, size = 10),
          legend.text = element_text(family = FONT_FAMILY, size = 8),
          strip.text = element_text(family = FONT_FAMILY, size = 10)  # Facet title font
    )
  
  
  # Remove .png extension if present
  base_output_file <- sub("\\.png$", "", output_file)
  
  # Save as PNG (keeping the original output_file name)
  ggsave(output_file, plot = p, width = 18, height = 14, units = "cm", dpi = 300)
  
  
  # Save as SVG
  ggsave(paste0(base_output_file, ".svg"), plot = p, width = 18, height = 14, units = "cm")
  
  return(p)
}