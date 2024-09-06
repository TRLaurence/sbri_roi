
# TODO get NIHR colour scheme


add_theme_and_save <- function(p, fig_path, case_study, chart_name, avoid_overlap_x_axis = FALSE) {
  font_family <- "serif"
  
  output_file <- paste0(fig_path, case_study, "_", chart_name, ".png")
  
  if (avoid_overlap_x_axis) {
    x_axis_theme <- element_text(family = font_family, size = 10, angle = 45, hjust = 1)
  } else { 
    x_axis_theme <- element_text(family = font_family, size = 10)
  }
  
  p <- p +  
    theme_classic() +
    theme(plot.title = element_text(family = font_family, face = "bold", size = 14),
          axis.title.x = element_text(family = font_family, size = 12),
          axis.title.y = element_text(family = font_family, size = 12),
          axis.text.x = x_axis_theme,
          axis.text.y = element_text(family = font_family, size = 10),
          legend.position = "top",
          legend.justification = "right",
          legend.title = element_text(family = font_family, size = 10),
          legend.text = element_text(family = font_family, size = 8),
          strip.text = element_text(family = font_family, size = 10)  # Facet title font
    )
  
  
  # Remove .png extension if present
  base_output_file <- sub("\\.png$", "", output_file)
  
  # Save as PNG (keeping the original output_file name)
  ggsave(output_file, plot = p, width = 18, height = 14, units = "cm", dpi = 300)
  
  
  # Save as SVG
  ggsave(paste0(base_output_file, ".svg"), plot = p, width = 18, height = 14, units = "cm")
  
  return(p)
}