# SBRI Healthcare brand colours
# Top 4 = main colours; bottom 4 = secondary colours

COLOR_BRAND_MAIN <- c(
  "SBRI Blue"  = "#007DB3",  # RGB 0/125/179
  "Light Blue" = "#A0D3E5",  # RGB 160/211/229
  "Sky Blue"   = "#34AFD3",  # RGB 52/175/211
  "Teal"       = "#2C948F"   # RGB 44/148/143
)

COLOR_BRAND_SECONDARY <- c(
  "Lime Green" = "#8FBF21",  # RGB 143/191/33
  "Pink"       = "#EA4B94",  # RGB 234/75/148
  "Yellow"     = "#FFD500",  # RGB 255/213/0
  "Orange"     = "#E84E0F"   # RGB 232/78/15
)

COLOR_CATEGORICAL <- c(
  COLOR_BRAND_MAIN,
  COLOR_BRAND_SECONDARY
)

# No true red is provided in the brand palette, so orange is used for "Stop".
COLOR_STOPLIGHT <- c(
  "Stop" = "#E84E0F",
  "Wait" = "#FFD500",
  "Go"   = COLOR_CATEGORICAL[["SBRI Blue"]]
)

# This preserves your existing blue-to-orange structure.
# Technically this is a diverging palette rather than a pure sequential palette.
COLOR_SEQUENTIAL <- c(
  "100 SBRI Blue"   = "#007DB3",
  "80 SBRI Blue"    = "#3397C2",
  "60 SBRI Blue"    = "#66B1D1",
  "40 SBRI Blue"    = "#99CBE1",
  "20 SBRI Blue"    = "#CCE5F0",
  "20 SBRI Orange"  = "#FADCCF",
  "40 SBRI Orange"  = "#F6B89F",
  "60 SBRI Orange"  = "#F1956F",
  "80 SBRI Orange"  = "#ED713F",
  "100 SBRI Orange" = "#E84E0F"
)

# Optional true sequential palette using only the core blue family
COLOR_SEQUENTIAL_BLUE <- c(
  "20 SBRI Blue"  = "#CCE5F0",
  "40 SBRI Blue"  = "#99CBE1",
  "60 SBRI Blue"  = "#66B1D1",
  "80 SBRI Blue"  = "#3397C2",
  "100 SBRI Blue" = "#007DB3"
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



add_theme_and_save <- function(p, fig_path, case_study, chart_name, avoid_overlap_x_axis = FALSE, legend_pos = "top", ppt_version = FALSE) {
  
  if (ppt_version == TRUE) {
    FONT_FAMILY <- "Arial"
    clarifier <- "ppt_"
  } else {
    clarifier <- ""
  }
  
  
  output_file <- paste0(fig_path, case_study, "_", clarifier, chart_name, ".png")
  
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
          legend.position = legend_pos,
          legend.justification = "center",
          # legend.title = element_text(family = FONT_FAMILY, size = 10),
          legend.title = element_blank(),
          legend.text = element_text(family = FONT_FAMILY, size = 8),
          strip.text = element_text(family = FONT_FAMILY, size = 10)  # Facet title font
    )
  
  
  # Remove .png extension if present
  base_output_file <- sub("\\.png$", "", output_file)
  
  # Save as PNG (keeping the original output_file name)
  if (ppt_version == TRUE) {
    ggsave(output_file, plot = p, width = 18, height = 12, units = "cm", dpi = 300)
  } else {
    ggsave(output_file, plot = p, width = 18, height = 14, units = "cm", dpi = 300)
  }
  # 
  # 
  # # Save as SVG
  # ggsave(paste0(base_output_file, ".svg"), plot = p, width = 18, height = 14, units = "cm")
  
  return(p)
}