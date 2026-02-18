
# pathlyXYZ - utils_plot.R




# helpers
# - .get paletter colors
# - .plot_titedmaps_pathlyXYZ

# some example of ramp colors in:
# https://r-charts.com/color-palettes/

.get_paletteer_colors <- function(palette, n) {

  if (!requireNamespace("paletteer", quietly = TRUE)) {
    stop("Package 'paletteer' is required for palette-based coloring")
  }

  if (!grepl("::", palette)) {
    palette <- paste0("grDevices::", stringr::str_to_title(palette))
  }

  paletteer::paletteer_c(palette, n)
}

