

# get paletter colors

.get_paletteer_colors <- function(palette, n) {

  if (!requireNamespace("paletteer", quietly = TRUE)) {
    stop("Package 'paletteer' is required for palette-based coloring")
  }

  # Si el usuario pasa solo "viridis", asumimos grDevices
  if (!grepl("::", palette)) {
    palette <- paste0("grDevices::", stringr::str_to_title(palette))
  }

  paletteer::paletteer_c(palette, n)
}


