


# simple 3D plots using rgl function

plot_path3d <- function(x, y, z, add = FALSE, color = NULL) {
  
  # check
  if (length(x) != length(y) || length(x) != length(z)) {
    stop("X, Y and Z must have the same length")
  }
  
  n <- length(x) # path points
  
  # ---- colors -----------------------------------------------------------
  # pathlyXYZ ramp colors
  
  if (is.null(color)) {
    cols <- rep("grey20", n)
    col_start <- "black"
    col_end   <- "black"
  }
  
  if (!is.null(color) && color == "rg") {
    cols <- colorRampPalette(
      c("#9ACD32", "#CAFF70", "#FF7256", "#CD5B45")
    )(n)
    
    col_start <- "#698B22"
    col_end   <- "#8B3E2F"
  }
  
  # paletteer (optional dependency)
  
  # for paletter defalt ramp colors
  if (!is.null(color) && !color %in% c("rg")) {
    library(paletteer)
    cols <- .get_paletteer_colors(color, n)
    col_start <- cols[1]
    col_end   <- cols[n]
  }
  
  # ---- plot -------------------------------------------------------------
  if (!add) {
    rgl::open3d()
  }
  
  # start / end points
  rgl::points3d(x[1],  y[1],  z[1],
                col = col_start, size = 10)
  
  rgl::points3d(x[n], y[n], z[n],
                col = col_end, size = 10)
  
  # path points
  rgl::points3d(x, y, z, col = cols, size = 6)
  
  # path line
  rgl::lines3d(x, y, z, col = cols, lwd = 3)
  
  rgl::axes3d(bbox = TRUE)
  
  invisible(NULL)
}
