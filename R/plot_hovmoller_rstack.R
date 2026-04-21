

# ------------------------------------------------------------------------------
# pathlyXYZ - plot_hovmoller_rstack.R
# ------------------------------------------------------------------------------

# @jmenblaz / J. Menéndez-Blázquez

#' Hovmöller plot (Vertical section) from raster stack of continuous values layered
#'  along line / transect across Z dimension
#'
#' `plot_hovmoller_rstack()` visualizes a vertical cross-section of spatial data
#' (SpatRaster/RasterStack) along a specific transect (sf LINESTRING). It interpolates
#' values between layers.
#' (e.g., Copernicus Marine Services environmental variables, 3D Habitat suitability, etc)
#'
#' @param rstack A `terra::SpatRaster` or `raster::RasterStack` with multiple layers
#' representing different depths or altitudes.
#' @param tr An `sf` object (LINESTRING) representing the transect path.
#' @param z_values Numeric. Either a single value (equidistant spacing) or a
#' vector of length `nlyr(rstack)` specifying the depth/altitude/distance of each
#' raster stack layer (z value (coordinate).
#' @param quiet Logical. If `TRUE`, suppresses status messages. Default `FALSE`.
#' @param visual_info Logical. If `TRUE`, plots a 2D map showing the transect alignment
#' and quality control of the extent.
#' @param pts_res Numeric. Resolution factor for point sampling along the transect.
#' Default `NULL` (uses raster resolution). Higher values increase point density.
#' @param z_units Character. Label for the Z-axis (e.g., "Depth (m)" or "Altitude (m)").
#' @param nx Integer. Number of points in the X-axis for interpolation. Default: `nrow(pts)`.
#' @param ny Integer. Number of points in the Y-axis for interpolation.
#' Default, number of z values: `ny = max(z_values_final) / 1.1`.
#' @param plot Logical. If `TRUE`, displays the plot immediately in the enviroment.
#' @param plot_relation Numeric fraction of the aspect ratio of the final plot.
#' Default `1:1`, but it can be change based on user preferences.
#' @param colramp A color palette or vector of colors for the plot. Default `viridis::viridis(100)`.
#' @param colramp_breaks Numeric. Number of breaks for color ramp (this parameter is linked to contours)
#' @param val_range Vector. `NULL` by default, (1) calculate by min and max stack values;
#' or (2) custom values range for plot e.g.: c(0,1).
#' @param rev Logical. If `FALSE` (default), reverses or not the Y-axis (useful for depth profiles).
#' @param contour Logical `FALSE` (default), If `TRUE`, adds contour lines to the plot, and
#' then change the next @param: contour_col, contour_alpha, contour_lwd.
#' @param contour_col Color of the contour lines. Default `"black"`.
#' @param contour_alpha Numeric (0-1). Transparency of contour lines.
#' @param contour_lwd Numeric. Line width of contours.
#' @param x_ticks Numeric. Number of label sticks in X axi for coordinates along
#' @param y_ticks Numeric. Desired number of intervals for the Y-axis.
#' @param cex_y Numeric. Font size for label in Y axi.
#' @param cex_x Numeri. Font size for label in X axi.
#' The actual number of labels is dynamically optimized to provide "pretty" (rounded) values.
#' @param margin_plot Character. Internal margin plots to show. Default `"none"`, `"top"`, `"righ"`
#' @param zlim Numeric vector `c(min, max)` for the Y-axis limits.
#'
#' @param ... Additional arguments passed to `rasterVis::levelplot`.
#'
#' @return A `trellis` object (from `lattice` via `rasterVis`).
#'
#' @details
#' The function follows a streamlined workflow:
#'
#' 1. **Spatial Alignment**: Synchronizes CRS between the transect and raster stack,
#'    clipping the line to the data extent to ensure valid extraction.
#'
#' 2. **Transect Discretization**: Segments the LINESTRING into equidistant points.
#'    Sampling density is controlled by `pts_res` relative to the raster's
#'    horizontal resolution.
#'
#' 3. **Vertical Interpolation**: Extracts stack values at each point and generates
#'    a "Distance vs. Depth" grid. It uses the `akima` algorithm to interpolate
#'    between discrete vertical bins, creating a continuous cross-section.
#'
#' 4. **Visual Rendering**: Uses `rasterVis::levelplot` for plotting, but pathlyXYZ
#'    implemented label text with geographic coordinates (Lat/Lon) on the X-axis,
#'    and choose the scale values for the color ramp.
#'
#'
#'
#' @examples
#' \donttest{
#' library(terra)
#' library(sf)
#'
#' # 1. Create a SpatRaster stack from 'volcano' dataset
#' r <- terra::rast(volcano)
#' # Set fake extent and CRS for spatial operations
#' terra::ext(r) <- c(-0.1, 0.1, -0.1, 0.1)
#' terra::crs(r) <- "EPSG:4326"
#'
#' # Create a stack simulating 6 depth/altitude layers
#' rstack <- c(r, r * 0.85, r * 0.7, r * 0.5, r * 0.3, r * 0.1)
#' names(rstack) <- paste0("layer_", 1:6)
#'
#' # 2. Create a transect line within the raster extent
#' p1 <- matrix(c(-0.08, -0.08), ncol = 2)
#' p2 <- matrix(c(0.08, 0.08), ncol = 2)
#' tr <- sf::st_linestring(rbind(p1, p2)) %>%
#'   sf::st_sfc(crs = "EPSG:4326") %>%
#'   sf::st_as_sf()
#'
#' # 3. Run pathlyXYZ custom Hovmoller or Vertical section plot
#' p <- plot_hovmoller_rstack(
#'   rstack = rstack,
#'   tr = tr,
#'   z_values = 50,          # 50m between each rstack layer
#'   colramp_breaks = 20,
#'   rev = TRUE,             # Reverse Y-axis for depth
#'   z_units = "Depth (m)",
#'   contour = TRUE,
#'   contour_col = "black",
#'   x_ticks = 3,
#'   visual_info = TRUE,     # Show the QC map
#'   plot = TRUE
#' )
#' }
#'-----------------------------------------------------------------------------
#'
#' @export
plot_hovmoller_rstack <- function(rstack, tr, z_values,
                                  quiet = FALSE,
                                  visual_info = FALSE,
                                  # resolution (distance between points sample)
                                  pts_res = NULL,
                                  # z units (meters, km, etc)
                                  z_units = NULL,
                                  # interpolate parameters
                                  nx = NULL,
                                  ny = NULL,    # DEFAULT
                                  # plot
                                  plot = TRUE, # plot directly
                                  plot_relation = 1:1,
                                  colramp = viridis::viridis(100),
                                  colramp_breaks = NULL,
                                  val_range = NULL,
                                  rev = FALSE, # logic,
                                  contour = FALSE, # False logic
                                    contour_col = "black",
                                    contour_alpha = 0.4,
                                    contour_lwd = 0.5,  # line width
                                  margin_plot = 'none', # 'none', 'top', 'right'
                                  # labels and font size
                                  y_ticks = 5,
                                  x_ticks = 5,
                                  cex_y = 0.8,
                                  cex_x = 0.7,
                                  # Y limit plot
                                  zlim = NULL,  # custom limits for Y axy c(min,max)

                                  ...) {


  # Dependencies check -----------------------------
  if (!requireNamespace("sf", quietly = TRUE)) stop("El paquete 'sf' es requerido para datos vectoriales.")
  if (!requireNamespace("terra", quietly = TRUE)) stop("El paquete 'terra' es requerido (sucesor de raster).")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("El paquete 'dplyr' es requerido para manipulación de datos.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("El paquete 'ggplot2' es requerido para visualización.")
  if (!requireNamespace("tidyverse", quietly = TRUE)) stop("El paquete 'tidyverse' es requerido.")
  if (!requireNamespace("ggnewscale", quietly = TRUE)) stop("El paquete 'ggnewscale' es requerido para múltiples leyendas.")
  if (!requireNamespace("tidyterra", quietly = TRUE)) stop("El paquete 'tidyterra' es requerido para usar terra con ggplot2.")
  if (!requireNamespace("rasterVis", quietly = TRUE)) stop("El paquete 'rasterVis' es requerido para métodos de visualización avanzada.")


  # Pre-processing for Hovmoller plot into transect (line) in the rstack exten area
  # check raster clas
  stopifnot(inherits(rstack, "RasterStack") | inherits(rstack, "SpatRaster"))


  # QC - control point --------------------------------------------------------
  # check rstack class / using terra (transform)
  # for use terra or raster package
  if (inherits(rstack, "RasterStack")) rstack <- terra::rast(rstack)  # transfrom for terra

  # Check same CRS and transectextension of transect and rstack

  crs_rstack <- terra::crs(rstack, proj = TRUE)
  crs_transect <- sf::st_crs(tr)$proj4string

  # QC - 1 CRS
  if (crs_rstack != crs_transect) {
    if (!quiet) message("⚠️ CRS mismatch. Transform transect/line into stack CRS...")
    tr <- sf::st_transform(tr, terra::crs(rstack))
  } else {
    if (!quiet) cat("✅ Same CRS")
  }

  # QC - 2. Comprobar extensión (Está el transecto dentro?)
  # QC - 2. Check extension (is the transect inside the rstack¿)

  # 2.1 Convertimos la extensión del raster a un polígono sf
  # 2.1 Convert extent of rstack into polygon for fuerther analysis
  r_ext <- terra::ext(rstack) %>%
    terra::as.polygons() %>%
    sf::st_as_sf()

  sf::st_crs(r_ext) <- terra::crs(rstack) # re-asign CRS

  # QC 3 - Intersection between tr and rstack extent
  # Note: tr must be a linestring (unique)
  within_check <- sf::st_within(tr, r_ext, sparse = FALSE)
  intersects_check <- sf::st_intersects(tr, r_ext, sparse = FALSE)

  # QC 4 - results of quality control
  status_msg <- ""

  if (!quiet) {

    if (all(within_check)) {
      status_msg <- "TOTAL OVERLAP"
      message("✅ QC extent: transect within the stack extent\n")
      col_line <- "#9ACD32" # YellowGreen
      col_ext  <- "#698B22" # OliveDrab

    } else if (any(intersects_check)) {
      status_msg <- "PARTIAL OVERLAP"
      message("⚠️ QC extent: transect within the stack extent partially\n")
      col_line <- "orange"
      col_ext  <- "orange3"

    } else {
      status_msg <- "NO OVERLAP"
      message("❌ ERROR: The transect is COMPLETELY outside the rstack extent\n")
      col_line <- "#CD4F39" # Tomato
      col_ext  <- "#8B3626" # Firebrick
    }
  }

  if (visual_info) {
    # base
    plot(rstack[[1]],
         col = colorRampPalette(c("grey97", "grey60", "grey20"))(100))
    # extent
    plot(terra::ext(rstack),
         add = TRUE,
         border = col_ext,
         lwd = 10)

    # Line
    plot(sf::st_geometry(tr), add = TRUE, col = "black", lwd = 5)
    plot(sf::st_geometry(tr), add = TRUE, col = col_line, lwd = 3)

    # 4. Títulos dinámicos
    title(main = "QC: Transect vs Raster Extent Alignment", line = 3)
    title(sub = paste("Status:", status_msg), col.sub = col_ext, font.sub = 2)
  }


  if (status_msg == "NO OVERLAP") {
    stop("\n")
  }

  # clip transect / line into raster extent
  if (any(intersects_check) & !all(within_check)) {
    tr <- sf::st_intersection(tr, r_ext)
  }



  # ------------------------------------------------------------------

  # it is independ of the CRS units (degrees or meters)
  # resolution of strack
  res_stack <- terra::res(rstack)[1]

  # Pre-processing - Create points along transect  -------------------------------
  # segmentar line en puntos
  # segment line into points baded on distance
  # use planer proyection (without CRS) to avoid curvature problem if use sf (WGS)

  original_crs <- sf::st_crs(tr)
  tr_no_crs <- tr
  sf::st_crs(tr_no_crs) <- NA

  # Segmentation
  if (is.null(pts_res)) pts_res <- 1
  # (perfect straight line, between number (or coords))
  pts_no_crs <- sf::st_segmentize(tr_no_crs, dfMaxLength = res_stack / pts_res)  # Usa la res en grados
  pts_no_crs <- sf::st_cast(pts_no_crs, "POINT")

  # Original CRS
  pts <- sf::st_as_sf(pts_no_crs)
  sf::st_crs(pts) <- original_crs

  # add points into visual info
  if (visual_info) {
    plot(rstack[[1]],
         col = colorRampPalette(c("grey97", "grey60", "grey20"))(100))
    # plot(st_geometry(tr))
    plot(sf::st_geometry(pts),
         add = TRUE, col = "red", cex = 0.2)
  }

  # calculate distance in the transect (for further interpolation)
  # I can't use latitude or longitude for stadarization
  # if use latitud or longitud, depends of line horientation, the interpolation
  # of akima will be not precise (in some case, almost perfect line N-S or W-E,
  # the results are cohrent, but not in a mix)

  # Extract values into pts for layers -------------------------------------------
  names(rstack) <- as.character(1:nlyr(rstack))  # layer names for Hovmoller plot
  vals <- terra::extract(rstack,  terra::vect(pts))

  # add coordinates into vals
  coords <- terra::crds(terra::vect(pts))
  vals$longitude <- coords[,1]
  vals$latitude <- coords[,2]

  # add distance between points
  pts$dist_m <- as.numeric(sf::st_distance(pts, pts[1,]))
  vals$dist_m <- pts$dist_m  # <--- Fundamental


  # Post - processing for plotting ---------------------------------------------
  # create long dataframe for plot rstack --------------------------------------
  # interpolation and asig z values for each stack layer

  # z values, different options based on user preferences:
  if (length(z_values) == 1) {
    # A: Distancia única (equidistante)
    # A: Unique distance (equidistant)
    # From 0 to
    if (!quiet) cat(paste("ℹ️ Generated equidistant z_layers every", z_values, "units\n"))
    z_values_final <- seq(from = 0, by = z_values, length.out = terra::nlyr(rstack))

  } else if (length(z_values) == terra::nlyr(rstack)) {
    # Opción B: Vector de distancias personalizadas (como tu depth_mid)
    if (!quiet) cat("✅ Provided z_values vector used\n")
    z_values_final <- z_values

  } else {
    stop(paste("❌ Error: 'z values' must be either a single numeric value (for equidistance)",
               "or a vector with length equal to the number of stack layers (", terra::nlyr(rstack), ")\n"))
  }


  # Create long dataframe for (1) interpolate and (2) plot --------
  vals_long <- vals %>%
    pivot_longer(
      cols = matches("^[0-9]+$"),
      names_to = "layer_index",
      values_to = "value"
    ) %>%
    mutate(
      layer_index = as.numeric(layer_index),
      # asign Z value
      z_layer = z_values_final[layer_index]
    ) %>%
    filter(!is.na(value), !is.infinite(value))


  # interpolate values between layer based on z values  --------------------------
  if (is.null(nx)) nx = nrow(pts) # x matrix == points (latitud)
  if (is.null(ny)) ny = max(z_values_final) / 1.1

  # interpolate using akima R package
  interp_res <- suppressWarnings(with(vals_long,
                     akima::interp(
                       x = dist_m,
                       y = z_layer,
                       z = value,
                       nx = nx, # x matrix == points (latitud)
                       ny = ny,   # y matrix (z dimension)
                       duplicate = "mean"
                     )))

  # convert to raster (not terra, works propertly with levelplot)
  r <- raster::raster(list(x = interp_res$x, y = interp_res$y, z = interp_res$z))



  # ----------------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  # Plot -----------------------------------------------------------------------

  # check plot parameters
  if (is.null(z_units)) z_label <- " "
  if (!is.null(z_units)) z_label <- z_units # e.g., Depth (m)
  if (is.null(zlim)) zlim <- c(0, (max(z_values_final) + 0.1))  # add 0.1 for improve plot visualization limits

  # Rev raster for correct vertical horientation (depth/altited)
  # and include the marginal plots associate
  if (rev) {
    r <- raster::flip(r, direction = "y")
  }


  # # custom ticks
  # extract coordinates from 5 pts in the trasect / line for plotting ----------
  # add coordinates into X axy plot
  # if (plot_coordinates) {

  # coordinates
  coords_mat <- sf::st_coordinates(pts)

  idx <- round(seq(1, nrow(pts), length.out = x_ticks))
  at_pos <- pts$dist_m[idx]

  # coordinates in the idx
  lats <- round(coords_mat[idx, 2], 1)
  lons <- round(coords_mat[idx, 1], 1)

  labels_x <- paste0(lats, "°", ifelse(seq_along(lats) == length(lats), " Lat", ""), "\n",
                     lons, "°", ifelse(seq_along(lons) == length(lons), " Lon", ""))
  # scales
  # }


  # # values range and colramp breaks for improve plot visualization ------
  if (!is.null(colramp_breaks)) {
    n_colors <- colramp_breaks
    # Re-generamos la rampa para que coincida exactamente con los breaks pedidos
    colramp <- colorRampPalette(colramp)(n_colors)
  } else {
    n_colors <- length(colramp)
  }

  # minmax values of terra
  r_min <- terra::minmax(terra::rast(r)) [1]
  r_max <- terra::minmax(terra::rast(r)) [2]


  # En lattice/levelplot, necesitamos n_colors + 1 puntos de corte
  if (is.null(val_range)) {
    # automatic values range
    at_breaks <- seq(r_min, r_max, length.out = n_colors + 1)
  } else if (length(val_range) == 2) {
    # custom values range
    at_breaks <- seq(val_range[1], val_range[2], length.out = n_colors + 1)
  } else {
    # values range by user
    at_breaks <- val_range
    if (length(at_breaks) - 1 != length(colramp)) {
      colramp <- colorRampPalette(colramp)(length(at_breaks) - 1)
    }
  }


  # for rev plots and improve Y axi label plots
  y_ticks_coords <- pretty(z_values_final, n = y_ticks)


  # Plot Hovmöller plot using levelplot ----------------------------------------
  # rasterVis wrapper and levelplot function()
  p <- rasterVis::levelplot(r,
                            ylab = z_label,      # label axy Y or (z units)
                            ylim = zlim,
                            at = at_breaks,  # custom values range
                            # labels = TRUE,
                            # margin = list(name = "latitude", FUN = "mean"),
                            col.regions = colramp,
                            contour = contour,
                            # double labelling of coordinates (based on pts line)
                            scales = list(
                              x = list(
                                at = at_pos,      # position in the line (dist)
                                labels = labels_x,
                                cex = cex_x,
                                rot = 0),
                              y = list(cex = cex_y,
                                       at = y_ticks_coords,
                                       labels = if(rev) rev(y_ticks_coords) else y_ticks_coords
                              )
                            ),
                            # panel
                            panel = function(...) {
                              panel.levelplot(...)  # relleno
                              panel.contourplot(...,
                                                col = contour_col,
                                                alpha = contour_alpha,
                                                lwd = contour_lwd)})
  # process plot
  # aspect 1:1
  p <- update(p, aspect = plot_relation)

  # # revert plot (from up to bottom)
  # First version - only chnage raster values, not marginal plots
  # if (rev) p <- update(p, ylim = rev(p$y.limits)) # revert Y axy

  # margin plots
  margin_plot <- tolower(trimws(as.character(margin_plot)))
  if (margin_plot == "none") {
    p$legend$right <- NULL
    p$legend$top   <- NULL
  } else if (margin_plot == "top") {
    p$legend$right <- NULL
  } else if (margin_plot == "right") {
    p$legend$top   <- NULL
  }

  # plot just to apply function
  if (plot) print(p)

  return(p)
}

# ------------------------------------------------------------------------------






