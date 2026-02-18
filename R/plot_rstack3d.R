

# ------------------------------------------------------------------------------
# pathlyXYZ - plot_rstack3d.R
# ------------------------------------------------------------------------------

#' Plot a RasterStack or SpatRaster in 2.5D/3D
#'
#' `plot_rstack3d()` is a convenient wrapper around the `layer` package that allows
#' quick and easy visualization of raster stacks containing vertical information
#' (e.g., depth, altitude, or other continuous variables) in a 2.5D perspective.
#'
#' The function tilts and stacks each layer in the raster along the vertical axis,
#' optionally shifts layers up or down, and applies a color palette for better
#' differentiation. It is particularly useful for ecological or environmental
#' applications such as habitat suitability, species utilization distributions (UDs),
#' and other continuous spatial variables.
#'
#' @param rstack RasterStack (from `raster`) or SpatRaster (from `terra`) object containing the layers to plot.
#' @param n_layers Integer specifying the number of layers from the stack to plot.
#'        By default, all layers are plotted.
#' @param idx_layers Optional numeric vector specifying the exact indices of layers to plot.
#'        If provided, `n_layers` is ignored.
#' @param y_tilt Numeric controlling the tilt angle of the layers in the vertical (Y) direction. Default is 1.25.
#' @param y_layer_shift Numeric controlling the vertical spacing between layers. Default is 10.
#' @param angle_rotate Numeric specifying the rotation angle (radians) for tilting the layers. Default is pi/20.
#' @param cols Optional palette of colors to apply to the layers. Supports `viridis`, or `scico` (see `layer` docs)
#' @param colramp Optional custom color ramp (vector of colors) for plotting the stack (*under development*)
#' @param alpha Numeric transparency for the layers (0 = fully transparent, 1 = opaque). Default is 1.
#' @param parallel Logical; if TRUE, layer pre-processing is done in parallel to improve speed on large stacks.
#' @param top_bottom Character, either `"up"` or `"bottom"`. Determines whether the first layer in the stack
#'        appears at the top or bottom of the plot. Useful for marine (surface to depth) or aerial (ground to altitude) contexts.
#'        Default is `"up"`.
#'
#' @return A `ggplot2` object (via `layer::plot_tiltedmaps`) showing the raster layers tilted and stacked in 2.5D/3D.
#'
#' @details
#' - Internally, this function uses the `layer` package to tilt each raster layer and plot it.
#' - Layers can be selected either by index (`idx_layers`) or by number (`n_layers`).
#' - The vertical stacking effect simulates a 3D perspective, which is useful for visualizing
#'   environmental gradients or vertical distributions.
#' - The function is a wrapper that simplifies creating 2.5D visualizations without manual
#'   processing of individual layers.
#'
#' @examples
#' \donttest{
#' library(terra)
#' r <- rast(system.file("ex/logo.tif", package="terra"))
#' r <- rep(r, 3)
#' # Plot the first two layers with a tilt and vertical shift
#' plot_rstack3d(r,
#'               n_layers = 2,
#'               y_tilt = 1.5,
#'               y_layer_shift = 2.5,
#'               angle_rotate = pi/20,
#'               cols = "cividis",
#'               top_bottom = 'up')
#' }
#'
#' @export

plot_rstack3d <- function(rstack,
                          n_layers = NULL,
                          idx_layers = NULL,
                          y_tilt = 1.25,
                          y_layer_shift = 10,
                          angle_rotate = pi/20,
                          cols = NULL,
                          alpha = 1,
                          colramp = NULL,
                          # legend = FALSE,
                          parallel = TRUE,
                          top_bottom = "up") {


  if (!requireNamespace("layer", quietly = TRUE)) {
    stop("Package 'layer' is required for plot_rstack3d()")
  }
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required for plot_rstack3d()")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'terra' is required for plot_rstack3d()")
  }

  stopifnot(inherits(rstack, "RasterStack") | inherits(rstack, "SpatRaster"))
  stopifnot(top_bottom %in% c("up","bottom"))


  # Processing stack for tilted and plot each layer individually
  # check rstack class
  # using terra (transform)
  if (inherits(rstack, "RasterStack")) rstack <- terra::rast(rstack)  # transfrom for terra

  # create a list of layers for tilting ------------------

  # select stack layer based on user preferences
  # option 1 null idx_layer and n_layer null
  # option 2 idx_layer and null n_layer
  # option 3 null idx_layer and n_layer

  # priority: idx_layers > n_layers > todas
  if (!is.null(idx_layers)) {
    layers <- terra::as.list(rstack[[idx_layers]])
  } else if (!is.null(n_layers)) {
    layers <- terra::as.list(rstack[[1:min(n_layers, terra::nlyr(rstack))]])
  } else {
    layers <- terra::as.list(rstack)
  }

  # processing tilt layers
  # note: could take some minutes in processing (See function documentation)

  tilt_list <- list()

  for (i in 1:length(layers)) {
    layer  <- layers[[i]]  # list of spatRaster
    # terra::plot(layer, range = c(0,1))

    # 1. custom shift by layer based on Y shift distance and direction (top-bottom)
    if (top_bottom == "up") {
      y_shift = (((i*y_layer_shift)-y_layer_shift)*-1)
    }
    if (top_bottom == "bottom") {
      y_shift = (((i*y_layer_shift)-y_layer_shift))
    }

    # 2. create tilt layer and customize it
    if (i == 1) {
      tilt_layer <- layer::tilt_map(layer, angle_rotate = angle_rotate,
                                    y_tilt = y_tilt,
                                    parallel = parallel)
    } else {
      tilt_layer <- layer::tilt_map(layer, angle_rotate = angle_rotate,
                                    y_shift = y_shift,
                                    y_tilt = y_tilt,
                                    parallel = parallel)
    }

    # append tilt layer
    tilt_list[[i]] <- tilt_layer
  }


  # plot tilt layers -----------------------------------------

  if (!is.null(colramp)) {

    p <- layer::plot_tiltedmaps(tilt_list, palette = "viridis", alpha = alpha)
    p <- p +
      ggplot2::scale_fill_gradientn(colors = colramp) +
      ggplot2::guides(fill = "none")

  } else {

    pal_name <- if (!is.null(cols)) cols else "viridis"
    p <- layer::plot_tiltedmaps(tilt_list, palette = pal_name, alpha = alpha)

  }


  # plot using custom colramp
    # if (is.null(cols) && !is.null(colramp)) {
    #   p <- layer::plot_tiltedmaps(tilt_list)
    #
    #   # custom colramp
    #   p <- p +
    #     ggplot2::scale_fill_gradientn(colors = colramp) +
    #     ggplot2::scale_fill_gradientn(colors = colramp) +
    #     ggplot2::guides(fill = "none")
    #
    #   p
    #
    #   if (legend == FALSE) {
    #     guides(fill = "none")
    #   }
    # }

  return(p)

}

