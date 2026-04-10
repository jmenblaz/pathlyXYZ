

# ------------------------------------------------------------------------------
# pathlyXYZ - plot_rstack3d.R
# ------------------------------------------------------------------------------

#' Plot a RasterStack or SpatRaster in 2.5D/3D
#'
#' `plot_rstack3d()` is a custom modifywrapper around the `layer` package that allows
#' quick and easy visualization of raster stacks containing vertical information
#' (e.g., depth, altitude, or other continuous variables) in a 2.5D perspective.
#'
#' The function tilts and stacks each layer in the raster along the vertical axis,
#' optionally shifts layers up or down, and applies a color palette for better
#' differentiation. It is particularly useful for ecological or environmental
#' applications such as habitat suitability, species utilization distributions (UDs),
#' and other continuous spatial variables.
#'
#''pathlyXYZ' added features:
#'
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
#' @param z_limits Controls how color scales are calculated across the stack:
#'         - `"auto"` (Default): Calculates a global min/max across all layers. Ideal for comparing
#'           intensities (e.g., SDMs, UDs) across different depths/altitudes.
#'         - `c(min, max)`: A fixed numeric vector (e.g., `c(0, 1)`). Useful for probabilities or
#'           standardized indices (e.g., normalized variables, such us cumulative impacts; 0 - 1)
#'         - `NULL`: Each layer uses its own independent scale. Recommended when layers have
#'           different units (e.g., Temperature vs Salinity).
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





# -----------------------------------------------------------------------------
# helpers

#' Internal function to plot tilted maps with a shared custom color ramp, as in
#' 'layer' package
#'
#' This fixes the issue where each layer in layer::plot_tiltedmaps gets its
#' own independent color scale, making comparisons impossible between tilted layers,
#' in order to plot 2.5D continuos variables stack (SDM, UD, etc).
#'
#' @keywords internal

plot_tiltedmaps_helper <- function(map_list, colramp, limits = "auto", alpha = 1) {

  if (length(alpha) == 1) alpha <- rep(alpha, length(map_list))

  # Lógica de límites
  if (is.character(limits) && limits == "auto") {
    # Extraer todos los valores de todas las capas para sacar el rango global
    # extract all vlaues of stack layer to obtain range
    # minmax of all stack
    all_values <- unlist(lapply(map_list, function(x) x$value))
    render_limits <- range(all_values, na.rm = TRUE)
  } else {
    render_limits <- limits # Puede ser c(min, max) o NULL
  }

  p <- ggplot2::ggplot()

  for (i in seq_along(map_list)) {

    # Si limits es NULL, forzamos escalas independientes usando new_scale
    #  If limit null, use independt scale using new scale (same logic than 'layer')

    if (is.null(limits) || i > 1) {
      p <- p +
        ggnewscale::new_scale_fill() +
        ggnewscale::new_scale_color()
    }

    p <- p +
      ggplot2::geom_sf(
        data = map_list[[i]],
        ggplot2::aes(fill = .data[["value"]], color = .data[["value"]]),
        alpha = alpha[i],
        size = 0.01
      )

    # Aplicar color solo si se definió colramp
    #  apply custom ramp color
    if (!is.null(colramp)) {
      p <- p +
        ggplot2::scale_fill_gradientn(
          colors = colramp,
          limits = render_limits, # Si es NULL, ggplot lo calcula por capa
          guide  = "none",
          oob    = scales::squish
        ) +
        ggplot2::scale_color_gradientn(
          colors = colramp,
          limits = render_limits,
          guide  = "none",
          oob    = scales::squish
        )
    }
  }
  return(p + ggplot2::theme_void())
}



# -----------------------------------------------------------------------------
# function (main)

plot_rstack3d <- function(rstack,
                          n_layers = NULL,
                          idx_layers = NULL,
                          y_tilt = 1.25,
                          y_layer_shift = 10,
                          angle_rotate = pi/20,
                          cols = NULL,
                          alpha = 1,
                          colramp = NULL,
                          z_limits = "auto", # 'auto', 'custom limits: c(0,1)'
                                             # or c(min,max) values based on values of stack
                          # legend = FASLE,
                          parallel = TRUE,
                          top_bottom = "up") {

  # Dependencies check -----------------------------
  if (!requireNamespace("layer", quietly = TRUE)) stop("Package 'layer' is required.")
  if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is required.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' required.")
  if (!requireNamespace("ggnewscale", quietly = TRUE)) stop("Package 'ggnewscale' required.")

  stopifnot(inherits(rstack, "RasterStack") | inherits(rstack, "SpatRaster"))
  stopifnot(top_bottom %in% c("up","bottom"))
  # ------------------------------------------------

  # Processing stack for tilted and plot each layer individually ------

  # check rstack class / using terra (transform)
  # for use terra or raster package
  if (inherits(rstack, "RasterStack")) rstack <- terra::rast(rstack)  # transfrom for terra

  # create a list of layers for tilting --------------------------

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

  # processing tilt layers -----------------------------------------
  # note: could take some minutes in processing (See function documentation)

  tilt_list <- list()

  tilt_list <- list()
  for (i in seq_along(layers_to_process)) {
    lyr <- layers_to_process[[i]]

    # Calculate vertical shift
    direction <- if (top_bottom == "up") -1 else 1
    y_shift <- (i - 1) * y_layer_shift * direction

    tilt_layer <- layer::tilt_map(lyr,
                                  angle_rotate = angle_rotate,
                                  y_shift = y_shift,
                                  y_tilt = y_tilt,
                                  parallel = parallel)
    tilt_list[[i]] <- tilt_layer
  }


  # plot tilt layers ----------------------------------------------------
  # main differences with 'layer' package

  # If colramp is provided by user,
  # use our custom internal function to fix the 0-1 scale issue

  if (!is.null(colramp)) {
    p <- plot_tiltedmaps_helper(
      map_list = tilt_list,
      colramp  = colramp,
      limits   = z_limits, # "auto", c(min, max), o NULL
      alpha    = alpha
    )
  } else {
    # Si el usuario quiere escalas independientes pero con paletas
    # predefinidas (viridis, etc)
    # El paquete 'layer' original ya hace esto por defecto.
    pal_name <- if (!is.null(cols)) cols else "viridis"
    p <- layer::plot_tiltedmaps(tilt_list, palette = pal_name, alpha = alpha)
  }

  return(p)
}






























