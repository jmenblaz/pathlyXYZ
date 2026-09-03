# ------------------------------------------------------------------------------
# sim_background3d.R
# ------------------------------------------------------------------------------
#
# Generation of 3D Pseudo-Absences by Sampling Background methods
#
# Reference:
# Wilson et al. (2026). Temporal and spatial transferability in telemetry-based
# dynamic species distribution models: The effects of algorithms and
# pseudo-absence techniques. Ecological Modelling.
# https://www.sciencedirect.com/science/article/pii/S0304380026002309
# "Background sampling is preferable to other pseudo-absence techniques."
#
# Author: Javier Menéndez-Blázquez | @jmenblaz
# Package: pathlyXYZ
#
# This script contains:
#
# Main function:
# - sim_background3d
#
# Helper and internal functions:
# - .sample_bbox3d
# - .sample_mcp3d
# - .sample_env_stack3d
# ------------------------------------------------------------------------------


#' Simulate 3D Background Points for Species Distribution Modeling
#'
#' Generates 3D background points from 3D animal tracking data
#' @param track Data frame, \code{sf} object, or \code{Trajectory3D} object
#'   containing 3D coordinates (X, Y, Z / Depth).
#' @param n_points Integer. Number of background points to simulate (default: 1000).
#' @param method Character. Strategy for 3D background generation:
#'   \itemize{
#'     \item \code{"bbox"}: Uniform sampling within the 3D bounding box (bbox) of the track.
#'     \item \code{"mch"}: Sampling within the 3D Minimal Convex Volume / Hull.
#'   }
#'
#'
#'
#'
#'
#' @param z_bounds Numeric vector of length 2 \code{c(min_depth, max_depth)}.
#'   Optional vertical boundaries to constrain background point generation.
#' @param quiet Logical. If \code{FALSE}, displays progress and status messages.
#' @param ... Additional arguments passed to internal sampling methods.
#'
#' @return An \code{sf} object or \code{data.frame} containing the simulated 3D
#'   background points with X, Y, Z coordinates and a presence/absence flag (\code{pa = 0}).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic background sampling within 3D bounding box
#' bg_pts <- sim_background3d(track = my_track3d, n_points = 1000, method = "random")
#'
#' # Sampling constrained by environmental raster stack and bathymetry/depth bounds
#' bg_pts <- sim_background3d(track = my_track3d,
#'                            env_stack = ocean_rstack3d,
#'                            n_points = 2000,
#'                            method = "grid3d",
#'                            z_bounds = c(-200, 0))
#' }


sim_background3d <- function(track,
                             n_points = 1000,
                             method = c("bbox", "mch"),
                             z_bounds = NULL,
                             quiet = FALSE,
                             ...) {
  # QC
  # for potential internal function...
  method <- match.arg(method)  # QC method selection

  # 2. QC - class
  if (!is.data.frame(track) && !inherits(track, "Trajectory3D")) {
    stop("Input 'track' must be a data.frame or Trajectory3D object containing x, y, z coordinates")
  }

  # 3. QC - x, y, z coordinates
  req_cols <- c("x", "y", "z")
  if (!all(req_cols %in% names(track))) {
    stop("The track object must contain 'x', 'y', and 'z' columns.")
  }

  # info
  if (!quiet) {
    message(paste0("Generating ", n_points, " 3D background points using method: '", method, "'..."))
  }

  # ---------------------------------------------------------------------------
  # 1. bbox   -------------------------------------------------
  if (method == "bbox") {

    # spatial range - x, y
    x_range <- range(track$x, na.rm = TRUE)
    y_range <- range(track$y, na.rm = TRUE)

    # vertical range - z
    if (!is.null(z_bounds)) {
      if (length(z_bounds) != 2 || !is.numeric(z_bounds)) {
        stop("'z_bounds' must be a numeric vector of length 2: c(min_z, max_z).")
      }
      z_range <- z_bounds
    } else {
      z_range <- range(track$z, na.rm = TRUE)
    }

    # background sampling (bbox)
    bg_df <- data.frame(
      x = stats::runif(n_points, min = x_range[1], max = x_range[2]),
      y = stats::runif(n_points, min = y_range[1], max = y_range[2]),
      z = stats::runif(n_points, min = z_range[1], max = z_range[2])
    )

    # Random metada from original track (time [date] following Wilson et al., 2026)
    # Asignación aleatoria de metadatos del track (time, date, etc. estilo Wilson et al., 2026)
    # use setdiff in case other metadata or names (see Sequeira et al., 2021)
    ignore_cols <- c("x", "y", "z", "polar", "displacement", "displacementTime")
    extra_cols <- setdiff(names(track), ignore_cols)

    if (length(extra_cols) > 0) {
      sampled_indices <- sample(seq_len(nrow(track)), size = n_points, replace = TRUE)
      for (col in extra_cols) {
        bg_df[[col]] <- track[[col]][sampled_indices]
      }
    }

    # Añadir identificador de pseudo-ausencia / background
    bg_df$pa <- 0

    if (!quiet) {
      message("3D Background sampling (bbox) completed successfully.")
    }

    # --------------------------------------------------------------------------
    # 2. Method - Minimum Convex Hull/Volume (mch) -----------------------------
    # steps - simulated points in bbox of track and then select those
    # are within the volume of the hull

    # QC - Packages
      if (!requireNamespace("geometry", quietly = TRUE)) {
        stop("Package 'geometry' is required for 3D Minimum Convex Hull sampling.")
      }

    # extract 3D path points for calculate Minimum Convex Hull
    pts_obs <- as.matrix(track[, c("x", "y", "z")])
    hull <- geometry::convhulln(pts_obs)  # Minimum convex hull - mch (points)

    # bbox (x,y)
    x_range <- range(pts_obs[, "x"], na.rm = TRUE)
    y_range <- range(pts_obs[, "y"], na.rm = TRUE)

    if (!is.null(z_bounds)) {
      if (length(z_bounds) != 2 || !is.numeric(z_bounds)) {
        stop("'z_bounds' must be a numeric vector of length 2: c(min_z, max_z).")
      }
      z_range <- z_bounds
    } else {
      z_range <- range(pts_obs[, "z"], na.rm = TRUE)
    }

    # Rejection Sampling Loop:
    # create points within bounding box until full the hull
    # generar puntos dentro de la bounding box hasta llenar el Hull
    accepted_pts <- matrix(numeric(0), ncol = 3, dimnames = list(NULL, c("x", "y", "z")))
    batch_size <- max(n_points * 2, 1000) # Lote para acelerar el proceso

    while (nrow(accepted_pts) < n_points) {

      # Candidate points uniformes en 3D
      cand <- cbind(
        x = stats::runif(batch_size, min = x_range[1], max = x_range[2]),
        y = stats::runif(batch_size, min = y_range[1], max = y_range[2]),
        z = stats::runif(batch_size, min = z_range[1], max = z_range[2])
      )

      # Test: comprobar qué candidatos caen DENTRO del Convex Hull 3D
      inside <- geometry::inhulln(hull, cand)

      if (any(inside)) {
        accepted_pts <- rbind(accepted_pts, cand[inside, , drop = FALSE])
      }
    }

    # Recortar al número exacto de puntos solicitado (n_points)
    bg_df <- as.data.frame(accepted_pts[1:n_points, ])



    # Asignación aleatoria de metadatos del track (time, date, etc. estilo Wilson et al., 2026)
    ignore_cols <- c("x", "y", "z", "polar", "displacement", "displacementTime")
    extra_cols <- setdiff(names(track), ignore_cols)

    if (length(extra_cols) > 0) {
      sampled_indices <- sample(seq_len(nrow(track)), size = n_points, replace = TRUE)
      for (col in extra_cols) {
        bg_df[[col]] <- track[[col]][sampled_indices]
      }
    }

    # Identificador de pseudo-ausencia / background
    bg_df$pa <- 0








    # plot ----------------------------------------------------------
    if (plot == TRUE) {
      # plot track
      pathlyXYZ::plot_path3d(path3D$x, path3D$y, path3D$z, color = "viridis")

      # plot background points generated
      rgl::points3d(bg_df$x, bg_df$y, bg_df$z,
                    col = adjustcolor("grey80", alpha = 0.5),
                    size = 4)
    }


    # info
    if (quiet =! TRUE) {
      cat("")

    }

    # return
    return(bg_df)
  }
}


















# testing ---------------------------------------------------------------------
path3D <- gen_path(n = 250, dim = "3D", stepLength = 1, random = FALSE, fps = 1)

plot_path3d(path3D$x, path3D$y, path3D$z, color = "viridis")










