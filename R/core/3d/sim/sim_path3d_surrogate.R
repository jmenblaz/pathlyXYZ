

# ---- Surrogate CRW 

#  null model condicional



# ============================================================================
# sim_path3d_surrogate.R
# ============================================================================
# Surrogate-based simulations of movement tracks
#
# This script provides functions to simulate tracks from observed trajectories
# using vector autoregressive models (surrogateARModel & surrogateAR from
# availability package). Simulations start at the same initial position
# but follow the stochastic dynamics of the fitted AR model.
#
# Author: @jmenblaz
# Date: 2026-02-16
# ============================================================================
# Dependencies: availability, data.table, dplyr
# ============================================================================


# ============================================================================
# -------------------- Internal helper functions -----------------------------
# ============================================================================

# --------------------------------------------------------------------------
# Internal helper: fit 3D surrogate AR model --------------------------------
# --------------------------------------------------------------------------
.fit_surrogate3D <- function(track, coords = c("x","y","z")) {
  
  stopifnot(all(coords %in% names(track)))
  
  X <- as.matrix(track[, coords])
  
  model <- availability::surrogateARModel(X)
  
  model
}


# --------------------------------------------------------------------------
# Internal helper: simulate one surrogate 3D track ------------------------
# --------------------------------------------------------------------------
.simulate_surrogate3D <- function(model, track, coords = c("x","y","z"), seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  sim <- availability::surrogateAR(
    model,
    xs = as.matrix(track[, coords]),
    ts = track$date,
    point.check = NULL,      # omit land mask
    fixed = rep(c(TRUE, FALSE), c(1, nrow(track)-1)),
    partial = FALSE
  )
  
  if (is.null(sim) || any(is.na(sim$xs[1, ]))) return(NULL)
  
  # convert to Trajectory3D
  df <- track
  df[, coords] <- sim$xs
  
  Traj3DFromCoords(df, xCol = coords[1], yCol = coords[2], zCol = coords[3], timeCol = "date", fps = 1)
}


# --------------------------------------------------------------------------
# Public function: simulate multiple 3D surrogate tracks ------------------
# --------------------------------------------------------------------------
#' Simulate 3D trajectories using surrogateAR (availability)
#'
#' Generates alternative 3D trajectories starting from the observed initial
#' position using a fitted 3D surrogate autoregressive model.
#'
#' @param track Trajectory3D-compatible data.frame with columns x, y, z, date
#' @param coords Names of the coordinate columns (default c("x","y","z"))
#' @param n_sim Number of simulations to generate
#' @param seed Optional random seed
#'
#' @return List of Trajectory3D objects
#' @export
simulate_3d_tracks_surrogate <- function(track, coords = c("x","y","z"), n_sim = 5, seed = NULL) {
  
  stopifnot(all(coords %in% names(track)))
  
  if (!requireNamespace("availability", quietly = TRUE)) {
    stop("Package 'availability' is required")
  }
  
  # fit model
  model <- .fit_surrogate3D(track, coords = coords)
  
  # generate simulations
  sim_list <- lapply(seq_len(n_sim), function(i) {
    .simulate_surrogate3D(
      model, track, coords = coords,
      seed = if (!is.null(seed)) seed + i else NULL
    )
  })
  
  sim_list
}


# ============================================================================
# Example -------------------------------------------------------------------
# ============================================================================
if (interactive()) {
  
  # create a mock 3D track
  pathDF <- data.frame(
    id = 1,
    date = seq.POSIXt(from = Sys.time(), by = "min", length.out = 50),
    x = cumsum(rnorm(50)),
    y = cumsum(rnorm(50)),
    z = cumsum(rnorm(50))
  )
  
  # simulate 5 surrogate 3D tracks
  sims <- simulate_3d_tracks_surrogate(pathDF, n_sim = 5, seed = 123)
  
  # plot
  plot_simpath3d(sims, original = Traj3DFromCoords(pathDF, xCol="x", yCol="y", zCol="z", timeCol="date"))
  
}





