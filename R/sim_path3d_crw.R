# ------------------------------------------------------------------------------
  # sim_path3d_crw.R
# ------------------------------------------------------------------------------

# Correlated Random Walk (CRW) simulation in 3D using VAR(1) on displacements
# VAR: Vector-autoregressive movement model(1) -> t-1

# This script provides functions to simulate 3D Correlated Random Walks (CRW)
# from observed trajectories (Trajectory3D objects).

# The simulation can include or exclude a drift (mean displacement) component.
# for example, drift is usefull for simulate preferentian movements,
# migration, searching foraging, etc



# Author: Javier Menéndez-Blázquez | @jmenblaz

# Dependencies: MASS, and rgl (for plotting)

# This script contains

# Helper and internals functions
# - .fit fit VAR(1) model to 3D displacements
# - .simulate_var3d

# Main simulate CRW function
# - sim_path3d_crw



# Helper and internals functions -----------------------------------------------
# Fit VAR(1) model to 3D displacements --------------------
.fit_var3d <- function(path3d,
                       drift = FALSE,
                       ridge = 1e-6) {

  stopifnot(inherits(path3d, "Trajectory3D"))

  dx <- diff(path3d$x)
  dy <- diff(path3d$y)
  dz <- diff(path3d$z)

  D <- cbind(dx, dy, dz)

  mu <- colMeans(D)
  Dc <- if (drift) sweep(D, 2, mu) else D

  Y <- Dc[-1, , drop = FALSE]
  X <- Dc[-nrow(Dc), , drop = FALSE]

  XtX <- t(X) %*% X
  p <- ncol(XtX)

  # ridge regularization
  A <- solve(XtX + ridge * diag(p)) %*% t(X) %*% Y

  resid <- Y - X %*% A
  Sigma <- cov(resid)

  eig <- max(Mod(eigen(A)$values))
  if (eig >= 1) {
    warning("VAR(1) may be unstable (max eigenvalue >= 1)")
  }

  list(
    A     = A,
    Sigma = Sigma,
    mu    = if (drift) mu else c(0, 0, 0),
    init  = Dc[1, ]
  )
}



#  internal: simulate one 3D VAR-based CRW --------------------------
.simulate_var3d <- function(path3d, drift = FALSE, seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  model <- .fit_var3d(path3d, drift = drift)

  n_steps <- nrow(path3d) - 1
  Dsim <- matrix(NA, n_steps, 3)

  Dsim[1, ] <- model$init

  for (i in 2:n_steps) {
    eps <- MASS::mvrnorm(1, mu = c(0, 0, 0), Sigma = model$Sigma)
    Dsim[i, ] <- Dsim[i - 1, ] %*% model$A + eps
  }

  # re-add drift if needed
  Dsim <- sweep(Dsim, 2, model$mu, "+")

  # reconstruct coordinates
  x_sim <- c(path3d$x[1], path3d$x[1] + cumsum(Dsim[, 1]))
  y_sim <- c(path3d$y[1], path3d$y[1] + cumsum(Dsim[, 2]))
  z_sim <- c(path3d$z[1], path3d$z[1] + cumsum(Dsim[, 3]))

  trj_sim <- path3d
  trj_sim$x <- x_sim
  trj_sim$y <- y_sim
  trj_sim$z <- z_sim

  trj_sim$displacement <- c(
    0 + 0i,
    complex(real = diff(x_sim), imaginary = diff(y_sim))
  )
  trj_sim$polar <- trj_sim$displacement

  trj_sim
}




# Main simulate CRW function ---------------------------------------------------

#' Simulate 3D correlated random walks using VAR(1)
#'
#' @param path3d Trajectory3D object
#' @param n_sim Number of simulations
#' @param drift Logical; include observed mean displacement
#' @param seed Optional seed
#'
#' @return List of Trajectory3D objects
#' @export


sim_path3d_crw <- function(path3d,
                           n_sim = 5,
                           drift = FALSE,
                           seed = NULL) {

  stopifnot(inherits(path3d, "Trajectory3D"))

  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package 'MASS' is required")
  }

  lapply(seq_len(n_sim), function(i) {
    .simulate_var3d(
      path3d,
      drift = drift,
      seed = if (!is.null(seed)) seed + i else NULL
    )
  })
}








# -----------------------------------------------------------------------------
# example of use
#
# # use pathlyXYZ gen_path function for example
# # Movimientos con dirección preferencial (p.ej., migración, corriente)
#
#
# path3D <- gen_path(n = 250, dim = "3D", stepLength = 1, random = FALSE, fps = 1)
# plot_path3d(path3D$x, path3D$y, path3D$z, color = "viridis")
#
#   sim_tracks <- sim_path3d_crw(
#     path3D, n_sim = 10, drift = FALSE,
#     seed = 123)
#
#   plot_simpath3d(sim_tracks, original = path3D)
#
#   # with drif
#   sim_tracks <- sim_path3d_crw(
#     path3D, n_sim = 10, drift = TRUE,
#     seed = 123)
#
#   plot_simpath3d(sim_tracks, original = t3)









































