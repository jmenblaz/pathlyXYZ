#  DEVELOP VERSION FOR INLUDE
# BOUNDARYS, DEM AND BATH FOR BOUNDARIES, ETC








# ------------------------------------------------------------------------------
  # sim_path3d_CRW_VAR.R
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
# Fit VAR(1) model to 3D displacements    --------------------

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
    warning("VAR(1) puede ser inestable (max eigenvalue >= 1)")
  }

  list(
    A     = A,
    Sigma = Sigma,
    mu    = if (drift) mu else c(0,0,0)
  )
}


# ------------------------------------------------------------------------------
# Helper interno: simular un CRW 3D VAR(1) ------------------------------------
.simulate_var3d <- function(path3d,
                            drift = FALSE,
                            seed = NULL,
                            z_bounds = c(-Inf, Inf),
                            central_place = FALSE,
                            central_places_strength = 0.05) {

  if (!is.null(seed)) set.seed(seed)

  model <- .fit_var3d(path3d, drift = drift)
  n_steps <- nrow(path3d) - 1

  # storage
  # store
  x_sim <- numeric(n_steps + 1)
  y_sim <- numeric(n_steps + 1)
  z_sim <- numeric(n_steps + 1)
  Dsim  <- matrix(0, n_steps, 3)

  # posición inicial
  # initial position
  x_sim[1] <- path3d$x[1]
  y_sim[1] <- path3d$y[1]
  z_sim[1] <- path3d$z[1]

  # desplazamiento inicial
  # initial displacement
  Dsim[1, ] <- c(
    path3d$x[2] - path3d$x[1],
    path3d$y[2] - path3d$y[1],
    path3d$z[2] - path3d$z[1]
  )

  # loop de simulación
  # simulation loop
  for (i in 2:n_steps) {

    eps <- MASS::mvrnorm(1, mu = c(0,0,0), Sigma = model$Sigma)

    # step / paso VAR(1) con drift
    step <- model$mu + (Dsim[i-1, ] - model$mu) %*% model$A + eps

    x_new <- x_sim[i] + step[1]
    y_new <- y_sim[i] + step[2]
    z_new <- z_sim[i] + step[3]

    # -------------------------------------------------
    # central place foraging (regresa al inicio)
    # -------------------------------------------------
    if (central_place) {
      to_home <- c(x_sim[1] - x_new,
                   y_sim[1] - y_new,
                   z_sim[1] - z_new)

      remaining_steps <- n_steps - i + 1
      if (remaining_steps > 0) {
        correction <- (to_home / remaining_steps) * central_places_strength
        x_new <- x_new + correction[1]
        y_new <- y_new + correction[2]
        z_new <- z_new + correction[3]
      }
    }

    # reflecting boundaries in Z
    if (z_new > z_bounds[2]) {
      z_new <- z_bounds[2] - (z_new - z_bounds[2])
      step[3] <- z_new - z_sim[i]
    }
    if (z_new < z_bounds[1]) {
      z_new <- z_bounds[1] + (z_bounds[1] - z_new)
      step[3] <- z_new - z_sim[i]
    }

    # almacenar nueva posición
    # store new position
    x_sim[i + 1] <- x_new
    y_sim[i + 1] <- y_new
    z_sim[i + 1] <- z_new

    # almacenar desplazamiento
    # store displacement
    Dsim[i, ] <- step
  }

  # forzar último punto en central place
  if (central_place) {
    x_sim[n_steps + 1] <- x_sim[1]
    y_sim[n_steps + 1] <- y_sim[1]
    z_sim[n_steps + 1] <- z_sim[1]
  }

  # construir objeto Trajectory3D
  trj_sim <- path3d
  trj_sim$x <- x_sim
  trj_sim$y <- y_sim
  trj_sim$z <- z_sim
  trj_sim$displacement <- c(0+0i, complex(real = diff(x_sim), imaginary = diff(y_sim)))
  trj_sim$polar <- trj_sim$displacement

  trj_sim
}


# ------------------------------------------------------------------------------
# Función principal: simular N CRWs 3D ----------------------------------------
sim_path3d_crw <- function(path3d,
                           n_sim = 5,
                           drift = FALSE,
                           seed = NULL,
                           z_bounds = c(-Inf, Inf),
                           central_place = FALSE,
                           central_places_strength = 0.05) {

  stopifnot(inherits(path3d, "Trajectory3D"))

  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package 'MASS' is required")
  }

  lapply(seq_len(n_sim), function(i) {
    .simulate_var3d(
      path3d,
      drift = drift,
      seed = if (!is.null(seed)) seed + i else NULL,
      z_bounds = z_bounds,
      central_place = central_place,
      central_places_strength = central_places_strength
    )
  })
}






# ------------------------------------------------------------------------------
# Examples of use
#
# sim_tracks <- sim_path3d_crw(path3D,
#                              n_sim = 5,
#                              drift = TRUE,
#                              seed = NULL,
#                              z_bounds = c(0, Inf),
#                              # central place foraging
#                              central_place = TRUE,
#                              central_places_strength = 0.5)
#
# plot_simpath3d(sim_tracks, original = path3D)
