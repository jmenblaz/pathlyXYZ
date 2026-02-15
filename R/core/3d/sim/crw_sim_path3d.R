







# Correlatd Random Walk 3D simualtion
# used as base path3d object (stadarized sequeira et al.,) 


simulate_3d_track(trj, method = "random_walk", n = 1)
simulate_3d_tracks(trj, n_sim = 5)






sim_tracks <- lapply(1:5, function(i) {
  simulate_3d_track(t3)  # usando opción A o B
})






dx <- diff(t3$x)
dy <- diff(t3$y)
dz <- diff(t3$z)


step_lengths <- sqrt(dx^2 + dy^2 + dz^2)


# vector unitario aleatorio en 3D
theta <- runif(n, 0, 2*pi)       # ángulo horizontal
phi   <- acos(runif(n, -1, 1))   # ángulo vertical
dx_sim <- step_lengths * sin(phi) * cos(theta)
dy_sim <- step_lengths * sin(phi) * sin(theta)
dz_sim <- step_lengths * cos(phi)


x_sim <- cumsum(c(t3$x[1], dx_sim))
y_sim <- cumsum(c(t3$y[1], dy_sim))
z_sim <- cumsum(c(t3$z[1], dz_sim))











# =====================================================================
# simulate_3d_tracks.R
# Prototipo para simular múltiples trayectorias 3D a partir de un track real
# =====================================================================

# Internal helper: generar un solo track 3D random walk manteniendo longitudes de pasos
.simulate_3d_track <- function(trj, seed = NULL) {
  if (!inherits(trj, "Trajectory3D")) stop("trj debe ser un Trajectory3D")
  
  if (!is.null(seed)) set.seed(seed)
  
  dx <- diff(trj$x)
  dy <- diff(trj$y)
  dz <- diff(trj$z)
  step_lengths <- sqrt(dx^2 + dy^2 + dz^2)
  n_steps <- length(step_lengths)
  
  theta <- runif(n_steps, 0, 2*pi)
  phi   <- acos(runif(n_steps, -1, 1))
  
  dx_sim <- step_lengths * sin(phi) * cos(theta)
  dy_sim <- step_lengths * sin(phi) * sin(theta)
  dz_sim <- step_lengths * cos(phi)
  
  x_sim <- c(trj$x[1], trj$x[1] + cumsum(dx_sim))
  y_sim <- c(trj$y[1], trj$y[1] + cumsum(dy_sim))
  z_sim <- c(trj$z[1], trj$z[1] + cumsum(dz_sim))
  
  traj_sim <- trj
  traj_sim$x <- x_sim
  traj_sim$y <- y_sim
  traj_sim$z <- z_sim
  
  # recalcular desplazamientos y polar
  traj_sim$displacement <- c(0+0i, complex(real = diff(x_sim), imaginary = diff(y_sim)))
  traj_sim$polar <- traj_sim$displacement
  
  traj_sim
}

# ---------------------------------------------------------------------
# Public function: generar n tracks simulados
# ---------------------------------------------------------------------

#' Simulate multiple 3D trajectories from an observed track
#'
#' Generates alternative 3D trajectories that start at the same point
#' and respect the original step lengths (total path length similar).
#'
#' @param trj A Trajectory3D object
#' @param n_sim Number of simulated tracks to generate (default 5)
#' @param seed Optional random seed
#'
#' @return A list of Trajectory3D objects
#' @export
simulate_3d_tracks <- function(trj, n_sim = 5, seed = NULL) {
  lapply(1:n_sim, function(i) {
    .simulate_3d_track(trj, seed = seed)
  })
}















# ---------------------------------------------------------------------
# Example usage
# ---------------------------------------------------------------------
# t3: track original
sim_tracks <- simulate_3d_tracks(t3, n_sim = 25, seed = 123)
length_3dpath(sim_tracks[[1]])   # comprobar longitud similar


# check



# -------------------------------------------------------------------
# plot multiple 3D tracks with optional original track
# -------------------------------------------------------------------

plot_simpath3d <- function(sim_tracks, original = NULL, add = FALSE, alpha_sim = 0.3) {
  # sim_tracks: lista de Trajectory3D simulados
  # original: Trajectory3D original (opcional)
  
  if (!add) rgl::open3d()
  
  # ---- plot simulated tracks ----
  for (trj in sim_tracks) {
    rgl::lines3d(trj$x, trj$y, trj$z,
                 col = grDevices::adjustcolor("grey50", alpha.f = alpha_sim),
                 lwd = 2)
    rgl::points3d(trj$x, trj$y, trj$z,
                  col = grDevices::adjustcolor("grey50", alpha.f = alpha_sim),
                  size = 4)
  }
  
  # ---- plot original track if provided ----
  if (!is.null(original)) {
    n <- length(original$x)
    
    # start / end
    rgl::points3d(original$x[1], original$y[1], original$z[1],
                  col = "green", size = 10)
    rgl::points3d(original$x[n], original$y[n], original$z[n],
                  col = "red", size = 10)
    
    # path line
    cols <- colorRampPalette(c("blue", "cyan"))(n)
    rgl::lines3d(original$x, original$y, original$z, col = cols, lwd = 3)
    rgl::points3d(original$x, original$y, original$z, col = cols, size = 6)
  }
  
  rgl::axes3d(bbox = TRUE)
  invisible(NULL)
}

plot_simpath3d(sim_tracks, original = t3)
# ---------------------------------------------------------------------


