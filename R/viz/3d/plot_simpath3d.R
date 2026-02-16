



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

# plot_simpath3d(sim_tracks, original = t3)






