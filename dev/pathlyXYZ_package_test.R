




# load package
# pathlyXYZ
devtools::load_all()


# Simple test for run just after isntalling pathlyXYZ in order to ensure all is ok
# gen_path function to generate a 2D or 3D path/track
path3D <- gen_path(n = 250, dim = "3D", stepLength = 1, random = FALSE, fps = 1)

# plot 3D path
# color pathlyXYZ colors = NULL
# or standar
plot_path3d(path3D$x, path3D$y, path3D$z, color = "viridis")

# simulate 3D tracks with Correlated Random Walk (without drift o direction)
sim_tracks <- sim_path3d_crw(
  path3D, n_sim = 10, drift = FALSE,
  seed = 123)
# plot multiple 3D plots or simulations and the original/observed one
plot_simpath3d(sim_tracks, original = path3D)

# with drift
sim_tracks <- sim_path3d_crw(
  path3D, n_sim = 10, drift = TRUE,
  seed = 123)

plot_simpath3d(sim_tracks, original = path3D)





























