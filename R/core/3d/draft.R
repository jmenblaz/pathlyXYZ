


# function for simulate trajectories in 3D based on a observed track / path


library(rgl)
library(trajr)
library(dplyr)

# 2D dimension trajectory
trj <- trajr::TrajGenerate(n = 100)
# Plot it
plot(trj)


# 3D track
t3 <- trajr::Traj3DFromCoords(trj)
t3$z <- t3$z * 50


# check 3D path 
# plot using pathlyXYZ plot function
  # pathlyxyz::plot_3dpath()

# change function name by 
# plot_path3d

plot_3dpath(t3$x, t3$y, t3$z, 
            color = "viridis")  

# map units
# improving for the use of sf package and calculate the distance between points and depth
# change the name length_path3d
length_3dpath(t3)


str(t3)







