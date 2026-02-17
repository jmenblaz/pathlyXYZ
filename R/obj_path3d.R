

# create object 3d pathlyXYZ 3D object for simulation


Traj3DFromCoords <- function(track, xCol = 1, yCol = 2, zCol = 3,
                             timeCol = NULL, fps = 50,
                             spatialUnits = "m", timeUnits = "s") {
  
  trj3d <- track
  
  # Check Z coordinates are valid and remove leading/trailing NAs
  trj3d <- .checkCoords(trj3d, cols = zCol)
  
  # Create 2D trajectory
  trj3d <- TrajFromCoords(trj3d, xCol = xCol, yCol = yCol, timeCol = timeCol, fps = fps, spatialUnits = spatialUnits, timeUnits = timeUnits)
  
  # Rename Z column
  trj3d <- .renameCol(zCol, "z", trj3d)
  
  # Give it a special class
  class(trj3d) <- c(.TRAJ_3D_CLASS, class(trj3d))
  
  trj3d
}