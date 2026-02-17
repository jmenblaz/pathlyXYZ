


# ----------------------------------------------------------
#' Construct a pathly_path object
#'
#' @param coords data.frame or matrix with columns x, y[, z]
#' @param timestamp frames per second (optional)
#' @param organismID (optional)
#' @return object of class c("pathly_2D", "pathly_path") or c("pathly_3D", "pathly_path")
#'
#' @export
pathly_path <- function(coords, fps = NULL) {

  # ensure data.frame
  df <- as.data.frame(coords)

  if(!all(c("x","y") %in% colnames(df))) {
    stop("coords must have at least columns x and y")
  }

  # detect 2D or 3D
  if("z" %in% colnames(df)) {
    class(df) <- c("pathly_3D", "pathly_path", "data.frame")
  } else {
    class(df) <- c("pathly_2D", "pathly_path", "data.frame")
  }

  attr(df, "fps") <- fps

  df
}



















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

class(path3D)
