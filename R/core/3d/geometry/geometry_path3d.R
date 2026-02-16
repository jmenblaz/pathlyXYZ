

# tayectory geometrys
# ----------------------------------------------------------------------------



# ----------------------------------------------------------
# .length_xyz (helper) calculate length from numeric vectors
# ----------------------------------------------------------

#' Calculate cumulative length from X, Y, Z coordinates
#'
#' Internal helper function to compute the cumulative 3D length
#' along a trajectory defined by numeric vectors. Not exported.
#'
#' @param x Numeric vector of X coordinates.
#' @param y Numeric vector of Y coordinates.
#' @param z Numeric vector of Z coordinates.
#' @param start Integer index of the starting point.
#' @param end Integer index of the ending point.
#'
#' @return Numeric scalar: total length along the trajectory segment.
#'
#' @keywords internal

.length_xyz <- function(x, y, z, start = 1, end = length(x)) {
  
  idx <- seq(start, end)
  
  dx <- diff(x[idx])
  dy <- diff(y[idx])
  dz <- diff(z[idx])
  
  sum(sqrt(dx^2 + dy^2 + dz^2))
}




# ----------------------------------------------------------
# length_3dpath - cumulative length of a 3D trajectory object


#' Length of a 3D path
#' 
#' Computes the total length of a 3D trajectory or a portion of it.
#' Length is the cumulative distance travelled along the trajectory.

#' @param trj3d A \code{Trajectory3D} object (standardized path object in pathlyXYZ).
#' @param startIndex Index of the first point to include (default 1).
#' @param endIndex Index of the last point to include (default nrow(trj3d)).

#' @return Numeric scalar: total length of the trajectory segment

#' @examples
#' # length of full trajectory
#' traj3d_length(trj3d)
#' # length of first 10 points
#' traj3d_length(trj3d, startIndex = 1, endIndex = 10)
#'
#' @export


length_path3d <- function(trj3d, startIndex = 1, endIndex = nrow(trj3d)) {
  
  stopifnot(inherits(trj3d, "Trajectory3D"))
  
  .length_xyz(trj3d$x, trj3d$y, trj3d$z, startIndex, endIndex)
}





