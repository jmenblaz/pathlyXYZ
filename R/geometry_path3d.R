
# ---------------------------------------------------------------------
# trajectory geometries
# -------------------------------------------------------------

# Index ----
# Helpers --

# functions --
#' @function length_path3d
#'








#--------------------------------------------------------------------
# Helpers and internals functions
# ----------------------------------------------------------
# .length_xyz (helper) calculate length from numeric vectors
# .vec
# .unit
# .central



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


#' @keywords internal
#'
.vec_norm <- function(v) {
  sqrt(rowSums(v^2))
}

#' @keywords internal

.unit_vector <- function(v) {
  n <- .vec_norm(v)
  v / n
}

#' @keywords internal
.central_diff <- function(v) {
  n <- nrow(v)
  dv <- matrix(NA_real_, n, 3)
  dv[2:(n-1), ] <- (v[3:n, ] - v[1:(n-2), ]) / 2
  dv
}





#---------------------------------------------------------------------
# Geometries function for 3Dpaths
# ---------------------------------------------------------


# ----------------------------------------------------------
#' @function length_path3d - cumulative length of a 3D trajectory object

#' Length of a 3D path
#'
#' Computes the total length of a 3D trajectory or a portion of it.
#' Length is the cumulative distance travelled along the trajectory.

#' @param path3d A \code{Trajectory3D} object (standardized path object in pathlyXYZ).
#' @param startIndex Index of the first point to include (default 1).
#' @param endIndex Index of the last point to include (default nrow(path3d)).

#' @return Numeric scalar: total length of the trajectory segment

#' @examples
#' # length of full trajectory
#' traj3d_length(path3d)
#' # length of first 10 points
#' traj3d_length(path3d, startIndex = 1, endIndex = 10)
#'
#' @export


length_path3d <- function(path3d, startIndex = 1, endIndex = nrow(path3d)) {

  stopifnot(inherits(path3d, "Trajectory3D"))

  .length_xyz(path3d$x, path3d$y, path3d$z, startIndex, endIndex)
}




#----------------------------------------------------------
#' @function straight_distance_3d - linear distance of a 3D trajectory
#'
#' Computes the straight-line distance between the first and last point
#' of a 3D trajectory.
#'
#' @param path3d A \code{Trajectory3D} object (standardized path object in pathlyXYZ)
#' @param startIndex Index of the start point (default 1)
#' @param endIndex Index of the end point (default nrow(path3d))
#'
#' @return Numeric scalar: straight-line distance between start and end
#'
#' @examples
#' # distance between first and last point
#' straight_distance_3d(path3d)
#' # distance from point 1 to point 10
#' straight_distance_3d(path3d, startIndex = 1, endIndex = 10)
#'
#'
#' @export
straight_dist_path3d <- function(path3d, startIndex = 1, endIndex = nrow(path3d)) {

    stopifnot(inherits(path3d, "Trajectory3D"))
    stopifnot(startIndex >= 1, endIndex <= nrow(path3d), startIndex < endIndex)

    dx <- path3d$x[endIndex] - path3d$x[startIndex]
    dy <- path3d$y[endIndex] - path3d$y[startIndex]
    dz <- path3d$z[endIndex] - path3d$z[startIndex]

    sqrt(dx^2 + dy^2 + dz^2)
}











#'
#'
#'
#'
#'
#' # ------------------------------------------------------------
#' # Frenet–Serret frame
#' # ------------------------------------------------------------
#'
#' #' Frenet frame for a 3D trajectory
#' #'
#' #' @param traj data.frame with columns x, y, z
#' #' @return data.frame with T, N, B vectors
#' Traj3DFrenetFrame <- function(traj) {
#'
#'   stopifnot(all(c("x", "y", "z") %in% names(traj)))
#'
#'   r <- as.matrix(traj[, c("x", "y", "z")])
#'
#'   dr <- rbind(NA, r[-1, ] - r[-nrow(r), ])
#'   ds <- .vec_norm(dr)
#'
#'   T <- .unit_vector(dr)
#'
#'   dT <- .central_diff(T)
#'   kappa <- .vec_norm(dT)
#'
#'   N <- dT / kappa
#'   B <- t(apply(cbind(T, N), 1, function(v) {
#'     t <- v[1:3]
#'     n <- v[4:6]
#'     c(
#'       t[2]*n[3] - t[3]*n[2],
#'       t[3]*n[1] - t[1]*n[3],
#'       t[1]*n[2] - t[2]*n[1]
#'     )
#'   }))
#'
#'   out <- data.frame(
#'     Tx = T[,1], Ty = T[,2], Tz = T[,3],
#'     Nx = N[,1], Ny = N[,2], Nz = N[,3],
#'     Bx = B[,1], By = B[,2], Bz = B[,3],
#'     ds = ds
#'   )
#'
#'   out
#' }
#'
#'
#' # ------------------------------------------------------------
#' # Curvature
#' # ------------------------------------------------------------
#'
#' #' Curvature of a 3D trajectory
#' #'
#' #' @param traj data.frame with x, y, z
#' #' @return numeric vector of curvature
#' Traj3DCurvature <- function(traj) {
#'
#'   fr <- Traj3DFrenetFrame(traj)
#'
#'   T <- as.matrix(fr[, c("Tx", "Ty", "Tz")])
#'   dT <- .central_diff(T)
#'
#'   kappa <- .vec_norm(dT)
#'
#'   kappa
#' }
#'
#'
#' # ------------------------------------------------------------
#' # Torsion
#' # ------------------------------------------------------------
#'
#' #' Torsion of a 3D trajectory
#' #'
#' #' @param traj data.frame with x, y, z
#' #' @return numeric vector of torsion
#' Traj3DTorsion <- function(traj) {
#'
#'   fr <- Traj3DFrenetFrame(traj)
#'
#'   B <- as.matrix(fr[, c("Bx", "By", "Bz")])
#'   N <- as.matrix(fr[, c("Nx", "Ny", "Nz")])
#'
#'   dB <- .central_diff(B)
#'
#'   tau <- -rowSums(dB * N)
#'
#'   tau
#' }
#'
#'
#' # ------------------------------------------------------------
#' # Turning angles (yaw / pitch)
#' # ------------------------------------------------------------
#'
#' #' Turning angles in 3D
#' #'
#' #' @param traj data.frame with x, y, z
#' #' @param type "horizontal" (yaw) or "vertical" (pitch)
#' #' @return numeric vector of turning angles
#' Traj3DTurningAngles <- function(traj,
#'                                 type = c("horizontal", "vertical")) {
#'
#'   type <- match.arg(type)
#'
#'   dx <- diff(traj$x)
#'   dy <- diff(traj$y)
#'   dz <- diff(traj$z)
#'
#'   if (type == "horizontal") {
#'     v1 <- cbind(dx[-length(dx)], dy[-length(dy)])
#'     v2 <- cbind(dx[-1], dy[-1])
#'
#'     num <- rowSums(v1 * v2)
#'     den <- sqrt(rowSums(v1^2) * rowSums(v2^2))
#'
#'     ang <- acos(pmax(pmin(num / den, 1), -1))
#'   }
#'
#'   if (type == "vertical") {
#'     inc <- atan2(dz, sqrt(dx^2 + dy^2))
#'     ang <- diff(inc)
#'   }
#'
#'   c(NA, ang)
#' }
#'
#'
#' # ------------------------------------------------------------
#' # Yaw and Pitch (wrappers)
#' # ------------------------------------------------------------
#'
#' Traj3DYaw <- function(traj) {
#'   Traj3DTurningAngles(traj, type = "horizontal")
#' }
#'
#' Traj3DPitch <- function(traj) {
#'   Traj3DTurningAngles(traj, type = "vertical")
#' }
#'
#'
#' # ------------------------------------------------------------
#' # Azimuth and inclination
#' # ------------------------------------------------------------
#'
#' #' Azimuth of movement in the horizontal plane
#' #'
#' #' @param traj data.frame with x, y
#' #' @return numeric vector (radians)
#' Traj3DAzimuth <- function(traj) {
#'
#'   dx <- diff(traj$x)
#'   dy <- diff(traj$y)
#'
#'   az <- atan2(dy, dx)
#'
#'   c(NA, az)
#' }
#'
#'
#' #' Inclination angle of movement
#' #'
#' #' @param traj data.frame with x, y, z
#' #' @return numeric vector (radians)
#' Traj3DInclination <- function(traj) {
#'
#'   dx <- diff(traj$x)
#'   dy <- diff(traj$y)
#'   dz <- diff(traj$z)
#'
#'   inc <- atan2(dz, sqrt(dx^2 + dy^2))
#'
#'   c(NA, inc)
#' }
#'
#'
#'
#'
#'
#' #
#' # Traj3DDistance 	Returns the distance between the start and end points of a 3D trajectory
#' # Traj3DStraightness 	Returns the straightness of a 3D trajectory (distance / length)
#' #
#' # Traj3DRediscretize 	Resamples a 3D trajectory to a constant step length
#' # Traj3DResampleTime 	Resample a 3D trajectory to a constant time interval
#' # Traj3DSmoothSG 	Smooths a 3D trajectory using a Savitzky-Golay filter
#' # Traj3DStepLengths 	Returns a vector of step lengths of a 3D trajectory
#'
#'
#'
