
# -----------------------------------------------------------------------------
# gen_path.R  - Generate synthetic trajectory in 2D or 3D
# -----------------------------------------------------------------------------

# author: @jmenblaz

# -----------------------------------------------------------------------------
# helpers

.random_unit_vectors_3d <- function(n = 1) {

  v <- matrix(stats::rnorm(n * 3), ncol = 3)
  norms <- sqrt(rowSums(v^2))

  v <- v / norms

  colnames(v) <- c("x", "y", "z")
  v
}



# -----------------------------------------------------
# function

# gen_path()

#' Generate synthetic 2D or 3D trajectory
#'
#' @param n Number of steps
#' @param dim "2D" or "3D"
#' @param stepLength Step length
#' @param random Logical, fully random or correlated
#' @param alpha Turn persistence (less alpha more 'snowball' effect, and less correlated)
#' @param angularErrorSd Standard deviation of angular noise
#' @param linearErrorSd Standard deviation of step length noise
#' @param angularErrorDist Function to generate angular errors
#' @param linearErrorDist Function to generate linear errors
#' @param fps Frames per second
#' @param start Starting coordinates (x, y[, z])
#' @param z_bounds for z axy limits -> c(-Inf, Inf),
#' @param central_place, for central place foraging paths (Logic TRUE/FALSE
#' @param central_place_strength, if @central_place is TRUE, modify the atracction force
#'
#' @param ... Additional args for trajr functions
#'
#' @return Trajectory object (2D or 3D) from trajr
#' @export

# -----------------------------------------------------------------------------
# helpers


# -----------------------------------------------------
# function

gen_path <- function(n = 500,
                     dim = c("2D", "3D"),
                     stepLength = 2,
                     random = FALSE,
                     alpha = 0.7,
                     angularErrorSd = 0.5,
                     linearErrorSd = 0.2,
                     angularErrorDist = function(n) stats::rnorm(n, sd = angularErrorSd),
                     linearErrorDist  = function(n) stats::rnorm(n, sd = linearErrorSd),
                     fps = 10,
                     start = c(0, 0, 0),
                     z_bounds = c(-Inf, Inf),
                     central_place = FALSE,
                     central_place_strength = 0.5,
                     ...) {

  library(trajr)

  dim <- match.arg(dim)

  angularErrors <- angularErrorDist(n)
  linearErrors  <- linearErrorDist(n)
  stepLengths   <- stepLength + linearErrors
  stepLengths[stepLengths < 0] <- 0

  # ----------------------
  # 2D trajectories
  # ----------------------
  if(dim == "2D"){

    coords <- complex(n+1)
    coords[1] <- complex(real = start[1], imaginary = start[2])
    angle <- 0

    for(i in 1:n){
      if(random){
        angle <- angle + angularErrors[i]
      } else {
        angle <- alpha*angle + (1-alpha)*angularErrors[i]
      }

      step <- complex(modulus = stepLengths[i], argument = angle)
      coords[i+1] <- coords[i] + step

      # central place attraction
      if(central_place){
        to_home <- c(start[1]-Re(coords[i+1]), start[2]-Im(coords[i+1]))
        remaining_steps <- n - i + 1
        correction <- (to_home / remaining_steps) * central_place_strength
        coords[i+1] <- complex(real = Re(coords[i+1]) + correction[1],
                               imaginary = Im(coords[i+1]) + correction[2])
      }
    }

    df <- data.frame(x = Re(coords), y = Im(coords))
    trj <- trajr::TrajFromCoords(df, fps=fps, ...)
  }

  # ----------------------
  # 3D trajectories
  # ----------------------
  if(dim == "3D"){

    dirs <- matrix(NA, n, 3)
    dirs[1,] <- c(1,0,0)

    if(random){
      dirs <- .random_unit_vectors_3d(n)
    } else {
      for(i in 2:n){
        rand_dir <- .random_unit_vectors_3d(1)
        dirs[i,] <- alpha*dirs[i-1,] + (1-alpha)*rand_dir
        dirs[i,] <- dirs[i,]/sqrt(sum(dirs[i,]^2))
      }
    }

    x <- numeric(n+1); y <- numeric(n+1); z <- numeric(n+1)
    x[1] <- start[1]; y[1] <- start[2]; z[1] <- start[3]

    for(i in 1:n){
      dx <- stepLengths[i] * dirs[i,1]
      dy <- stepLengths[i] * dirs[i,2]
      dz <- stepLengths[i] * dirs[i,3]

      x_new <- x[i] + dx
      y_new <- y[i] + dy
      z_new <- z[i] + dz

      # central place attraction
      if(central_place){
        to_home <- c(start[1]-x_new, start[2]-y_new, start[3]-z_new)
        remaining_steps <- n - i + 1
        correction <- (to_home / remaining_steps) * central_place_strength
        x_new <- x_new + correction[1]
        y_new <- y_new + correction[2]
        z_new <- z_new + correction[3]
      }

      # reflecting boundary in Z
      if(z_new > z_bounds[2]) z_new <- z_bounds[2] - (z_new - z_bounds[2])
      if(z_new < z_bounds[1]) z_new <- z_bounds[1] + (z_bounds[1] - z_new)

      x[i+1] <- x_new
      y[i+1] <- y_new
      z[i+1] <- z_new
    }

    # asegurar que último punto coincida con start si central_place
    if(central_place){
      x[n+1] <- start[1]
      y[n+1] <- start[2]
      z[n+1] <- start[3]
    }

    df <- data.frame(x=x, y=y, z=z)
    trj <- trajr::Traj3DFromCoords(df, xCol="x", yCol="y", zCol="z", fps=fps, ...)
  }

  trj
}




# -----------------------------------------------------------------------------
# example of use
#
# # use pathlyXYZ gen_path function for example

# path3D <- gen_path(
#   n = 500,
#   dim = "3D",
#   stepLength = 1,
#   alpha = 0.8,
#   central_place = TRUE,
#   central_place_strength = 2,
#   start = c(0,0,0),
#   z_bounds = c(0,1000)
# )
#
# plot_path3d(path3D$x, path3D$y, path3D$z, color = "viridis")
