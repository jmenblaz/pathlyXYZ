


# =====================================================================
# Generate synthetic trajectory in 2D or 3D

# @jmenblaz


gen_path <- function(n = 500,
                     dim = c("2D", "3D"),
                     stepLength = 2,
                     random = FALSE,
                     alpha = 0.7,                          # turn persistence 0.7 keep balance between randomly and linear path 
                     angularErrorSd = 0.5,
                     linearErrorSd = 0.2,
                     angularErrorDist = function(n) stats::rnorm(n, sd = angularErrorSd),
                     linearErrorDist  = function(n) stats::rnorm(n, sd = linearErrorSd),
                     fps = 10,
                     start = c(0, 0, 0),
                     ...) {
  
  library(trajr)
  
  dim <- match.arg(dim)
  
  # --- simulate step lengths and errors ---
  angularErrors <- angularErrorDist(n)
  linearErrors  <- linearErrorDist(n)
  stepLengths   <- stepLength + linearErrors
  stepLengths[stepLengths < 0] <- 0
  
  if (dim == "2D") {
    steps <- complex(length.out = n, modulus = stepLengths, argument = angularErrors)
    
    coords <- complex(n + 1)
    coords[1] <- complex(real = start[1], imaginary = start[2])
    
    if (random) {
      # angular errors acumulativos (como antes)
      angle <- 0
      for (i in 1:n) {
        angle <- angle + angularErrors[i]
        coords[i + 1] <- coords[i] + complex(modulus = stepLengths[i], argument = angle)
      }
    } else {
      # angular error con persistencia alpha
      angle <- 0
      for (i in 1:n) {
        angle <- alpha*angle + (1-alpha)*angularErrors[i]
        coords[i + 1] <- coords[i] + complex(modulus = stepLengths[i], argument = angle)
      }
    }
    
    df <- data.frame(x = Re(coords), y = Im(coords))
    trj <- trajr::TrajFromCoords(df, fps = fps, ...)
    
  } else if (dim == "3D") {
    # --- 3D random directions ---
    dirs <- matrix(NA, n, 3)
    dirs[1, ] <- c(1, 0, 0)  # dirección inicial arbitraria
    
    if (random) {
      # pasos completamente aleatorios
      dirs <- .random_unit_vectors_3d(n)
    } else {
      # CRW 3D con persistencia alpha
      for(i in 2:n) {
        rand_dir <- .random_unit_vectors_3d(1)
        dirs[i, ] <- alpha*dirs[i-1, ] + (1-alpha)*rand_dir
        dirs[i, ] <- dirs[i, ] / sqrt(sum(dirs[i, ]^2))  # normalizar
      }
    }
    
    dx <- stepLengths * dirs[,1]
    dy <- stepLengths * dirs[,2]
    dz <- stepLengths * dirs[,3]
    
    x <- cumsum(c(start[1], dx))
    y <- cumsum(c(start[2], dy))
    z <- cumsum(c(start[3], dz))
    
    df <- data.frame(x = x, y = y, z = z)
    trj <- trajr::Traj3DFromCoords(df, xCol = "x", yCol = "y", zCol = "z", fps = fps, ...)
  }
  
  trj
}







