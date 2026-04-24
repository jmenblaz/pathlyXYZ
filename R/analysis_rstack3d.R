

#' cv_rstacks3d()

#' Calculate 3D Coefficient of Variation (CV) across Multiple Raster Stacks
#'
#' @description
#' This function computes the voxel-wise Coefficient of Variation (CV) across a list
#' of 3D raster stacks (e.g., monthly cumulative impacts, distribution, enviromental
#' variable, habitat suitability).It processes data layer by layer to optimize memory usage, making it suitable for high-resolution
#' 2.5/3D environmental data.
#'
#' @param stack_list A list of SpatRaster objects or a character vector of file paths
#' to .tif files (min 2 stacks). All stacks must have the same spatial geometry and number of layers.
#' @param na.rm Logical. Should NA values be ignored during mean and SD calculations?
#' Default is TRUE.
#' @param filename Character. Optional path to save the resulting CV stack as a GeoTIFF(.tif).
#' @param summary Logical. If TRUE, prints a summary table with mean, min, max CV
#' and stable area percentage per layer.
#' @param quiet Logical. If FALSE (default), shows progress bars and informative messages.
#' #' @param cv_threshold Numeric. Threshold percentage to define the 'stable area'.
#' Default is 20.
#' @param ... Additional arguments.
#'
#' @details
#' The Coefficient of Variation is calculated as: \deqn{CV = (\sigma / \mu) * 100}.
#' The 'Stable Area' metric represents the percentage of pixels with a CV < 20%.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' library(pathlyXYZ)
#' # Dummy 3D stacks based on terra volcano data
#' r <- terra::rast(volcano)
#' s1 <- c(r, r*0.9, r*0.8) # Stack Month 1 (3 bins)
#' s2 <- c(r*1.1, r, r*0.7) # Stack Month 2 (3 bins)
#' s3 <- c(r*1.2, r*0.8, r*0.9) # Stack Month 3 (3 bins)
#'
#' names(s1) <- names(s2) <- names(s3) <- c("layer_1", "layer_2", "layer_3")
#' stack_list <- list(s1, s2, s3)
#'
#' # Calculate CV across the 3 stack (e.g., months)
#' cv_3d <- pathlyXYZ::cv_rstacks3d(stack_list, summary = TRUE)
#'
#' # Plot titled rstack using plot_rstack3d
#' patlyXYZ::plot_rstack3d(cv_3d)
#'
#' ## util: example for create a list of SpatRaster from list.files()
#' files <- list.files()
#' stack_list <- lapply(files, terra::rast)
#'
#' }
#' @export
cv_rstacks3d <- function(stack_list,
                       na.rm = TRUE,
                       filename = NULL,
                       summary = TRUE,
                       quiet = FALSE,
                       cv_threshold = 20,
                       # plot3d = FALSE, # In progress...
                       ...) {

  # QC: pre-processing and formatting ------------------------------------------

  # If input is a character vector (e.g., file paths from list.files),
  # convert into list for processing
  if (is.character(stack_list)) {
    stack_list <- as.list(stack_list)
  }

  # QC: Ensure we have a non-empty list for further processing
  if (!is.list(stack_list) || length(stack_list) == 0) {
    stop("Input 'stack_list' must be a non-empty list or a character vector of file paths.")
  }

  # Ensure all elements are SpatRasters
  # This loop handles both pre-loaded SpatRasters and paths to .tif files
  stack_list <- lapply(stack_list, function(x) {
    if (inherits(x, "SpatRaster")) {
      return(x)
    } else if (is.character(x) && file.exists(x)) {
      return(terra::rast(x))
    } else {
      stop(paste("Element is neither a SpatRaster nor a valid file path:", x))
    }
  })

  # QC: Minimum of two stacks required for variability analysis ---------------
  if (length(stack_list) < 2) {
    stop("CV calculation requires at least two stacks to compute variability over time.")
  }

  # QC: Geometries and spatial features ---------------------------------------
  # Use terra::compareGeom to ensure CRS, Extent and Resolution match
  ref_stack <- stack_list[[1]]

  # Iterate through the remaining stacks to ensure spatial consistency
  lapply(seq_along(stack_list)[-1], function(i) {
    tryCatch({
      terra::compareGeom(ref_stack, stack_list[[i]],
                         crs = TRUE, ext = TRUE, res = TRUE,
                         stopOnError = TRUE)
    }, error = function(e) {
      # Custom pathlyXYZ error message
      stop(paste0("\n[pathlyXYZ::cv_rstacks] Spatial mismatch error:\n",
                  "Stack [[", i, "]] does not match the reference geometry of Stack [[1]].\n",
                  "Details: ", e$message, "\n",
                  "Hint: Check if all .tif files have the same CRS, Extent, and Resolution."),
           call. = FALSE)
    })
  })

  # Check for consistent number of layers (depth bins) -------------------------
  # n layer of all stack
  n_lyrs <- base::sapply(stack_list, terra::nlyr)
  if (length(unique(n_lyrs)) > 1) {
    stop("Inconsistent number of layers (bins) across the provided stacks")
  }

  # ---------------------------------------------------------------------------
  # CV processing -------------------------------------------------------------
  # usin first layer as base
  layers_per_stack <- n_lyrs[1]

  # Create a list to store the resulting CV layers
  cv_layers <- list()

  # Note: using in order to avoid using a single stack
  # This keeps memory usage low because we only load one layer per file at a time
  # Initialize progress bar if not quiet
  if (!quiet) {
    pb <- utils::txtProgressBar(min = 0, max = layers_per_stack,
                                style = 3,
                                title = "Calculating CV")
  }

  for (i in 1:layers_per_stack) {
    # Extract the i-th layer from each stack
    current_layer_list <- lapply(stack_list, function(s) s[[i]])
    layer_stack <- terra::rast(current_layer_list)

    # CV (using the vectorized approach is faster/safer) ---
    avg_layer <- terra::mean(layer_stack, na.rm = na.rm)
    sd_layer  <- terra::stdev(layer_stack, na.rm = na.rm)
    cv_layer  <- (sd_layer / avg_layer) * 100
    cv_layer  <- terra::ifel(avg_layer == 0, NA, cv_layer)
    # ---------------------------------------------------------

    # append
    cv_layers[[i]] <- cv_layer

    # update barprogress
    if (!quiet) {
      utils::setTxtProgressBar(pb, i)
    }
  }

  # Close progress bar
  if (!quiet) close(pb)

  # Merge the individual CV layers into a single SpatRaster
  cv_result <- terra::rast(cv_layers)
  names(cv_result) <- names(ref_stack)

  # summary and export ---------------------------------------------------------

  if (summary) {
    summary_df <- data.frame(
      layer = names(ref_stack),
      mean_CV = as.vector(terra::global(cv_result, "mean", na.rm = TRUE)[,1]),
      min_CV = as.vector(terra::global(cv_result, "min", na.rm = TRUE)[,1]),
      max_CV  = as.vector(terra::global(cv_result, "max", na.rm = TRUE)[,1]),
      stable_area = as.vector(terra::global(cv_result < cv_threshold, "mean", na.rm = TRUE)[,1]) * 100
    )

    cat("CV stack result summary ---\n\n")
    cat("    · Values in percentage (%)\n\n")
    print(summary_df, row.names = FALSE)
    cat("\n")
  }

  if (!is.null(filename)) {
    terra::writeRaster(cv_result, filename = filename, overwrite = TRUE)
    message(paste("CV stack exported to:", filename))
  }

  # Result visualization

  # if (plot3d) {
  #   # Assuming plot_rstack3d is available in the environment/package
  #   plot_rstack3d(cv_result, ...)
  # }

  return(cv_result)
}



