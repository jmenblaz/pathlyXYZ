
# ------------------------------------------------------------------------------
# pathlyXYZ - utils_biologging.R

# This script gathers a series of functions to held during the pre-proceing metadata
# and bio-logging data

# @jmenblaz / J. Menéndez-Blázque

# index
#' 1- @function sequeira_names()
#' 2- @function preproc_fields_comp()
#' 3- @function read_wcGPE()
# ------------------------------------------------------------------------------




# ------------------------------------------------------------------------------
#' 1 - sequeira_names()

#' @description
#' Standardized field names for bio-logging metadata
#'
#' This function return the name of the 92 variables for stadarization bio-logging
#' database stablished by Sequeira et al., 2021 (fields names)
#' https://github.com/ocean-tracking-network/biologging_standardization/tree/master/templates/fields
#' https://github.com/ocean-tracking-network/biologging_standardization/tree/4af00fd3e04a2489b2ebc66962b3a933437f2b70

#' @return character vector of field names
#' @export
sequeira_names <- function() {
  c("argosErrorRadius", "argosFilterMethod", "argosGDOP", "argosLC",
    "argosOrientation", "argosSemiMajor", "argosSemiMinor",
    "attachmentMethod", "axes", "calibrationsDone", "citation",
    "commonName", "deploymentDateTime", "deploymentEndType",
    "deploymentID", "deploymentLatitude", "deploymentLongitude",
    "depthGLS", "detachmentDateTime", "detachmentDetails",
    "detachmentLatitude", "detachmentLongitude", "dutyCycle",
    "gpsSatelliteCount", "instrumentID", "instrumentManufacturer",
    "instrumentModel", "instrumentSerialNumber", "instrumentSettings",
    "instrumentType", "latitude", "license", "longitude",
    "lowerSensorDetectionLimit", "organismAgeReproductiveClass",
    "organismID", "organismIDSource", "organismSex", "organismSize",
    "organismSizeMeasurementDescription", "organismSizeMeasurementTime",
    "organismSizeMeasurementType", "organismWeightAtDeployment",
    "organismWeightRemeasurement", "organismWeightRemeasurementTime",
    "orientationOfAccelerometerOnOrganism", "otherDataCoowners",
    "otherDataTypesAssociatedWithDeployment", "otherRelevantIdentifiers",
    "ownerEmailContact", "ownerInstitutionalContact", "ownerName",
    "ownerPhoneContact", "positionOfAccelerometerOnOrganism", "ptt",
    "qcDone", "qcNotes", "qcProblemsFound", "references",
    "residualsGPS", "resolution", "scientificName", "scientificNameSource",
    "sensorCalibrationDate", "sensorCalibrationDetails",
    "sensorDetectionLimits", "sensorDutyCycling", "sensorIMeasurement",
    "sensorIType", "sensorManufacturer", "sensorModel", "sensorPrecision",
    "sensorSamplingFrequency", "sensorType", "sunElevationAngle",
    "temperatureGLS", "time", "trackEndLatitude", "trackEndLongitude",
    "trackEndTime", "trackStartLatitude", "trackStartLongitude",
    "trackStartTime", "trackingDevice", "transmissionMode",
    "transmissionSettings", "trappingMethodDetails",
    "unitOfAltitudeDepth", "unitsReported", "uplinkInterval",
    "uplinkIntervalUnits", "upperSensorDetectionLimit")
}


# Example of use:
# sequeira_names <- sequeira_names()



# ------------------------------------------------------------------------------
#' 2- check_sequeira_names()
#'
#' Check fields names against Sequeira et al., 2021 standard
#'
#' Identifies columns in a dataframe that are non-standard (extra)
#' or missing compared to Sequeira et al., 2021 standardized fields
#'
#' @param df A data.frame of bio-logging tracking data and its metadata
#' @param verbose Logical, if TRUE prints summary
#'
#' @return List with elements:
#'   \item{extra}{columns in df not in standard}
#'   \item{missing}{standard columns not in df}

#' Check dataframe fields against Sequeira et al. standard
#'
#' Identifies which columns are standardized (Sequeira), which are extra,
#' and si las obligatorias están presentes. Por defecto, el eje Z se asume `depthGLS`.
#'
#' @param df Dataframe a chequear
#' @param required_fields Character vector de campos obligatorios. Default: c("organismID","latitude","longitude","unitOfAltitudeDepth","depthGLS")
#' @return List con:
#'   \item{standard}{columnas que coinciden con Sequeira}
#'   \item{extra}{columnas no estandarizadas}
#'   \item{missing_required}{columnas obligatorias que faltan}
#' @export
check_sequeira_names <- function(df,
                                  required_fields = c("organismID","latitude","longitude")) {

  stopifnot(is.data.frame(df))  # check class

  sequeira <- sequeira_names()
  df_cols <- colnames(df)

  standard_cols <- intersect(df_cols, sequeira)
  extra_cols    <- setdiff(df_cols, sequeira)
  missing_required <- setdiff(required_fields, df_cols)

  if ((length(missing_required) == 0)) {
    missing_required <- ""
  }

  result <- list(
    standard = standard_cols,
    extra = extra_cols,
    missing_required = missing_required
  )

  return(result)

}




#-------------------------------------------------------------------------------
# 3- read_wcGPE()
#-------------------------------------------------------------------------------

#' Read Wildlife Computers GPE model location files
#'
#' Reads raw CSV output files generated by Wildlife Computers GPE models (Global Position Estimator)
#' modeling software. It extracts the header metadata, handles irregular comma-delimited
#' structures, and formats temporal fields (`Date`, `Sunrise`, `Sunset`) into `POSIXct` objects.
#'
#' @param file Character string specifying the file path to the GPE CSV file.
#' @param info_meta Logical. If `TRUE` (default), prints the header metadata (PTT, tag model, status, reference datasets) to the console.
#' @param tz Character string specifying the time zone for datetime parsing. Default is `"UTC"`.
#' @param quiet Logical. If `TRUE`, suppresses informational messages printed during loading. Default is `FALSE`.
#' @param visual_info Logical. If `TRUE`, plot location and observatiion type maps
#' @return A `data.frame` containing the GPE position estimates with standardized column headers and formatted datetime fields.
#'
#' @details
#' Wildlife Computers GPE CSV exports typically include 4 lines of metadata prefixed with `;`,
#' followed by an empty line, a header row at position 6, and data rows that may begin with a leading comma.
#' This function handles this specific structure by indexing lines directly, stripping leading delimiters
#' from data rows, and converting blank character entries into `NA`.
#'
#' @seealso \code{\link[base]{as.POSIXct}}
#'
#' @export
#' @examples
#' \dontrun{
#' gpe_data <- read_wcGPE("path/to/GPE_Locations.csv", info_meta = TRUE)
#' head(gpe_data)
#' }
read_wcGPE <- function(file, info_meta = TRUE, tz = "UTC",
                       sequeira_rename = TRUE,
                       visual_info = FALSE, quiet = FALSE) {

  # 0 - local config
  old_lc <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  on.exit(Sys.setlocale("LC_TIME", old_lc), add = TRUE)

  # 1 - Read .csv by lines
  all_lines <- readLines(file, warn = FALSE)  # read raw lines of the .csv

  # 2 - Metadata info
  if (info_meta) {
    meta_lines <- all_lines[1:4]
    # clean text
    clean_meta <- gsub('^"|"$', '', meta_lines)
    clean_meta <- sub('^\\s*;\\s*', '', clean_meta)
    cat("----- GPE Metadata:  \n")
    cat(paste(clean_meta, collapse = "\n"), "\n")
    cat("----------------\n\n")

  }

  # 3 - Select data - Omit lines 1:5 (at least for Wildlife Computer, GP3 Model)
  body_lines <- all_lines[6:length(all_lines)]

  # 4 - Convert into df
  df <- read.csv(
    text = body_lines,
    header = TRUE,
    fill = TRUE,
    strip.white = TRUE,
    stringsAsFactors = FALSE
  )


  # 5 - Format fields - POSIXct
  date_cols <- c("Date", "Sunrise", "Sunset")

  for (col in date_cols) {
    if (col %in% names(df)) {
      # Limpiar espacios en blanco y convertir cadenas vacías a NA
      val <- trimws(df[[col]])
      val[val == ""] <- NA

      # Convertir a POSIXct
      df[[col]] <- as.POSIXct(val, format = "%d-%b-%Y %H:%M:%S", tz = tz)
    }
  }

  if (!quiet) {
    cat(paste0("· GPE position data loaded: ", nrow(df), " rows and ", ncol(df), " columns.\n"))
  }

  # 6 - Renames fields based on Sequeria et al., 2021
  if (sequeira_rename) {
    rename_map <- c(
      "Ptt"                   = "ptt",
      "Date"                  = "time",
      "Most.Likely.Latitude"  = "latitude",
      "Most.Likely.Longitude" = "longitude"
    )
    for (old_col in names(rename_map)) {
      if (old_col %in% names(df)) {
        names(df)[names(df) == old_col] <- rename_map[old_col]
      }
    }

    df$organismID <- NA

    if (!quiet) {
      cat(" · Renamed fields to Sequeira et al. standard (organismID, time, latitude, longitude).\n")
    }
  }

  # Summary plot of GPE positions
  if (visual_info) {

    # World map (WGS84)
    world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

    # Map exten +- 1 degree
    xlim <- range(df[["longitude"]], na.rm = TRUE) + c(-2, 2)
    ylim <- range(df[["latitude"]], na.rm = TRUE) + c(-2, 2)

    # Mapa theme
    clean_theme <- ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid      = ggplot2::element_blank(),
        axis.title      = ggplot2::element_blank(),
        axis.text       = ggplot2::element_text(size = 10),
        legend.position = "right",
        plot.margin     = ggplot2::margin(4, 4, 4, 4)
      )

    # Plot 1: Progresión temporal (viridis)
    p1 <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = world, fill = "grey80", color = NA) +
      ggplot2::geom_path(data = df, ggplot2::aes(x = .data[["longitude"]], y = .data[["latitude"]]), color = "grey60", linewidth = 0.3) +
      ggplot2::geom_point(data = df, ggplot2::aes(x = .data[["longitude"]], y = .data[["latitude"]], color = .data[["time"]]), size = 1.2) +
      ggplot2::scale_color_viridis_c() +
      ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
      clean_theme

    p1

    # Plot 2: Tipo de observación
    p2 <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = world, fill = "grey80", color = NA) +
      ggplot2::geom_point(data = df, ggplot2::aes(x = .data[["longitude"]], y = .data[["latitude"]], color = Observation.Type), size = 1.2) +
      ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
      clean_theme

    #  print plots
    if (requireNamespace("patchwork", quietly = TRUE)) {
      print(patchwork::wrap_plots(p1, p2, ncol = 2))
    } else {
      print(p1)
      print(p2)
    }
  }

  return(df)
}











