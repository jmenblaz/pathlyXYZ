
# ------------------------------------------------------------------------------
# pathlyXYZ - utils_biologging.R

# This script gathers a series of functions to held during the pre-proceing metadata
# and bio-logging data

# @jmenblaz / J. Menéndez-Blázque

# index
#' 1- @function sequeira_fields()
#' 2- @function preproc_fields_comp()
#' 3- @function read_wcGPE()





# ------------------------------------------------------------------------------
#' 1- @function sequeira_fields()

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
#' 2- @function preproc_fields_comp()
#'
#' @description Check fields names against Sequeira et al., 2021 standard
#'
#' Identifies columns in a dataframe that are non-standard (extra)
#' or missing compared to Sequeira et al., 2021 standardized fields
#'
#'
#' @param df A data.frame of bio-logging tracking data and its metadata
#' @param verbose Logical, if TRUE prints summary
#'
#' @return List with elements:
#'   \item{extra}{columns in df not in standard}
#'   \item{missing}{standard columns not in df}
#' @export


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
#'

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
#' 3- @function read_wcGPE()

#' Read Wildlife Computers GPE model location Files
#'
#' @description
#' Reads raw CSV output files generated by Wildlife Computers GPE models (Global Position Estimator)
#' modeling software. It extracts the header metadata, handles irregular comma-delimited
#' structures, and formats temporal fields (`Date`, `Sunrise`, `Sunset`) into `POSIXct` objects.
#'
#' @param file Character string specifying the file path to the GPE3 CSV file.
#' @param info_meta Logical. If `TRUE` (default), prints the header metadata (PTT, tag model, status, reference datasets) to the console.
#' @param tz Character string specifying the time zone for datetime parsing. Default is `"UTC"`.
#' @param quiet Logical. If `TRUE`, suppresses informational messages printed during loading. Default is `FALSE`.
#'
#' @return A `data.frame` containing the GPE3 position estimates with standardized column headers and formatted datetime fields.
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
#' gpe_data <- read_wcGPE("path/to/GPE3_Locations.csv", info_meta = TRUE)
#' head(gpe_data)
#' }
#'
read_wcGPE <- function(file, info_meta = TRUE, tz = "UTC", quiet = FALSE) {

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

  # 4 - remove initial comma(ej. ",220437...")
  body_lines <- sub("^,", "", body_lines)

  # 5 - Convert into df
  df <- read.csv(
    text = body_lines,
    header = TRUE,
    fill = TRUE,
    strip.white = TRUE,
    stringsAsFactors = FALSE
  )

  # 6. Format fields - POSIXct
  date_cols <- c("Date", "Sunrise", "Sunset")

  for (col in date_cols) {
    if (col %in% names(df)) {
      # Convertir cadenas vacías en NA
      df[[col]][df[[col]] == ""] <- NA
      # Parsear a POSIXct con el formato '26-Nov-2021 15:26:00'
      df[[col]] <- as.POSIXct(df[[col]], format = "%d-%b-%Y %H:%M:%S", tz = tz)
    }
    if (!quiet) {
      cat(paste("· GPE mdoel position data model loaded: ", nrow(df), "rows y", ncol(df), "columns.\n"))
    }
    return(df)
  }
}











