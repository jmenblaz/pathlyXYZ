
# ------------------------------------------------------------------------------
# pathlyXYZ - utils_biologging.R

# This script gathers a series of functions to held during the pre-proceing metadata
# and bio-logging data

# @jmenblaz / J. Menéndez-Blázque

# index
#' 1- @function sequeira_fields()
#' 2- @function preproc_fields_comp()






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



