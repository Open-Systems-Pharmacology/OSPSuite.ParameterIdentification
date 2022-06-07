.getPackageVersion <- function() {
  version <- getNamespaceVersion("ospsuite.parameteridentification")
  return(version)
}

# Environment that holds various global variables and settings for the ospsuite.parameteridentification,
# It is not exported and should not be directly manipulated by other packages.
piEnv <- new.env(parent = emptyenv())

# name of the package. This will be used to retrieve information on the package at run time
piEnv$packageName <- "ospsuite.parameteridentification"

# Version of the package
piEnv$packageVersion <- .getPackageVersion()

#' Names of the settings stored in piEnv Can be used with `getOSPSuitePISetting()`
#' @export
ospsuitePIRSettingNames <- enum(names(piEnv))

#' Get the value of a global ospsuite.parameteridentification setting.
#'
#' @param settingName String name of the setting
#'
#' @return Value of the setting stored in piEnv. If the setting does not exist, an error is thrown.
#' @export
#'
#' @examples
#' getOSPSuitePISetting("packageVersion")
getOSPSuitePISetting <- function(settingName) {
  if (!any(names(piEnv) == settingName)) {
    stop(messages$errorOSPSuitePISettingNotFound(settingName))
  }

  obj <- piEnv[[settingName]]
  # Evaluate if the object is a function. This is required since some properties are defined as function reference
  if (is.function(obj)) {
    return(obj())
  }

  return(obj)
}
