#' @title Create a Parameter Identification Results Object
#' @description
#' Internal helper to construct a standardized `piResult` S3 object containing
#' optimization results, confidence interval estimates, parameter metadata, and
#' configuration used for parameter identification.
#'
#' @param optimResult A list containing optimization results returned by
#' `Optimizer$run()`.
#' @param ciResult Optional list of confidence interval results, e.g., from
#' `Optimizer$estimateCI()`.
#' @param configuration The `PIConfiguration` used for the run.
#' @param piParameters A list of `PIParameters` objects used in the optimization.
#'
#' @return An S3 object of class `"piResult"` with structured fields for
#' optimization, confidence intervals, parameter info, and configuration.
#'
#' @keywords internal
#' @noRd
.createPIResult <- function(optimResult, ciResult = NULL, configuration, piParameters) {
  ciResult <- ciResult %||% list(
    sd = NA_real_, cv = NA_real_, lowerCI = NA_real_, upperCI = NA_real_,
    method = NA_character_, elapsed = NA_real_, error = NULL, details = list()
  )

  parameters <- do.call(rbind, lapply(seq_along(piParameters), function(i) {
    piParameters[[i]]$toDataFrame(group = paste0(i))
  }))

  result <- list(
    finalParameters = optimResult$par,
    sd = ciResult$sd,
    cv = ciResult$cv,
    lowerCI = ciResult$lowerCI,
    upperCI = ciResult$upperCI,
    objectiveValue = optimResult$value,
    initialParameters = optimResult$startValues,
    convergence = optimResult$convergence,
    algorithm = optimResult$algorithm,
    elapsed = optimResult$elapsed,
    iterations = optimResult$iterations,
    fnEvaluations = optimResult$fnEvaluations,
    ciMethod = ciResult$method,
    ciElapsed = ciResult$elapsed,
    ciError = ciResult$error,
    ciDetails = ciResult$details,
    parameters = parameters,
    configuration = configuration
  )

  resultObj <- structure(
    result,
    class = "piResult"
  )

  return(resultObj)
}

