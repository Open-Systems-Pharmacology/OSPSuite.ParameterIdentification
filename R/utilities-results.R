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
  ciResult <- ciResult %||% list()

  parameters <- do.call(rbind, lapply(seq_along(piParameters), function(i) {
    piParameters[[i]]$toDataFrame(group = paste0(i))
  }))

  if (!is.finite(optimResult$value)) {
    optimResult$convergence <- FALSE
  }

  result <- list(
    finalParameters = optimResult$par,
    sd = ciResult$sd %||% NA_real_,
    cv = ciResult$cv %||% NA_real_,
    lowerCI = ciResult$lowerCI %||% NA_real_,
    upperCI = ciResult$upperCI %||% NA_real_,
    objectiveValue = optimResult$value,
    initialParameters = optimResult$startValues,
    convergence = optimResult$convergence,
    algorithm = optimResult$algorithm,
    elapsed = optimResult$elapsed,
    iterations = optimResult$iterations,
    fnEvaluations = optimResult$fnEvaluations,
    ciMethod = ciResult$method %||% configuration$ciMethod,
    ciElapsed = ciResult$elapsed %||% NA_real_,
    ciError = ciResult$error,
    ciDetails = ciResult$details %||% list(),
    parameters = parameters,
    configuration = configuration
  )

  resultObj <- structure(
    result,
    class = "piResult"
  )

  return(resultObj)
}

#' @title Print Parameter Identification Results
#'
#' @description Custom `print()` method for `piResult` objects. Displays a
#' summary including algorithm, convergence status, objective value, estimated
#' parameters and confidence intervals.
#'
#' @param x An S3 object of class `piResult`.
#'
#' @return The input `piResult` object, invisibly.
#' @export
print.piResult <- function(x) {
  cat("Parameter Identification Result\n\n")

  cat("Algorithm:", x$algorithm, "\n")
  if (!is.null(x$ciMethod) && !is.na(x$ciMethod)) {
    cat("CI Method:", x$ciMethod, "\n")
  }
  cat("Objective value:", format(x$objectiveValue, digits = 6), "\n")
  cat("Convergence:", x$convergence, "\n")
  cat("Iterations:", x$iterations, " | Function Evaluations:", x$fnEvaluations, "\n\n")

  coefs <- data.frame(
    Parameter = x$parameters$name,
    Estimate = x$finalParameters,
    SD = x$sd,
    CV = x$cv,
    Lower.CI = x$lowerCI,
    Upper.CI = x$upperCI
  )

  print(coefs, digits = 4, row.names = FALSE, na.print = "NA")

  cat("\n\n")
  cat("Elapsed time (optimization):", format(x$elapsed, digits = 3), "s\n")
  if (!is.null(x$ciElapsed) && !is.na(x$ciElapsed)) {
    cat("Elapsed time (CI):          ", format(x$ciElapsed, digits = 3), "s\n")
  }
  cat("\n")

  invisible(x)
}

#' @title Tidy Output from Parameter Identification
#'
#' @description Converts a `piResult` object into a tidy tabular format.
#' Returns one row per parameter with associated metadata and estimation results,
#' including confidence intervals and coefficient of variation.
#'
#' @param x A `piResult` object returned by `ParameterIdentification$run()`
#'
#' @return A tibble with the following columns:
#'
#' - `group`: Parameter group identifier
#' - `name`: Parameter name
#' - `path`: Full parameter path
#' - `unit`: Unit of the parameter
#' - `estimate`: Estimated parameter value after optimization
#' - `sd`: Standard deviation from CI estimation
#' - `cv`: Coefficient of variation
#' - `lowerCI`: Lower confidence bound
#' - `upperCI`: Upper confidence bound
#' - `initialValue`: Initial parameter value used for optimization
#'
#' @export
tidy <- function(x) {
  UseMethod("tidy")
}

#' @export
tidy.piResult <- function(x) {
  resultsDf <- tibble::tibble(
    group = x$parameters$group,
    name = x$parameters$name,
    path = x$parameters$path,
    unit = x$parameters$unit,
    estimate = x$finalParameters,
    sd = x$sd,
    cv = x$cv,
    lowerCI = x$lowerCI,
    upperCI = x$upperCI,
    initialValue = x$parameters$startValue
  )

  return(resultsDf)
}
