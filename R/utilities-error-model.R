#' Select the error model implied by a residual weighting method
#'
#' `none` leaves the absolute residual spread unmeasured, so the scale is
#' concentrated. `error` measures it per observation, so the scale is fixed.
#'
#' @param residualWeightingMethod One of `residualWeightingOptions`.
#' @return An `ErrorModels` value.
#' @keywords internal
#' @noRd
.errorModelFor <- function(residualWeightingMethod) {
  switch(
    residualWeightingMethod,
    "none" = ErrorModels$constant,
    "error" = ErrorModels$dataError,
    stop(messages$errorUnknownErrorModelSource(residualWeightingMethod))
  )
}

#' Negative log-likelihood from aggregated sufficient statistics
#'
#' Implements the Gaussian negative log-likelihood with the residual standard
#' deviation written as a known shape times a scale, `sigma_i = c / (s * w_i)`.
#' Under `constant` the scale is unknown and is concentrated at its
#' maximum-likelihood value; under `dataError` it is measured, so `c = 1`.
#'
#' @param weightedSSR Aggregated weighted sum of squared residuals.
#' @param nObservations Aggregated observation count.
#' @param sumLogSigma Aggregated `-sum(log(s * w_i))`.
#' @param errorModel An `ErrorModels` value.
#' @return A numeric scalar.
#' @keywords internal
#' @noRd
.negLogLikelihood <- function(
  weightedSSR,
  nObservations,
  sumLogSigma,
  errorModel
) {
  ospsuite.utils::validateEnumValue(errorModel, ErrorModels)
  # No observation carries likelihood information. Returning zero leaves any
  # censored contribution as the whole objective.
  if (nObservations == 0) {
    return(0)
  }

  gaussianConstant <- nObservations / 2 * log(2 * pi)

  switch(
    errorModel,
    "constant" = {
      # An exact fit drives the concentrated scale to zero, where the
      # log-likelihood is unbounded. Floor it so the objective stays finite.
      scaleSquared <- max(weightedSSR / nObservations, .Machine$double.eps)
      gaussianConstant +
        sumLogSigma +
        nObservations * log(sqrt(scaleSquared)) +
        nObservations / 2
    },
    "dataError" = gaussianConstant + sumLogSigma + weightedSSR / 2
  )
}
