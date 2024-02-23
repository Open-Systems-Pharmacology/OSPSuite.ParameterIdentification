#' @title Algorithms
#' List of optimization algorithms supported by optimization routines and, consequently,
#' by the ParameterIdentification class
#' @export
Algorithms <- ospsuite.utils::enum(c(
  "HJKB", # Hooke-Jeeves algorithm from the {dfoptim} package
  "BOBYQA", # BOBYQA algorithm from the {nloptr} package
  ## Stochastic global optimization methods
  "DEoptim" # differential evolution algorithm from the {DEoptim} package
))

#' @title AlgorithmOptions_HJKB
#' @description
#' Default options for the HJKB algorithm.
#' see [dfoptim::hjkb()] for details.
#'
#' @export
AlgorithmOptions_HJKB <- enum(list(
  tol = 1e-06, maxfeval = Inf, maximize = FALSE,
  target = Inf, info = FALSE
))

#' @title AlgorithmOptions_BOBYQA
#' @description
#' Default options for the BOBYQA algorithm.
#' see [nloptr::nl.opts()] for details.
#' @export
AlgorithmOptions_BOBYQA <- enum(nloptr::nl.opts())

#' @title AlgorithmOptions_DEoptim
#' @description
#' Default options for the DEoptim algorithm.
#' see [DEoptim::DEoptim.control()] for details.
#'
#' @export
AlgorithmOptions_DEoptim <- enum(DEoptim::DEoptim.control())


#' @title ObjectiveFunctions
#' List of supported objective functions to calculate the error.
#' @export
ObjectiveFunctionOptions <- list(
  objectiveFunctionType = c("lsq", "m3"),
  residualWeightingMethod = c("none", "std", "mean", "error"),
  robustMethod = c("none", "huber", "bisquare"),
  scaleVar = c(TRUE, FALSE),
  scaling = c("lin", "log"),
  linScaleCV = list(type = "numeric", min = 1e-9, max = 1),
  logScaleSD = list(type = "numeric", min = 1e-9, max = Inf)
)

#' @title ScalingOptions
#' List of scaling options for output mappings.
#' @export
ScalingOptions <- ospsuite.utils::enum(c(
  "lin", # Linear scaling (default)
  "log" # Logarithmic scaling
))

#' @title residualWeightingOptions
#' List of residual weighting methods used in the cost function.
#' @export
residualWeightingOptions <- ospsuite.utils::enum(c(
  "none", # no weighting
  "error", # error estimates for dependent variable in observed data
  "std", #  weights equal to the reciprocal of the standard deviation of the observed data
  "mean" # 1/mean of the absolute value of the observed data
))

#' @title robustMethodOptions
#' List of robust weighting methods used in the cost function for handling outliers.
#' @export
robustMethodOptions <- ospsuite.utils::enum(c(
  "none", # No robust weighting applied
  "huber", # Huber weighting for moderate outliers
  "bisquare" # Bisquare (Tukey's biweight) weighting for severe outliers
))
