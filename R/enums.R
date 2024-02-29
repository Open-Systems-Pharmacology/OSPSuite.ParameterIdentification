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

#' Algorithm Options for Optimization Algorithms
#'
#' Default options for various optimization algorithms used within the package.
#' These algorithms are configured via the `PIConfiguration` class.
#'
#' @section AlgorithmOptions_HJKB:
#' Default options for the HJKB algorithm. See [dfoptim::hjkb()] for more details
#' on the options.
#'
#' @section AlgorithmOptions_BOBYQA:
#' Default options for the BOBYQA algorithm. See [nloptr::nl.opts()] for more
#' details on the options.
#'
#' @section AlgorithmOptions_DEoptim:
#' Default options for the DEoptim algorithm. See [DEoptim::DEoptim.control()]
#' for more details on the options.
#'
#' @name AlgorithmOptions
#' @rdname AlgorithmOptions
#' @export
AlgorithmOptions_HJKB <- ospsuite.utils::enum(list(
  tol = 1e-06, maxfeval = Inf, maximize = FALSE,
  target = Inf, info = FALSE
))

#' @rdname AlgorithmOptions
#' @export
AlgorithmOptions_BOBYQA <- ospsuite.utils::enum(nloptr::nl.opts())

#' @rdname AlgorithmOptions
#' @export
AlgorithmOptions_DEoptim <- ospsuite.utils::enum(DEoptim::DEoptim.control())

#' Objective Function Options for Model Fit Assessment
#'
#' Default settings for objective function options in model fit analysis,
#' pivotal for calculating error and fit. These configurations enable tailored
#' analysis approaches, pivotal in the `ParameterIdentification` process for
#' optimizing parameter values against observed data. These options are
#' configured via the `PIConfiguration` class.
#'
#' @export
#' @name ObjectiveFunctionOptions
#' @details Settings include:
#' \describe{
#'   \item{\code{objectiveFunctionType}}{"lsq" for least squares; influences error calculation.}
#'   \item{\code{residualWeightingMethod}}{"none" by default; specifies method for residual weighting.}
#'   \item{\code{robustMethod}}{"none" for standard analysis; selects method for robust outlier handling.}
#'   \item{\code{scaleVar}}{FALSE by default; determines if scaling is applied to residuals.}
#'   \item{\code{scaling}}{"lin" for linear scaling; affects data scaling approach.}
#'   \item{\code{linScaleCV}}{0.2; coefficient of variation for linear scaling.}
#'   \item{\code{logScaleSD}}{NULL; standard deviation for log scaling.}
#' }
#' These options are configurable in `PIConfiguration`, directly influencing the
#' `calculateCostMetrics` functionality for detailed model fit assessment.
ObjectiveFunctionOptions <- ospsuite.utils::enum(list(
  objectiveFunctionType = "lsq",
  residualWeightingMethod = "none",
  robustMethod = "none",
  scaleVar = FALSE,
  scaling = "lin",
  linScaleCV = 0.2,
  logScaleSD = NULL
))

#' Objective Function Specifications
#'
#' Specifies supported objective function configurations for error calculation
#' in parameter optimization. These specifications detail the allowable options
#' for tailoring how model fit is assessed, configured within `PIConfiguration`.
#'
#' @export
#' @name ObjectiveFunctionSpecs
#' @details Includes:
#' \itemize{
#'   \item \code{objectiveFunctionType}: Types of error calculations ("lsq" for least squares, "m3" for method 3).
#'   \item \code{residualWeightingMethod}: Methods for weighting residuals ("none", "std", "mean", "error").
#'   \item \code{robustMethod}: Approaches for robust outlier handling ("none", "huber", "bisquare").
#'   \item \code{scaleVar}: Boolean for variance scaling (TRUE, FALSE).
#'   \item \code{scaling}: Data scaling methods ("lin" for linear, "log" for logarithmic).
#'   \item \code{linScaleCV}: Coefficient of variation for linear scale, with numeric range 1e-9 to 1.
#'   \item \code{logScaleSD}: Standard deviation for log scale, with numeric range 1e-9 to Inf.
#' }
#' These options directly influence the optimization process by defining how
#' discrepancies between simulated and observed data are quantified and managed.
ObjectiveFunctionSpecs <- list(
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
