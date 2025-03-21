#' Optimization Algorithms
#'
#' Optimization algorithms supported by optimization routines, available for use
#' in the `ParameterIdentification` class. These algorithms are configured via
#' the `PIConfiguration` class.
#'
#' @export
#' @name Algorithms
#' @details Supported algorithms include:
#' \describe{
#'   \item{\code{"HJKB"}}{Hooke-Jeeves algorithm from the \pkg{dfoptim} package.}
#'   \item{\code{"BOBYQA"}}{BOBYQA algorithm from the \pkg{nloptr} package.}
#'   \item{\code{"DEoptim"}}{Differential evolution algorithm from the \pkg{DEoptim}
#'   package, suitable for stochastic global optimization.}
#' }
#'
#' These algorithms can be specified and configured within the `PIConfiguration`
#' class to tailor the parameter identification process to specific needs.
Algorithms <- ospsuite.utils::enum(c(
  "HJKB",
  "BOBYQA",
  "DEoptim"
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
AlgorithmOptions_BOBYQA <- ospsuite.utils::enum(
  nloptr::nl.opts() |>
    (\(x) x[names(x) != "algorithm"])()
)

#' @rdname AlgorithmOptions
#' @export
AlgorithmOptions_DEoptim <- ospsuite.utils::enum(DEoptim::DEoptim.control())

#' @noRd
AlgorithmDefaults <- list(
  hjkb = AlgorithmOptions_HJKB,
  bobyqa = AlgorithmOptions_BOBYQA,
  deoptim = AlgorithmOptions_DEoptim
)

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
  linScaleCV = 0.2,
  logScaleSD = sqrt(log(1 + 0.2^2, base = 10) / log(10))
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
#'   \item \code{linScaleCV}: Coefficient of variation for linear scale, with numeric range 1e-9 to 1.
#'   \item \code{logScaleSD}: Standard deviation for log scale, with numeric range 1e-9 to Inf.
#' }
#' These options directly influence the optimization process by defining how
#' discrepancies between simulated and observed data are quantified and managed.
ObjectiveFunctionSpecs <- list(
  objectiveFunctionType = list(
    type = "character",
    allowedValues = c("lsq", "m3")
  ),
  residualWeightingMethod = list(
    type = "character",
    allowedValues = c("none", "std", "mean", "error")
  ),
  robustMethod = list(
    type = "character",
    allowedValues = c("none", "huber", "bisquare")
  ),
  scaleVar = list(type = "logical"),
  scaling = list(type = "character", allowedValues = c("lin", "log")),
  linScaleCV = list(type = "numeric", valueRange = c(1e-9, 1)),
  logScaleSD = list(type = "numeric", valueRange = c(1e-9, 1))
)

#' Scaling Options for Output Mapping
#'
#' Scaling options applicable to output mappings, impacting how simulation
#' results are compared to observed data.
#'
#' @export
#' @name ScalingOptions
#' @details Available scaling options are:
#' \describe{
#'   \item{\code{"lin"}}{Linear scaling, used by default.}
#'   \item{\code{"log"}}{Logarithmic scaling, for when data spans several orders
#'   of magnitude.}
#' }
ScalingOptions <- ospsuite.utils::enum(c(
  "lin",
  "log"
))

#' Residual Weighting Methods for Cost Function
#'
#' Methods for weighting residuals in the model cost function, affecting how
#' discrepancies between simulations and observed data are evaluated.
#'
#' @export
#' @name residualWeightingOptions
#' @details The methods include:
#' \describe{
#'   \item{\code{"none"}}{No weighting applied, treating all residuals equally.}
#'   \item{\code{"error"}}{Weights based on error estimates for the dependent
#'   variable in observed data.}
#'   \item{\code{"std"}}{Weights equal to the reciprocal of the standard deviation
#'   of the observed data.}
#'   \item{\code{"mean"}}{Weights based on the reciprocal of the mean of the absolute
#'   values of the observed data, useful for relative error scaling.}
#' }
residualWeightingOptions <- ospsuite.utils::enum(c(
  "none",
  "error",
  "std",
  "mean"
))

#' Robust Weighting Methods for Cost Function
#'
#' Robust weighting methods to address outliers in the cost function calculation
#' during parameter optimization.
#'
#' @export
#' @name robustMethodOptions
#' @details The available methods are:
#' \describe{
#'   \item{\code{"none"}}{No robust weighting applied.}
#'   \item{\code{"huber"}}{Huber weighting for moderate outliers.}
#'   \item{\code{"bisquare"}}{Bisquare (Tukey's biweight) weighting for severe outliers.}
#' }
robustMethodOptions <- ospsuite.utils::enum(c(
  "none",
  "huber",
  "bisquare"
))
