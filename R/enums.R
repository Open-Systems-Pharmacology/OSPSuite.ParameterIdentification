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

#' @title AlgorithmOptions
#' List of options for optimization algorithms, some of them specific to certain algorithms.
#' @export
AlgorithmOptions <- ospsuite.utils::enum(c(
  # applies to "HJKB" algorithm
  "tol", # absolute error of consecutive iterations for termination
  "maxfeval", # maximum number of function evaluations
  "maximize", # flag to maximize function instead
  "target", # target objective function value for termination
  "info", # reporting settings
  # applies to "BOBYQA" algorithm
  "stopval", # target objective function value for termination
  "xtol_rel", # relative tolerance for parameter step for termination
  "maxeval", # maximum number of function evaluations
  "ftol_rel", # relative tolerance for objective value for termination
  "ftol_abs", # absolute tolerance for objective value for termination
  "check_derivatives", # FALSE by default
  # applies and passed on to the "DEoptim" algorithm
  "VTR", # target objective function value for termination
  "strategy", # strategy for differential evolution
  "bs", # strategy for selection
  "NP", # population size
  "itermax", # maximum number of iterations
  "CR", # crossover probability
  "F", # differential weight
  "trace", # reporting settings
  "reltol", # relative tolerance for termination
  "steptol" # step count before checking relative tolerance
))

#' @title ObjectiveFunctions
#' List of supported objective functions to calculate the error.
#' @export
ObjectiveFunctions <- ospsuite.utils::enum(c("lsq", "m3"))

#' @title ScalingOptions
#' List of scaling options for output mappings.
#' @export
ScalingOptions <- ospsuite.utils::enum(c(
  "lin", # Linear scaling (default)
  "log"  # Logarithmic scaling
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
