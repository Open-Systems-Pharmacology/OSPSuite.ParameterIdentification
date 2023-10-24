#' @title Algorithms
#' List of optimization algorithms supported by optimization routines and, consequently,
#' by the ParameterIdentification class
#' @export
Algorithms <- enum(c(
  "HJKB", # Hooke-Jeeves algorithm from the {dfoptim} package
  "BOBYQA", # BOBYQA algorithm from the {nloptr} package
  ## Stochastic global optimization methods
  "DEoptim" # differential evolution algorithm from the {DEoptim} package
))

#' @title AlgorithmOptions
#' List of options for optimization algorithms, some of them specific to certain algorithms.
#' @export
AlgorithmOptions <- enum(c(
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
  "check_derivatives" # FALSE by default
  ## Note that "DEoptim" algorithm has its own parameters which are not passed through the `control` argument.
  ## Instead, they are currently set inside the task setup.
))

#' @title ObjectiveFunctions
#' List of supported objective functions to calculate the error.
#' @export
ObjectiveFunctions <- enum(c("lsq", "m3"))
