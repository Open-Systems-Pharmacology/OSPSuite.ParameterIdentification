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

#' @title AlgotitmOptions_HJKB
#' @description
#' Default options for the HJKB algorithm.
#' see [dfoptim::hjkb()] for details.
#'
#' @export
AlgotitmOptions_HJKB <- enum(list(tol = 1e-06, maxfeval = Inf, maximize = FALSE,
                             target = Inf, info = FALSE))

#' @title AlgotitmOptions_BOBYQA
#' @description
#' Default options for the BOBYQA algorithm.
#' see [nloptr::nl.opts()] for details.
#' @export
AlgotitmOptions_BOBYQA <- enum(nloptr::nl.opts())

#' @title AlgotitmOptions_DEoptim
#' @description
#' Default options for the DEoptim algorithm.
#' see [DEoptim::DEoptim.control()] for details.
#'
#' @export
AlgotitmOptions_DEoptim <- enum(DEoptim::DEoptim.control())


#' @title ObjectiveFunctions
#' List of supported objective functions to calculate the error.
#' @export
ObjectiveFunctions <- enum(c("lsq", "m3"))
