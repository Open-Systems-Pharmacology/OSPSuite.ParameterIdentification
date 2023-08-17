#' @title Algorithms
#' List of optimization algorithms supported by optimization routines and, consequently,
#' by the ParameterIdentification class
Algorithms <- enum(c(
  "bobyqa",
  "Marq",
  "Nelder-Mead",
  "BFGS",
  "CG",
  "L-BFGS-B",
  "SANN",
  "minqa",
  "NMKB",
  "HJKB",
  "nloptr:BOBYQA",
  "nloptr:NM",
  "solnp",
  "DEoptim",
  "PSoptim", 
  "GenOUD"
))

#' @title AlgorithmOptions
#' List of options for optimization algorithms, some of them specific to certain algorithms.
#' See documentation of `FME::modFit()` for details.
AlgorithmOptions <- enum(c(
  # from the nls.lm.control routine
  "ftol",
  "ptol",
  "gtol",
  "diag",
  "epsfcn",
  "factor",
  "maxfev",
  "maxiter",
  "nprint",
  # from the nlminb routine
  "eval.max",
  "iter.max",
  "trace",
  "abs.tol",
  "rel.tol",
  "x.tol",
  "xf.tol",
  "step.min",
  "step.max",
  "sing.tol",
  "scale.init",
  "diff.g",
  # from the optim routine
  "fnscale",
  "parscale",
  "ndeps",
  "maxit",
  "abstol",
  "reltol",
  # specific to Nelder-Mead
  "alpha",
  "beta",
  "gamma",
  "REPORT",
  "warn.1d.NelderMead",
  # specific to conjugate gradient method
  "type",
  # specific to L-BFGS-B
  "lmm",
  "factr",
  "pgtol",
  # specific to SANN
  "temp",
  "tmax",
  # specific to pseudo-random search
  "npop",
  "numiter",
  "centroid",
  "varleft",
  "verbose",
  # specific to bobyqa
  "npt",
  "rhobeg",
  "rhoend",
  "iprint",
  "maxfun"
))

#' @title ObjectiveFunctions
#' List of supported objective functions to calculate the error.
ObjectiveFunctions <- enum(c("lsq", "m3"))
