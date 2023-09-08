#' @title Algorithms
#' List of optimization algorithms supported by optimization routines and, consequently,
#' by the ParameterIdentification class
#' @export
Algorithms <- enum(c(
  "bobyqa",       # BOBYQA (bound optimization by quadratic approximation) algorithm from the {FME} package
  "Marq",         # Levenberg-Marquardt algorithm from the {FME} package
  "Nelder-Mead",  # Nelder-Mead algorithm from the {stats} package
  "BFGS",         # BFGS algorithm from the {stats} package
  "CG",           # conjugate gradient algorithm from the {stats} package
  "L-BFGS-B",     # L-BFGS-B (limited, bounded memory) algorithm from the {stats} package
  "SANN",         # simulated annealing algorithm from the {stats} package
  "minqa",        # BOBYQA algorithm from the {minqa} package
  "NMKB",         # bounded version of the Nelder-Mead algorithm from the {dfoptim} package
  "HJKB",         # Hooke-Jeeves algorithm from the {dfoptim} package
  "nloptr:BOBYQA",# BOBYQA algorithm from the {nloptr} package
  "nloptr:NM",    # Nelder-Mead algorithm from the {nloptr} package
  "solnp",        # augmented Lagrange algorithm from the {Rsolnp} package
  "marqLevAlg",   # Levenberg-Marquardt algorithm from the {marqLevAlg} package
  "minpack",      # Levenberg-Marquardt algorithm from the {minpack.lm} package
  ## Stochastic global optimization methods
  "DEoptim",      # differential evolution algorithm from the {DEoptim} package
  "PSoptim",      # particle swarm algorithm from the {pso} package
  "GenOUD"        # genetic optimization using derivatives from the {rgenoud} package
))

#' @title AlgorithmOptions
#' List of options for optimization algorithms, some of them specific to certain algorithms.
#' @export
AlgorithmOptions <- enum(c(
  # applies to "Marq" and "minpack" algorithms
  "ftol",                     # relative error of sum of squares for termination
  "ptol",                     # relative error of consecutive iterations for termination
  "gtol",                     # cosine of gradient and jacobian columns for termination
  "diag",                     # scale factors for parameters
  "epsfcn",                   # step size for numeric gradient calculation
  "factor",                   # initial step bound factor
  "maxfev",                   # maximum function evaluations
  "maxiter",                  # maximum iterations
  "nprint",                   # print every n iterations
  # applies to "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", and "SANN" algorithms
  "fnscale",                  # scale factor for objective function
  "parscale",                 # scale factors for parameters
  "ndeps",                    # step size for numeric gradient calculation
  "maxit",                    # maximum iterations
  "abstol",                   # absolute value of the objective function for termination
  "reltol",                   # relative error of consecutive iterations for termination
  # applies to "Nelder-Mead" algorithm
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
#' @export
ObjectiveFunctions <- enum(c("lsq", "m3"))
