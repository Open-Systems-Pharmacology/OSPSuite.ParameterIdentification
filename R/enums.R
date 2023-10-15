#' @title Algorithms
#' List of optimization algorithms supported by optimization routines and, consequently,
#' by the ParameterIdentification class
#' @export
Algorithms <- enum(c(
  "Nelder-Mead", # Nelder-Mead algorithm from the {stats} package                                         *2
  "BFGS", # BFGS algorithm from the {stats} package                                                       !3
  "CG", # conjugate gradient algorithm from the {stats} package
  "L-BFGS-B", # L-BFGS-B (limited, bounded memory) algorithm from the {stats} package
  "SANN", # simulated annealing algorithm from the {stats} package
  "HJKB", # Hooke-Jeeves algorithm from the {dfoptim} package
  "nloptr:BOBYQA", # BOBYQA algorithm from the {nloptr} package                                           *1
  "minpack", # Levenberg-Marquardt algorithm from the {minpack.lm} package                                *4
  ## Stochastic global optimization methods
  "DEoptim", # differential evolution algorithm from the {DEoptim} package
  "PSoptim", # particle swarm algorithm from the {pso} package
  "GenOUD" # genetic optimization using derivatives from the {rgenoud} package
))

#' @title AlgorithmOptions
#' List of options for optimization algorithms, some of them specific to certain algorithms.
#' @export
AlgorithmOptions <- enum(c(
  # applies to "Marq" and "minpack" algorithms
  "ftol", # relative error of sum of squares for termination
  "ptol", # relative error of consecutive iterations for termination
  "gtol", # cosine of gradient and jacobian columns for termination
  "diag", # scale factors for parameters
  "epsfcn", # step size for numeric gradient calculation
  "factor", # initial step bound factor
  "maxfev", # maximum number of function evaluations
  "maxiter", # maximum number of iterations
  "nprint", # print every n iterations
  # applies to "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", and "SANN" algorithms
  "fnscale", # scale factor for objective function
  "parscale", # scale factors for parameters
  "ndeps", # step size for numeric gradient calculation
  "maxit", # maximum number of iterations
  "abstol", # absolute value of the objective function for termination
  "reltol", # relative error of consecutive iterations for termination
  # applies to "Nelder-Mead" algorithm
  "alpha", # reflection factor
  "beta", # contraction factor
  "gamma", # expansion factor
  "warn.1d.NelderMead", # flag to trigger a warning when used for 1-dimensional optimization
  # applies to "BFGS", "L-BFGS-B" and "SANN" algorithms
  "REPORT", # report every n iterations
  # applies to "CG" algorithm
  "type", # 1 for the Fletcher--Reeves update, 2 for Polak--Ribiere and 3 for Beale--Sorenson
  # applies to "L-BFGS-B" algorithm
  "lmm", # number of iterations retained
  "factr", # relative reduction of the objective function for termination
  "pgtol", # projected gradient for termination
  # applies to "SANN" algorithm
  "temp", # starting temperature
  "tmax", # maximum number of function evaluations at each temperature
  # applies to "HJKB" algorithm
  "tol", # absolute error of consecutive iterations for termination
  "maxfeval", # maximum number of function evaluations
  "maximize", # flag to maximize function instead
  "target", # target objective function value for termination
  "info", # reporting settings
  # applies to "nloptr:BOBYQA" and "nloptr:NM" algorithms
  "stopval", # target objective function value for termination
  "xtol_rel", # relative tolerance for parameter step for termination
  "maxeval", # maximum number of function evaluations
  "ftol_rel", # relative tolerance for objective value for termination
  "ftol_abs", # absolute tolerance for objective value for termination
  # applies to "solnp" algorithm
  "rho", # penalty factor for infeasible solutions
  "outer.iter", # maximum number of iterations
  "inner.iter", # maximum number of inner iterations
  "delta", # relative step size for numeric gradient calculations
  "tol", # relative tolerance for termination
  "trace", # print every n iterations
  # applies to "PSoptim" algorithm
  "trace", # reporting settings
  "fnscale", # scale factor for objective function
  "maxit", # maximum number of iterations
  "maxf", # maximum number of function evaluations
  "abstol", # absolute tolerance for objective value for termination
  "reltol", # relative tolerance between the best particle and all other particles for termination
  "REPORT", # report every n iterations
  "trace.stats", # flag to collect statistics at each iteration
  "s", # swarm size
  "k", # exponent
  "p", # average percentage of informants
  "w", # exploitation constant
  "c.p", # local exploration constant
  "c.g", # global exploration constant
  "d", # diameter of the search space
  "v.max", # maximum length of the velocity vector
  "rand.order", # flag to process particles in a random order
  "max.restart", # restarts before failure
  "maxit.stagnate", # maximum number of iterations without improvement
  "vectorize", # flag to process particles in a vectorized manner
  "hybrid", # flag to perform BFGS search after each PSO iteration
  "type" # which reference PSO implementation is used
  ## Note that "DEoptim" and "GenOUD" algorithms have their own parameters which are not passed through the `control` argument.
  ## Instead, they are currently set inside the task setup.
))

#' @title ObjectiveFunctions
#' List of supported objective functions to calculate the error.
#' @export
ObjectiveFunctions <- enum(c("lsq", "m3"))
