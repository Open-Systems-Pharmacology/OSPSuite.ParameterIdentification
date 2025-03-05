#' @title Optimizer Class
#'
#' @description Internal class for handling optimization in parameter identification.
#' Provides a unified interface for different optimization algorithms.
#'
#' @noRd
Optimizer <- R6::R6Class(
  "Optimizer",
  inherit = ospsuite.utils::Printable,
  cloneable = FALSE,
  active = list(
    #' @field algorithm Optimization algorithm to be used.
    algorithm = function(value) {
      if (missing(value)) {
        private$.algorithm
      } else {
        ospsuite.utils::validateIsCharacter(value)
        ospsuite.utils::validateEnumValue(value, Algorithms)
        private$.algorithm <- value
      }
    }
  ),
  private = list(
    # Optimization algorithm name
    .algorithm = NULL,
    # Field name in the objective function result that stores the model cost
    .modelCostField = NULL,
    # Optimizer-specific control options
    .controlOptim = NULL,

    # Hooke-Jeeves optimization using dfoptim::hjkb
    .runHJKB = function(par, fn, lower, upper, controlOptim, fixedParams = NULL) {
      dfoptim::hjkb(
        par = par,
        fn = if (!is.null(fixedParams)) {
          function(p) fn(private$.updateFixedParams(p, fixedParams))
        } else {
          fn
        },
        lower = lower,
        upper = upper,
        control = controlOptim
      )
    },

    # BOBYQA optimization using nloptr::bobyqa
    .runBOBYQA = function(par, fn, lower, upper, controlOptim, fixedParams = NULL) {
      nloptr::bobyqa(
        x0 = par,
        fn = if (!is.null(fixedParams)) {
          function(p) fn(private$.updateFixedParams(p, fixedParams))
        } else {
          fn
        },
        lower = lower,
        upper = upper,
        control = controlOptim
      )
    },

    # Differential Evolution optimization using DEoptim::DEoptim
    .runDEoptim = function(par, fn, lower, upper, controlOptim, fixedParams = NULL) {
      DEoptim::DEoptim(
        fn = if (!is.null(fixedParams)) {
          function(p) fn(private$.updateFixedParams(p, fixedParams))
        } else {
          fn
        },
        lower = lower,
        upper = upper,
        control = controlOptim
      )
    },

    # Update fixed parameters in the parameter vector
    .updateFixedParams = function(par, fixedParams) {
      if (is.null(fixedParams)) {
        return(par)
      }
      if (length(fixedParams$idx) >= length(par)) {
        stop(messages$fixedParamError("fixed"))
      }
      if (length(fixedParams$idx) != length(fixedParams$values)) {
        stop(messages$fixedParamError("length"))
      }

      par[fixedParams$idx] <- fixedParams$values

      return(par)
    },

    # Converts the objective function to handle fixed parameters and extract modelCost
    .preprocessFn = function(fn, fixedParams) {
      # If fn is already preprocessed, return it to prevent infinite recursion
      if (inherits(fn, "preprocessedFn")) {
        return(fn)
      }

      wrappedFn <- function(p) {
        result <- fn(p)
        if (is.list(result)) {
          if (!private$.modelCostField %in% names(result)) {
            stop(messages$objectiveFnOutputError(private$.modelCostField))
          }
          return(purrr::pluck(result, private$.modelCostField))
        }

        return(result)
      }

      class(wrappedFn) <- "preprocessedFn"

      return(wrappedFn)
    },

    # Formats the optimization output into a standardized result structure
    .formatOptimizationOutput = function(optimResult) {
      baseResult <- list(
        par = NULL,
        value = NULL,
        convergence = NULL,
        iterations = NULL,
        fnEvaluations = NULL,
        algorithm = private$.algorithm,
        elapsed = NULL
      )

      if (private$.algorithm == "HJKB") {
        baseResult$par <- optimResult$par
        baseResult$value <- optimResult$value
        baseResult$convergence <- optimResult$convergence == 0
        baseResult$iterations <- optimResult$niter
        baseResult$fnEvaluations <- optimResult$feval
      } else if (private$.algorithm == "BOBYQA") {
        baseResult$par <- optimResult$par
        baseResult$value <- optimResult$value
        baseResult$convergence <- optimResult$convergence > 0
        if (!baseResult$convergence) {
          baseResult$convergence$error <- optimResult$convergence
        }
        baseResult$iterations <- optimResult$iter
        baseResult$fnEvaluations <- optimResult$iter
      } else if (private$.algorithm == "DEoptim") {
        baseResult$par <- unname(optimResult$optim$bestmem)
        baseResult$value <- optimResult$optim$bestval
        baseResult$convergence <- NA
        baseResult$iterations <- optimResult$optim$iter
        baseResult$fnEvaluations <- optimResult$optim$nfeval
      }

      return(baseResult)
    }
  ),
  public = list(
    #' @description Initialize an Optimizer instance.
    #' @param algorithm The optimization algorithm to use ("HJKB", "BOBYQA", "DEoptim").
    #' @param modelCostField Optional field name in the objective function result
    #' that stores the model cost.
    initialize = function(algorithm, modelCostField = NULL) {
      algorithm <- match.arg(algorithm, c("HJKB", "BOBYQA", "DEoptim"))

      private$.algorithm <- algorithm
      private$.modelCostField <- modelCostField %||% "modelCost"
    },

    #' @description Run the optimization process with the given parameters.
    #' @param par Numeric vector of parameter values.
    #' @param fn Objective function to be minimized.
    #' @param lower Numeric vector of lower parameter bounds.
    #' @param upper Numeric vector of upper parameter bounds.
    #' @param controlOptim Optional list of control parameters for the optimizer.
    #' @param fixedParams Optional list specifying fixed parameters.
    #' The list must contain two named elements:
    #' - `idx`: A numeric vector specifying the indices of parameters to be fixed.
    #' - `values`: A numeric vector of the same length as `idx`, specifying the
    #' values to fix the corresponding parameters at.
    #' If `fixedParams` is provided, optimization proceeds while keeping the
    #' specified parameters constant.
    #' @return A list containing the optimization results.
    run = function(par, fn, lower, upper, controlOptim = NULL, fixedParams = NULL) {
      ospsuite.utils::validateIsNumeric(par)
      ospsuite.utils::validateIsOfType(fixedParams, "list", TRUE)
      if (!inherits(fn, "preprocessedFn")) {
        ospsuite.utils::validateIsOfType(fn, "function", FALSE)
      }

      controlOptim <- controlOptim %||% AlgorithmDefaults[[private$.algorithm]]

      fn <- private$.preprocessFn(fn)

      optimizeMethods <- list(
        "HJKB" = private$.runHJKB,
        "BOBYQA" = private$.runBOBYQA,
        "DEoptim" = private$.runDEoptim
      )

      optimizeFn <- optimizeMethods[[private$.algorithm]] %||%
        stop(messages$unknownAlgorithmError(private$.algorithm))

      startTime <- proc.time()
      rawResult <- optimizeFn(
        par, fn, lower, upper,
        control = controlOptim, fixedParams
      )
      elapsedTime <- proc.time() - startTime

      optimResult <- private$.formatOptimizationOutput(rawResult)
      optimResult$elapsed <- elapsedTime[["elapsed"]]

      optimResult$par <- private$.updateFixedParams(optimResult$par, fixedParams)

      return(optimResult)
    }
  )
)
