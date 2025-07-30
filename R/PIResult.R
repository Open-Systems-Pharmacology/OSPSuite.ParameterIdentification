#' @title PIResult
#' @docType class
#' @description
#' Structured output of a parameter identification task, including optimization
#' results, confidence intervals, parameter metadata, and configuration.
#'
#' @keywords internal
#' @format NULL
PIResult <- R6::R6Class(
  "PIResult",
  cloneable = TRUE,
  private = list(
    # Named list of all result components (optimization, CI)
    .result = NULL,
    # Detailed cost structure from objective function evaluation
    .costDetails = NULL,
    # Configuration used during parameter identification
    .configuration = NULL,
    # Flattened PIParameter metadata
    .parameters = NULL
  ),
  public = list(
    #' @description Initializes a `PIResult` instance. For internal use only.
    #'
    #' This constructor is used internally by the parameter identification
    #' workflow to store and standardize results from optimization and confidence
    #' interval estimation.
    #'
    #' @param optimResult A named list containing optimization results. Typically
    #' produced internally by `Optimizer$run()` and includes fields such as
    #' `par`, `value`, `startValues`, `elapsed`, `convergence`, etc.
    #' @param ciResult (Optional) A named list of confidence interval results,
    #' returned by `Optimizer$estimateCI()`. Contains fields like `sd`, `cv`,
    #' `lowerCI`, `upperCI`, `elapsed`, and `details`.
    #' @param costDetails (Optional) A named list with detailed cost metrics
    #' computed during optimization.
    #' @param configuration (Optional) The `PIConfiguration` object used during
    #' parameter identification.
    #' @param piParameters (Optional) A list of `PIParameter` objects used
    #' in the optimization. If not provided, default parameter names will be
    #' generated automatically.
    #'
    #' @return A `PIResult` object containing optimization results, confidence
    #' interval estimates (if available), parameter metadata (if available), and
    #' configuration.
    #'
    #' @keywords internal
    #
    # Internal structure of `PIResult` ----
    #
    # Stored in private$.result:
    # - finalParameters: numeric vector (optimized parameter values)
    # - objectiveValue: numeric (final cost)
    # - initialParameters: numeric vector
    # - convergence: logical
    # - algorithm: character
    # - elapsed: numeric (seconds)
    # - iterations: integer
    # - fnEvaluations: integer
    #
    # If CI is estimated (ciResult):
    # - sd: numeric vector
    # - cv: numeric vector
    # - lowerCI: numeric vector
    # - upperCI: numeric vector
    # - ciMethod: character
    # - ciElapsed: numeric
    # - ciError: error object or NULL
    # - ciDetails: list (e.g. covariance matrix, bootstrap samples)
    initialize = function(optimResult, ciResult = NULL, costDetails = NULL,
                          configuration = NULL, piParameters = NULL) {
      private$.configuration <- configuration
      private$.costDetails <- costDetails

      # Validate and extract parameter metadata (optional)
      private$.parameters <- tryCatch(
        {
          if (!is.null(piParameters)) {
            do.call(rbind, lapply(seq_along(piParameters), function(i) {
              piParameters[[i]]$toDataFrame(group = as.character(i))
            }))
          } else {
            NULL
          }
        },
        error = function(e) {
          warning(messages$warnParameterMetadata(e$message))
          NULL
        }
      )

      # Validate if parameter names match data
      paramNames <- tryCatch(
        {
          if (!is.null(private$.parameters)) {
            dplyr::distinct(private$.parameters, group, .keep_all = TRUE)$name
          } else {
            paste0("par", seq_along(optimResult$par))
          }
        },
        error = function(e) {
          paste0("par", seq_along(optimResult$par))
        }
      )

      # Construct results list
      ciResult <- ciResult %||% list()

      private$.result <- list(
        finalParameters = optimResult$par,
        objectiveValue = optimResult$value,
        initialParameters = optimResult$startValues,
        convergence = if (!is.finite(optimResult$value)) FALSE else optimResult$convergence,
        algorithm = optimResult$algorithm,
        elapsed = optimResult$elapsed,
        iterations = optimResult$iterations,
        fnEvaluations = optimResult$fnEvaluations,
        ciMethod = ciResult$method %||% configuration$ciMethod,
        ciElapsed = ciResult$elapsed %||% NA_real_,
        ciError = ciResult$error,
        ciDetails = ciResult$details %||% list(),
        sd = ciResult$sd %||% rep(NA_real_, length(optimResult$par)),
        cv = ciResult$cv %||% rep(NA_real_, length(optimResult$par)),
        lowerCI = ciResult$lowerCI %||% rep(NA_real_, length(optimResult$par)),
        upperCI = ciResult$upperCI %||% rep(NA_real_, length(optimResult$par)),
        paramNames = paramNames
      )
    },
    #' @description Export PIResult to a `data.frame`.
    #'
    #' @return A data frame with the following columns:
    #'
    #' - `group`: Parameter group identifier
    #' - `name`: Parameter name
    #' - `path`: Full parameter path
    #' - `unit`: Unit of the parameter
    #' - `estimate`: Estimated parameter value after optimization
    #' - `sd`: Standard deviation from CI estimation
    #' - `cv`: Coefficient of variation
    #' - `lowerCI`: Lower confidence bound
    #' - `upperCI`: Upper confidence bound
    #' - `initialValue`: Initial parameter value used for optimization
    toDataFrame = function() {
      params <- dplyr::distinct(private$.parameters, group, .keep_all = TRUE)
      data.frame(
        group = params$group,
        name = params$name,
        path = params$path,
        unit = params$unit,
        estimate = private$.result$finalParameters,
        sd = private$.result$sd,
        cv = private$.result$cv,
        lowerCI = private$.result$lowerCI,
        upperCI = private$.result$upperCI,
        initialValue = private$.result$initialParameters,
        stringsAsFactors = FALSE
      )
    },
    #' @description Returns the full internal result list.
    #' @return A named list containing all result values.
    toList = function() {
      c(private$.result, list(
        costDetails = private$.costDetails
      ))
    },
    #' @description Prints a summary of PIResult
    print = function() {
      ospsuite.utils::ospPrintClass(self)

      # Summary metadata
      x <- private$.result
      metaSummary <- list(
        "Algorithm" = x$algorithm,
        "CI Method" = x$ciMethod,
        "Convergence" = x$convergence,
        "Objective value" = .formatValues(x$objectiveValue),
        "Iterations" = x$iterations,
        "Function evaluations" = x$fnEvaluations,
        "Elapsed (optimization)" = paste0(.formatValues(x$elapsed), " s"),
        "Elapsed (CI)" = if (is.null(x$ciElapsed) || is.na(x$ciElapsed)) {
          NA
        } else {
          paste0(.formatValues(x$ciElapsed), " s")
        }
      )

      ospsuite.utils::ospPrintItems(metaSummary, title = "Optimization Summary")

      # Parameters table
      paramSummaries <- lapply(seq_along(x$paramNames), function(i) {
        paste0(
          "Estimate = ", .formatValues(x$finalParameters[i]), ", ",
          "SD = ", .formatValues(x$sd[i]), ", ",
          "CV = ", .formatValues(x$cv[i]), ", ",
          "CI = [", .formatValues(x$lowerCI[i]), ", ",
          .formatValues(x$upperCI[i]), "]"
        )
      })
      names(paramSummaries) <- x$paramNames

      ospsuite.utils::ospPrintItems(paramSummaries, title = "Parameter Estimates")
    }
  )
)
