#' @title PKResult
#' @docType class
#' @description Structured output of a PK-target parameter identification task,
#' including optimization results, achieved PK values, and mapping metadata.
#'
#' @keywords internal
#' @format NULL
PKResult <- R6::R6Class(
  "PKResult",
  cloneable = TRUE,
  private = list(
    .result = NULL,
    .parameters = NULL,

    .toDisplayUnit = function(val, pkParameter, targetUnit) {
      if (anyNA(val)) {
        return(NA_real_)
      }
      dim <- ospsuite::pkParameterByName(
        pkParameter,
        stopIfNotFound = TRUE
      )$dimension
      val /
        ospsuite::toBaseUnit(
          quantityOrDimension = dim,
          values = 1,
          unit = targetUnit
        )
    }
  ),
  public = list(
    #' @description Initializes a `PKResult` instance. For internal use only.
    #'
    #' @param optimResult A named list containing optimization results.
    #' @param piParameters (Optional) A list of `PIParameter` objects used in
    #'   the optimization.
    #' @param pkMappings A list of `PKOutputMapping` objects.
    #' @param achievedPKValues A list of numeric scalars with the PK parameter
    #'   values achieved at the optimized parameter estimate.
    #'
    #' @keywords internal
    initialize = function(
      optimResult,
      piParameters = NULL,
      pkMappings,
      achievedPKValues
    ) {
      private$.parameters <- tryCatch(
        {
          if (!is.null(piParameters)) {
            do.call(
              rbind,
              lapply(seq_along(piParameters), function(i) {
                piParameters[[i]]$toDataFrame(group = as.character(i))
              })
            )
          } else {
            NULL
          }
        },
        error = function(e) {
          warning(messages$warnParameterMetadata(e$message))
          NULL
        }
      )

      private$.result <- list(
        finalParameters = optimResult$par,
        objectiveValue = optimResult$value,
        initialParameters = optimResult$startValues,
        convergence = if (!is.finite(optimResult$value)) {
          FALSE
        } else {
          optimResult$convergence
        },
        algorithm = optimResult$algorithm,
        elapsed = optimResult$elapsed,
        iterations = optimResult$iterations,
        fnEvaluations = optimResult$fnEvaluations,
        pkMappings = lapply(pkMappings, function(m) {
          list(
            quantityPath = m$quantity$path,
            pkParameter = m$pkParameter,
            targetValue = m$targetValue,
            targetUnit = m$targetUnit
          )
        }),
        achievedPKValues = achievedPKValues
      )
    },
    #' @description Export PKResult to a `data.frame` in long format. Produces
    #'   N * M rows, where N is the number of optimized parameters and M is the
    #'   number of PK mappings.
    #'
    #' @return A data frame with the following columns:
    #'
    #' - `quantityPath`: Simulation quantity path
    #' - `pkParameter`: PK parameter name (e.g., `"C_max"`)
    #' - `targetValue`: Target PK value in display units
    #' - `targetUnit`: Unit of the target value
    #' - `achievedValue`: PK value achieved at the optimized parameter
    #' - `estimatedValue`: Optimized parameter value
    #' - `parameterUnit`: Unit of the estimated parameter (`NA` when
    #'   `piParameters` was not provided)
    #' - `parameterIndex`: Integer index of the optimized parameter (1..N)
    #' - `parameterPath`: Path of the optimized parameter in the simulation
    #'   (`NA` when `piParameters` was not provided)
    toDataFrame = function() {
      x <- private$.result
      m <- length(x$pkMappings)
      p <- length(x$finalParameters)

      paramInfo <- if (!is.null(private$.parameters)) {
        params <- private$.parameters
        lapply(seq_len(p), function(i) {
          rows <- params[params$group == as.character(i), , drop = FALSE]
          list(path = rows$path[[1L]], unit = rows$unit[[1L]])
        })
      } else {
        lapply(seq_len(p), function(i) {
          list(path = NA_character_, unit = NA_character_)
        })
      }
      paramPaths <- sapply(paramInfo, `[[`, "path")
      paramUnits <- sapply(paramInfo, `[[`, "unit")

      achievedVals <- mapply(
        function(mapping, val) {
          private$.toDisplayUnit(val, mapping$pkParameter, mapping$targetUnit)
        },
        x$pkMappings,
        x$achievedPKValues,
        SIMPLIFY = TRUE
      )

      mappingIdx <- rep(seq_len(m), times = p)
      paramIdx <- rep(seq_len(p), each = m)

      data.frame(
        quantityPath = sapply(x$pkMappings, `[[`, "quantityPath")[mappingIdx],
        pkParameter = sapply(x$pkMappings, `[[`, "pkParameter")[mappingIdx],
        targetValue = sapply(x$pkMappings, `[[`, "targetValue")[mappingIdx],
        targetUnit = sapply(x$pkMappings, `[[`, "targetUnit")[mappingIdx],
        achievedValue = achievedVals[mappingIdx],
        estimatedValue = x$finalParameters[paramIdx],
        parameterUnit = paramUnits[paramIdx],
        parameterIndex = paramIdx,
        parameterPath = paramPaths[paramIdx],
        stringsAsFactors = FALSE
      )
    },
    #' @description Returns the full internal result list.
    #' @return A named list containing all result values.
    toList = function() {
      private$.result
    },
    #' @description Prints a summary of `PKResult`
    print = function() {
      ospsuite.utils::ospPrintClass(self)

      x <- private$.result
      metaSummary <- list(
        "Algorithm" = x$algorithm,
        "Convergence" = x$convergence,
        "Objective value" = .formatValues(x$objectiveValue),
        "Iterations" = x$iterations,
        "Function evaluations" = x$fnEvaluations,
        "Elapsed" = paste0(.formatValues(x$elapsed), " s")
      )
      ospsuite.utils::ospPrintItems(metaSummary, title = "Optimization Summary")

      pkSummaries <- lapply(seq_along(x$pkMappings), function(i) {
        m <- x$pkMappings[[i]]
        paste0(
          "Target = ",
          .formatValues(m$targetValue),
          " ",
          m$targetUnit,
          ", Achieved = ",
          .formatValues(
            private$.toDisplayUnit(
              x$achievedPKValues[[i]],
              m$pkParameter,
              m$targetUnit
            )
          ),
          " ",
          m$targetUnit
        )
      })
      names(pkSummaries) <- sapply(x$pkMappings, `[[`, "pkParameter")
      ospsuite.utils::ospPrintItems(pkSummaries, title = "PK Parameters")
    }
  )
)
