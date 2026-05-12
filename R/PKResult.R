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
    .parameters = NULL
  ),
  public = list(
    #' @description Initializes a `PKResult` instance. For internal use only.
    #'
    #' @param optimResult A named list containing optimization results.
    #' @param piParameters (Optional) A list of `PIParameter` objects used in
    #'   the optimization.
    #' @param pkMappings A list of `PKOutputMapping` objects.
    #' @param achievedPKValues A list of numeric scalars with the PK metric
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
    #' @description Export PKResult to a `data.frame`.
    #'
    #' @return A data frame with the following columns:
    #'
    #' - `quantityPath`: Simulation quantity path
    #' - `pkParameter`: PK parameter name (e.g., `"C_max"`)
    #' - `targetValue`: Target PK value in display units
    #' - `targetUnit`: Unit of the target value
    #' - `achievedValue`: PK value achieved at the optimized parameter
    #' - `estimatedValue`: Optimized parameter value
    #' - `parameterUnit`: Unit of the estimated parameter
    toDataFrame = function() {
      x <- private$.result
      if (length(x$finalParameters) != 1L) {
        stop(messages$errorPKResultMultipleParameters())
      }
      n <- length(x$pkMappings)
      paramUnit <- if (!is.null(private$.parameters)) {
        private$.parameters$unit[[1L]]
      } else {
        NA_character_
      }
      data.frame(
        quantityPath = sapply(x$pkMappings, `[[`, "quantityPath"),
        pkParameter = sapply(x$pkMappings, `[[`, "pkParameter"),
        targetValue = sapply(x$pkMappings, `[[`, "targetValue"),
        targetUnit = sapply(x$pkMappings, `[[`, "targetUnit"),
        achievedValue = unlist(x$achievedPKValues),
        estimatedValue = rep(x$finalParameters[[1L]], n),
        parameterUnit = rep(paramUnit, n),
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
          .formatValues(x$achievedPKValues[[i]])
        )
      })
      names(pkSummaries) <- sapply(x$pkMappings, `[[`, "pkParameter")
      ospsuite.utils::ospPrintItems(pkSummaries, title = "PK Targets")
    }
  )
)
