#' @title RDResult
#' @docType class
#' @description Structured output of a [`ReverseDosimetry`] run, containing the
#'   estimated parameter value, achieved PK metric values, convergence
#'   information, and parameter metadata.
#' @keywords internal
#' @format NULL
RDResult <- R6::R6Class(
  "RDResult",
  cloneable = TRUE,
  private = list(
    # Named list of result components
    .result = NULL,
    # PIParameters object used in optimization (for metadata)
    .piParameters = NULL
  ),
  public = list(
    #' @description Initialize an `RDResult` instance. For internal use only.
    #'
    #' @param optimResult Named list from `Optimizer$run()`, including `par`,
    #'   `value`, `convergence`, `algorithm`, `elapsed`, `iterations`,
    #'   `fnEvaluations`, and `startValues`.
    #' @param achievedPKValues Named list of achieved PK metric values at the
    #'   optimized parameter value, in the quantity's display unit as returned by
    #'   `ospsuite::calculatePKAnalyses()`.
    #' @param rdMappings List of `RDOutputMapping` objects used in the run.
    #' @param piParameters `PIParameters` object representing the optimized
    #'   parameter.
    #' @param configuration `PIConfiguration` used during the run.
    #'
    #' @return A new `RDResult` object.
    #'
    #' @keywords internal
    initialize = function(
      optimResult,
      achievedPKValues,
      rdMappings,
      piParameters,
      configuration
    ) {
      private$.piParameters <- piParameters

      private$.result <- list(
        estimatedValue = optimResult$par[[1]],
        unit = piParameters$unit,
        objectiveValue = optimResult$value,
        convergence = if (!is.finite(optimResult$value)) {
          FALSE
        } else {
          optimResult$convergence
        },
        algorithm = optimResult$algorithm,
        elapsed = optimResult$elapsed,
        iterations = optimResult$iterations,
        fnEvaluations = optimResult$fnEvaluations,
        initialValue = optimResult$startValues[[1]],
        achievedPKValues = achievedPKValues,
        mappings = lapply(rdMappings, function(m) {
          list(
            quantityPath = m$quantity$path,
            pkParameter = m$pkParameter,
            targetValue = m$targetValue,
            targetUnit = m$targetUnit
          )
        }),
        configuration = configuration
      )
    },

    #' @description Export the result to a `data.frame` with one row per
    #'   output mapping.
    #'
    #' @return A `data.frame` with columns:
    #' - `quantityPath`: path of the mapped simulation output
    #' - `pkParameter`: name of the PK parameter
    #' - `targetValue`: user-specified target value
    #' - `targetUnit`: unit of the target value
    #' - `achievedValue`: PK metric value at the optimized parameter value
    #' - `estimatedValue`: optimized parameter value
    #' - `parameterUnit`: unit of the optimized parameter
    toDataFrame = function() {
      x <- private$.result
      n <- length(x$mappings)

      data.frame(
        quantityPath = sapply(x$mappings, `[[`, "quantityPath"),
        pkParameter = sapply(x$mappings, `[[`, "pkParameter"),
        targetValue = sapply(x$mappings, `[[`, "targetValue"),
        targetUnit = sapply(x$mappings, `[[`, "targetUnit"),
        achievedValue = unlist(x$achievedPKValues),
        estimatedValue = rep(x$estimatedValue, n),
        parameterUnit = rep(x$unit, n),
        stringsAsFactors = FALSE
      )
    },

    #' @description Returns the full internal result as a named list.
    #' @return A named list.
    toList = function() {
      private$.result
    },

    #' @description Estimate confidence intervals for the parameter estimate.
    #'
    #' @details **Not yet implemented.** Proper CI for reverse dosimetry
    #' requires Monte Carlo sampling over PK model parameters to build a
    #' distribution of ECF (Exposure Conversion Factor) values, as described
    #' in the Approach I QIVIVE framework. This is planned for a future
    #' version.
    #'
    #' @return An error (not yet implemented).
    estimateCI = function() {
      stop(messages$errorRDCINotImplemented())
    },

    #' @description Print a summary of the `RDResult`.
    print = function() {
      ospsuite.utils::ospPrintClass(self)

      x <- private$.result

      ospsuite.utils::ospPrintItems(
        list(
          "Algorithm" = x$algorithm,
          "Convergence" = x$convergence,
          "Objective value" = .formatValues(x$objectiveValue),
          "Iterations" = x$iterations,
          "Function evaluations" = x$fnEvaluations,
          "Elapsed" = paste0(.formatValues(x$elapsed), " s"),
          "Estimated value" = paste(
            .formatValues(x$estimatedValue),
            x$unit
          ),
          "Initial value" = paste(
            .formatValues(x$initialValue),
            x$unit
          )
        ),
        title = "Reverse Dosimetry Result"
      )

      mappingSummaries <- lapply(seq_along(x$mappings), function(i) {
        m <- x$mappings[[i]]
        paste0(
          m$pkParameter,
          ": target = ",
          format(m$targetValue, digits = 4),
          " ",
          m$targetUnit,
          ", achieved = ",
          .formatValues(x$achievedPKValues[[i]])
        )
      })
      names(mappingSummaries) <- sapply(x$mappings, `[[`, "quantityPath")

      ospsuite.utils::ospPrintItems(
        mappingSummaries,
        title = "PK Metric Comparison"
      )
    }
  )
)
