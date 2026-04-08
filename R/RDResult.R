#' @title RDResult
#' @docType class
#' @description Structured output of a reverse dosimetry run, containing the
#'   estimated external dose, achieved PK metric values, convergence
#'   information, and parameter metadata.
#'
#' @details
#' `RDResult` is returned by [`ReverseDosimetry`]`$run()`. It stores the
#' optimized dose that yields the specified in vitro target concentrations in
#' the PBPK simulation, together with the corresponding PK metric values
#' achieved at that dose.
#'
#' **Confidence intervals (v2)**: The `$estimateCI()` method is reserved for a
#' future implementation based on Monte Carlo sampling over PK model parameters,
#' as described in the QIVIVE uncertainty analysis framework. It currently
#' raises an error.
#'
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
    #'   optimized dose, in the quantity's display unit as returned by
    #'   `ospsuite::calculatePKAnalyses()`.
    #' @param rdMappings List of `RDOutputMapping` objects used in the run.
    #' @param piParameters `PIParameters` object representing the dose
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
        estimatedDose = optimResult$par[[1]],
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
        initialDose = optimResult$startValues[[1]],
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
    #' - `achievedValue`: PK metric value reached at the optimized dose
    #' - `estimatedDose`: optimized external dose
    #' - `doseUnit`: unit of the estimated dose
    toDataFrame = function() {
      x <- private$.result
      n <- length(x$mappings)

      data.frame(
        quantityPath = sapply(x$mappings, `[[`, "quantityPath"),
        pkParameter = sapply(x$mappings, `[[`, "pkParameter"),
        targetValue = sapply(x$mappings, `[[`, "targetValue"),
        targetUnit = sapply(x$mappings, `[[`, "targetUnit"),
        achievedValue = unlist(x$achievedPKValues),
        estimatedDose = rep(x$estimatedDose, n),
        doseUnit = rep(x$unit, n),
        stringsAsFactors = FALSE
      )
    },

    #' @description Returns the full internal result as a named list.
    #' @return A named list.
    toList = function() {
      private$.result
    },

    #' @description Estimate confidence intervals for the dose estimate.
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
          "Estimated dose" = paste(
            .formatValues(x$estimatedDose),
            x$unit
          ),
          "Initial dose" = paste(
            .formatValues(x$initialDose),
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
