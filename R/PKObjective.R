#' @title PKObjective
#' @docType class
#' @description The cost of a parameter vector for the PK targets it was built
#'   with, simulated through a `SimulationModel`: the sum over the PK mappings
#'   of the squared deviation of the PK value from its target, relative to the
#'   target. `evaluate()` keeps no state between calls, like
#'   `PIObjective$evaluate()`. A failed simulation or a PK value that is not
#'   finite is logged with the model and gives the error cost structure.
#'   Confidence intervals and plots are not available for PK targets.
#'
#' @keywords internal
#' @noRd
PKObjective <- R6::R6Class(
  "PKObjective",
  cloneable = FALSE,
  private = list(
    # The `SimulationModel` that simulates the PK mappings
    .model = NULL,
    # List of `PKOutputMapping` objects
    .pkMappings = NULL,
    # IDs of the simulations of the PK mappings. The model may have other
    # simulations, whose failure does not fail the evaluation.
    .simIds = NULL
  ),
  active = list(
    pkMappings = function(value) {
      if (missing(value)) {
        private$.pkMappings
      } else {
        stop(messages$errorPropertyReadOnly("pkMappings"))
      }
    },
    supportsCI = function(value) {
      if (missing(value)) {
        FALSE
      } else {
        stop(messages$errorPropertyReadOnly("supportsCI"))
      }
    },
    supportsPlots = function(value) {
      if (missing(value)) {
        FALSE
      } else {
        stop(messages$errorPropertyReadOnly("supportsPlots"))
      }
    }
  ),
  public = list(
    # @param model A `SimulationModel` object.
    # @param pkMappings List of `PKOutputMapping` objects.
    initialize = function(model, pkMappings) {
      ospsuite.utils::validateIsOfType(model, "SimulationModel")
      ospsuite.utils::validateIsOfType(pkMappings, "PKOutputMapping")
      private$.model <- model
      private$.pkMappings <- pkMappings
      private$.simIds <- unique(vapply(
        pkMappings,
        function(mapping) mapping$simId,
        character(1)
      ))
    },

    # Evaluates the cost of the parameter values.
    # @param values Vector of parameter values.
    # @return A `modelCost` object, the error cost structure when the
    #   simulations fail or a PK value is not finite.
    evaluate = function(values) {
      pkValues <- tryCatch(
        self$pkValues(values),
        simulationsFailedError = function(cond) {
          private$.model$logFailure(values, cond)
          NULL
        }
      )
      if (!is.null(pkValues)) {
        notFinite <- !vapply(pkValues, is.finite, logical(1))
        if (any(notFinite)) {
          mapping <- private$.pkMappings[[which(notFinite)[[1]]]]
          private$.model$logFailure(
            values,
            simpleError(messages$pkValueNotFinite(
              mapping$pkParameter,
              mapping$quantity$path
            ))
          )
          pkValues <- NULL
        }
      }
      if (is.null(pkValues)) {
        message(messages$simulationError())
        return(.createErrorCostStructure())
      }
      targets <- vapply(
        private$.pkMappings,
        function(mapping) mapping$targetValueInBaseUnit,
        numeric(1)
      )
      cost <- sum(((unlist(pkValues) - targets) / targets)^2)
      .pkModelCost(cost, length(private$.pkMappings))
    },

    # Simulates the parameter values and reads the PK value of every mapping.
    # @param values Vector of parameter values.
    # @return A list with one numeric value per PK mapping, in base units.
    pkValues = function(values) {
      # The objective reports a failed simulation itself on every evaluation,
      # so the warning of the simulation engine is not repeated
      simulationResults <- private$.model$simulate(
        values,
        usedSimulations = private$.simIds,
        silentMode = TRUE
      )
      # `calculatePKAnalyses()` analyses the outputs that are selected in the
      # simulation when it runs
      simulations <- private$.model$simulations
      savedState <- .storeSimulationState(simulations)
      on.exit(.restoreSimulationState(simulations, savedState), add = TRUE)
      .selectPKOutputs(simulations, private$.pkMappings)

      lapply(private$.pkMappings, function(mapping) {
        simResult <- simulationResults[[mapping$simId]][[1]]
        pkAnalysis <- ospsuite::calculatePKAnalyses(simResult)
        pkParam <- tryCatch(
          pkAnalysis$pKParameterFor(
            quantityPath = mapping$quantity$path,
            pkParameter = mapping$pkParameter
          ),
          error = function(e) {
            stop(messages$errorPKParameterNotAvailable(
              mapping$pkParameter,
              mapping$quantity$path,
              e$message
            ))
          }
        )
        vals <- pkParam$values
        if (length(vals) == 0L) {
          stop(messages$errorPKParameterNotAvailable(
            mapping$pkParameter,
            mapping$quantity$path
          ))
        }
        if (length(vals) > 1L) {
          stop(messages$errorPKMultiIndividualSimulation(
            mapping$pkParameter,
            mapping$quantity$path,
            length(vals)
          ))
        }
        vals
      })
    },

    # The objective has no observed data to replace.
    # @param data Ignored.
    # @return This objective.
    withData = function(data) {
      self
    }
  )
)

#' The `modelCost` of PK targets
#'
#' @param cost The relative squared deviation sum.
#' @param nObservations The number of PK mappings.
#'
#' @return A `modelCost` object with the cost in `modelCost` and
#'   `minLogProbability` (the optimizer reads either field) and no
#'   `residualDetails` rows.
#' @keywords internal
#' @noRd
.pkModelCost <- function(cost, nObservations) {
  modelCost <- .newModelCost(
    modelCost = cost,
    minLogProbability = cost,
    nObservations = nObservations,
    weightedSSR = cost
  )
  modelCost$residualDetails <- modelCost$residualDetails[0, ]
  modelCost
}
