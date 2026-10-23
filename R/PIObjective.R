#' @title PIObjective
#' @docType class
#' @description The cost of a parameter vector for the output mappings and
#'   the observed data it was built with. `evaluate()` is a pure function of
#'   the parameter values: it does not count, track or print anything. The
#'   `ParameterIdentification` facade builds one objective per public call
#'   and per bootstrap sample and wraps it in its evaluation closure.
#'
#' @keywords internal
#' @noRd
PIObjective <- R6::R6Class(
  "PIObjective",
  cloneable = FALSE,
  private = list(
    # List of `PIOutputMapping` objects
    .outputMappings = NULL,
    # Observed data of each output mapping in base units and with the data
    # transformations applied (see `.prepareObservedData()`), read once when
    # the objective is built
    .observedData = NULL,
    # The objective function options of each output mapping, with its scaling
    .costControls = NULL,
    # `function(values)` that returns the simulated values of every output
    # mapping (see `ParameterIdentification$.simulateOutputs()`) or signals a
    # `simulationsFailedError`
    .simulate = NULL
  ),
  active = list(
    # The prepared observed data of each output mapping (read-only)
    observedData = function(value) {
      if (missing(value)) {
        private$.observedData
      } else {
        stop(messages$errorPropertyReadOnly("observedData"))
      }
    }
  ),
  public = list(
    # @param outputMappings List of `PIOutputMapping` objects.
    # @param objectiveFunctionOptions The complete list of
    #   `PIConfiguration$objectiveFunctionOptions`.
    # @param simulate `function(values)` returning the simulated values of
    #   every output mapping, in the order of `outputMappings`.
    initialize = function(outputMappings, objectiveFunctionOptions, simulate) {
      ospsuite.utils::validateIsOfType(outputMappings, "PIOutputMapping")
      ospsuite.utils::validateIsOfType(simulate, "function")
      private$.outputMappings <- outputMappings
      private$.simulate <- simulate
      private$.observedData <- lapply(outputMappings, .prepareObservedData)
      private$.costControls <- lapply(outputMappings, function(mapping) {
        costControl <- objectiveFunctionOptions
        costControl$scaling <- mapping$scaling
        costControl
      })
      # The options of the output mappings differ only in their scaling
      scalings <- vapply(outputMappings, function(mapping) mapping$scaling, "")
      for (costControl in private$.costControls[!duplicated(scalings)]) {
        ospsuite.utils::validateIsOption(
          options = costControl,
          validOptions = ObjectiveFunctionSpecs
        )
      }
    },

    # Evaluates the cost of the parameter values.
    # @param values Vector of parameter values.
    # @return A `modelCost` object, the error cost structure when the
    #   simulations fail.
    evaluate = function(values) {
      simulated <- tryCatch(
        private$.simulate(values),
        simulationsFailedError = function(cond) NULL
      )
      if (is.null(simulated)) {
        return(.createErrorCostStructure())
      }
      costTerms <- vector("list", length(private$.outputMappings))
      for (idx in seq_along(private$.outputMappings)) {
        mapping <- private$.outputMappings[[idx]]
        costTerms[[idx]] <- .mappingCostTerms(
          simulated = simulated[[idx]],
          observed = private$.observedData[[idx]],
          dataWeights = mapping$dataWeights,
          costControl = private$.costControls[[idx]],
          index = idx,
          quantityPath = mapping$quantity$path
        )
      }
      .combineCostTerms(costTerms)
    }
  )
)
