#' @title PIObjective
#' @docType class
#' @description The cost of a parameter vector for the observed data it was
#'   built with. `evaluate()` keeps no state between calls. Counting the
#'   evaluations and tracking the best cost are left to the
#'   `ParameterIdentification` facade, which builds one objective per public
#'   call and wraps it in its evaluation closure, and `withData()` gives the
#'   objective of a bootstrap replicate on the resampled data. The `simulate`
#'   function of the facade logs and counts failed simulations, and the cost
#'   calculation warns about an invalid cost.
#'
#' @keywords internal
#' @noRd
PIObjective <- R6::R6Class(
  "PIObjective",
  cloneable = FALSE,
  private = list(
    # The `PIObservations` object with the output mappings and their prepared
    # observed data
    .data = NULL,
    # The complete list of `PIConfiguration$objectiveFunctionOptions`
    .objectiveFunctionOptions = NULL,
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
        private$.data$observedData
      } else {
        stop(messages$errorPropertyReadOnly("observedData"))
      }
    }
  ),
  public = list(
    # @param data A `PIObservations` object.
    # @param objectiveFunctionOptions The complete list of
    #   `PIConfiguration$objectiveFunctionOptions`.
    # @param simulate `function(values)` returning the simulated values of
    #   every output mapping, in the order of `data$outputMappings`.
    initialize = function(data, objectiveFunctionOptions, simulate) {
      private$.data <- data
      private$.objectiveFunctionOptions <- objectiveFunctionOptions
      private$.simulate <- simulate
      outputMappings <- data$outputMappings
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
      outputMappings <- private$.data$outputMappings
      observedData <- private$.data$observedData
      costTerms <- vector("list", length(outputMappings))
      for (idx in seq_along(outputMappings)) {
        mapping <- outputMappings[[idx]]
        costTerms[[idx]] <- .mappingCostTerms(
          simulated = simulated[[idx]],
          observed = observedData[[idx]],
          dataWeights = mapping$dataWeights,
          costControl = private$.costControls[[idx]],
          index = idx,
          quantityPath = mapping$quantity$path
        )
      }
      .combineCostTerms(costTerms)
    },

    # The objective on other data, with the same options and simulations.
    # @param data A `PIObservations` object.
    # @return A new `PIObjective` object.
    withData = function(data) {
      PIObjective$new(
        data = data,
        objectiveFunctionOptions = private$.objectiveFunctionOptions,
        simulate = private$.simulate
      )
    }
  )
)
