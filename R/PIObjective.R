#' @title PIObjective
#' @docType class
#' @description The cost of a parameter vector for the observed data it was
#'   built with, simulated through a `SimulationModel`. `evaluate()` keeps no
#'   state between calls. Counting the evaluations and tracking the best cost
#'   are left to the `ParameterIdentification` facade, which builds one
#'   objective per public call and wraps it in its evaluation closure, and
#'   `withData()` gives the objective of a bootstrap replicate on the
#'   resampled data. A failed simulation is logged with the model and gives
#'   the error cost structure, and the cost calculation warns about an
#'   invalid cost.
#'
#' @keywords internal
#' @noRd
PIObjective <- R6::R6Class(
  "PIObjective",
  cloneable = FALSE,
  private = list(
    # The `SimulationModel` that simulates the output mappings
    .model = NULL,
    # The `PIObservations` object with the output mappings and their prepared
    # observed data
    .data = NULL,
    # The complete list of `PIConfiguration$objectiveFunctionOptions`
    .objectiveFunctionOptions = NULL,
    # `PIConfiguration$blqMethod` and `PIConfiguration$blqOptions`
    .blqMethod = NULL,
    .blqOptions = NULL,
    # The objective function options of each output mapping, with its scaling,
    # `blqMethod` and `cv`
    .costControls = NULL
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
    # @param model A `SimulationModel` object.
    # @param data A `PIObservations` object.
    # @param objectiveFunctionOptions The complete list of
    #   `PIConfiguration$objectiveFunctionOptions`.
    # @param blqMethod `PIConfiguration$blqMethod`.
    # @param blqOptions The complete list of `PIConfiguration$blqOptions`.
    initialize = function(
      model,
      data,
      objectiveFunctionOptions,
      blqMethod,
      blqOptions
    ) {
      ospsuite.utils::validateIsOfType(model, "SimulationModel")
      ospsuite.utils::validateIsOfType(data, "PIObservations")
      ospsuite.utils::validateIsOption(
        objectiveFunctionOptions,
        ObjectiveFunctionSpecs
      )
      ospsuite.utils::validateEnumValue(blqMethod, BLQMethods)
      ospsuite.utils::validateIsOption(blqOptions, BLQOptionSpecs)
      private$.model <- model
      private$.data <- data
      private$.objectiveFunctionOptions <- objectiveFunctionOptions
      private$.blqMethod <- blqMethod
      private$.blqOptions <- blqOptions
      private$.costControls <- lapply(data$outputMappings, function(mapping) {
        costControl <- objectiveFunctionOptions
        costControl$scaling <- mapping$scaling
        costControl$blqMethod <- blqMethod
        costControl$cv <- blqOptions$cv
        costControl
      })
    },

    # Evaluates the cost of the parameter values.
    # @param values Vector of parameter values.
    # @return A `modelCost` object, the error cost structure when the
    #   simulations fail.
    evaluate = function(values) {
      outputMappings <- private$.data$outputMappings
      simulated <- tryCatch(
        private$.model$simulatedOutputs(values, outputMappings),
        simulationsFailedError = function(cond) {
          private$.model$logFailure(values, cond)
          message(messages$simulationError())
          NULL
        }
      )
      if (is.null(simulated)) {
        return(.createErrorCostStructure())
      }
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

    # The objective on other data, with the same options and model.
    # @param data A `PIObservations` object.
    # @return A new `PIObjective` object.
    withData = function(data) {
      PIObjective$new(
        model = private$.model,
        data = data,
        objectiveFunctionOptions = private$.objectiveFunctionOptions,
        blqMethod = private$.blqMethod,
        blqOptions = private$.blqOptions
      )
    }
  )
)
