#' @title PIConfiguration
#' @docType class
#' @description Encapsulates configurations such as optimization algorithm choice,
#' and evaluation settings for parameter identification.
#' @import R6 ospsuite.utils
#' @export
#' @format NULL
PIConfiguration <- R6::R6Class(
  "PIConfiguration",
  cloneable = TRUE,
  active = list(
    # #' @field simulateSteadyState Boolean representing whether the simulation
    # #' should be brought to a steady-state first
    # simulateSteadyState = function(value) {
    #   if (missing(value)) {
    #     private$.simulateSteadyState
    #   } else {
    #     validateIsLogical(value)
    #     private$.simulateSteadyState <- value
    #   }
    # },
    # #' @field steadyStateTime Time in minutes to simulate if simulating steady-state. May be NULL
    # steadyStateTime = function(value) {
    #   if (missing(value)) {
    #     private$.steadyStateTime
    #   } else {
    #     validateIsNumeric(value)
    #     if (value < 0) {
    #       stop(paste0("steadyStateTime must be a positive numerical value, but the value is ", value))
    #     }
    #     private$.steadyStateTime <- value
    #   }
    # },
    #' @field printEvaluationFeedback Boolean. If `TRUE`, prints objective
    #' function value after each evaluation. Default is `FALSE`.
    printEvaluationFeedback = function(value) {
      if (missing(value)) {
        private$.printEvaluationFeedback
      } else {
        ospsuite.utils::validateIsLogical(value)
        private$.printEvaluationFeedback <- value
      }
    },

    #' @field simulationRunOptions Object of `SimulationRunOptions` for simulation
    #' runs. If `NULL`, default options are used.
    simulationRunOptions = function(value) {
      if (missing(value)) {
        private$.simulationRunOptions
      } else {
        ospsuite.utils::validateIsOfType(
          value, "SimulationRunOptions",
          nullAllowed = TRUE
        )
        private$.simulationRunOptions <- value
      }
    },

    #' @field objectiveFunctionOptions Settings for model fit evaluation,
    #' affecting error metrics and cost calculation. See [`ObjectiveFunctionSpecs`]
    #' and [`calculateCostMetrics`] for details. Defaults in [`ObjectiveFunctionOptions`].
    objectiveFunctionOptions = function(value) {
      if (missing(value)) {
        private$.objectiveFunctionOptions
      } else {
        private$.objectiveFunctionOptions <- value
      }
    },

    #' @field algorithm Optimization algorithm name. See
    #' [`ospsuite.parameteridentification::Algorithms`] for a list of supported
    #' algorithms. Defaults to `BOBYQA`.
    algorithm = function(value) {
      if (missing(value)) {
        private$.algorithm
      } else {
        ospsuite.utils::validateIsCharacter(value)
        ospsuite.utils::validateEnumValue(value, Algorithms)
        private$.algorithm <- value
      }
    },

    #' @field ciMethod Confidence interval estimation method. See
    #' [`ospsuite.parameteridentification::CIMethods`] for available options.
    #' Defaults to `hessian`.
    ciMethod = function(value) {
      if (missing(value)) {
        private$.ciMethod
      } else {
        ospsuite.utils::validateIsCharacter(value)
        ospsuite.utils::validateEnumValue(value, CIMethods)
        private$.ciMethod <- value
      }
    },

    #' @field algorithmOptions Named list of settings specific to the selected
    #' algorithm.. Refer to [`ospsuite.parameteridentification::AlgorithmOptions`]
    #' for default settings per algorithm (e.g., `AlgorithmOptions_XYZ` where `XYZ`
    #' denotes the algorithm name). If `NULL`, algorithm's default settings are
    #' applied.
    algorithmOptions = function(value) {
      if (missing(value)) {
        private$.algorithmOptions
      } else {
        private$.algorithmOptions <- value
      }
    },

    #' @field ciOptions Named list of settings for the selected CI method.
    #' Refer to [`ospsuite.parameteridentification::CIOptions`] for default
    #' settings per method (e.g., `CIOptions_XYZ` where `XYZ` corresponds to the
    #' method name). If `NULL`, CI method's default settings are applied.
    ciOptions = function(value) {
      if (missing(value)) {
        private$.ciOptions
      } else {
        private$.ciOptions <- value
      }
    },

    #' @field estimateCI Logical. If `TRUE`, confidence intervals are estimated
    #' after optimization.
    estimateCI = function(value) {
      if (missing(value)) {
        private$.estimateCI
      } else {
        ospsuite.utils::validateIsLogical(value)
        private$.estimateCI <- value
      }
    },

    #' @field modelCostField Field name in cost object used as the optimization
    #' target. Defaults to `modelCost`.
    modelCostField = function(value) {
      if (missing(value)) {
        private$.modelCostField
      } else {
        ospsuite.utils::validateIsCharacter(value)
        private$.modelCostField <- value
      }
    }
  ),
  private = list(
    .simulateSteadyState = NULL,
    .steadyStateTime = NULL,
    .printEvaluationFeedback = NULL,
    .simulationRunOptions = NULL,
    .objectiveFunctionOptions = NULL,
    .algorithm = NULL,
    .algorithmOptions = NULL,
    .modelCostField = NULL,
    .ciMethod = NULL,
    .ciOptions = NULL,
    .estimateCI = NULL
  ),
  public = list(
    #' @description Initialize a new instance of the class.
    #' @return A new `PIConfiguration` object.
    initialize = function() {
      private$.simulateSteadyState <- FALSE
      private$.steadyStateTime <- 1000
      private$.printEvaluationFeedback <- FALSE
      private$.objectiveFunctionOptions <- ObjectiveFunctionOptions
      private$.algorithm <- "BOBYQA"
      private$.ciMethod <- "hessian"
      private$.modelCostField <- "modelCost"
      private$.estimateCI <- FALSE
    },

    #' @description Prints a summary of the PIConfiguration.
    print = function() {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "Optimization algorithm" = private$.algorithm,
        "Objective function type" = private$.objectiveFunctionOptions$objectiveFunctionType,
        "Residual weighting method" = private$.objectiveFunctionOptions$residualWeightingMethod,
        "Robust residual calculation method" = private$.objectiveFunctionOptions$robustMethod,
        "Print feedback after each function evaluation" = private$.printEvaluationFeedback
      ))
      #     private$printLine("Simulate to steady-state", private$.simulateSteadyState)
      #      private$printLine("Steady-state time [min]", private$.steadyStateTime)
    }
  )
)
