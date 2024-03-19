#' @title PIConfiguration
#' @docType class
#' @description Encapsulates configurations such as optimization algorithm choice,
#' and evaluation settings for parameter identification.
#' @import R6 ospsuite.utils
#' @export
#' @format NULL
PIConfiguration <- R6::R6Class(
  "PIConfiguration",
  inherit = ospsuite.utils::Printable,
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

    #' @field objectiveFunctionOptions Configures model fit evaluation settings,
    #' influencing error and fit metrics. For option details and impact on cost metrics,
    #' see [`ospsuite.parameteridentification::ObjectiveFunctionSpecs`] and
    #' [`ospsuite.parameteridentification::calculateCostMetrics`]. Defaults found in
    #' [`ospsuite.parameteridentification::ObjectiveFunctionOptions`].
    objectiveFunctionOptions = function(value) {
      if (missing(value)) {
        private$.objectiveFunctionOptions
      } else {
        private$.objectiveFunctionOptions <- value
      }
    },

    #' @field algorithm A string specifying the optimization algorithm to use. See
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

    #' @field algorithmOptions A list of named parameters for algorithm-specific
    #' settings. Refer to [`ospsuite.parameteridentification::AlgorithmOptions`]
    #' for default settings per algorithm (e.g., `AlgorithmOptions_XYZ` where `XYZ`
    #' denotes the algorithm name). If `NULL`, the algorithm's default settings
    #' are applied.
    algorithmOptions = function(value) {
      if (missing(value)) {
        private$.algorithmOptions
      } else {
        private$.algorithmOptions <- value
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
    .algorithmOptions = NULL
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
    },

    #' @description Prints a summary of the PIConfiguration.
    print = function() {
      private$printClass()
      #     private$printLine("Simulate to steady-state", private$.simulateSteadyState)
      #      private$printLine("Steady-state time [min]", private$.steadyStateTime)
      private$printLine(
        "Print feedback after each function evaluation",
        private$.printEvaluationFeedback
      )
      private$printLine(
        "Objective function type",
        private$.objectiveFunctionOptions$objectiveFunctionType
      )
      private$printLine(
        "Residual weighting method",
        private$.objectiveFunctionOptions$residualWeightingMethod
      )
      private$printLine(
        "Robust residual calculation method",
        private$.objectiveFunctionOptions$robustMethod
      )
      private$printLine("Optimization algorithm", private$.algorithm)
      invisible(self)
    }
  )
)
