#' @title PIConfiguration
#' @docType class
#' @description An object storing configuration for parameter identification
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
    #' function value after each evaluation. Default is `FALSE`
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

    #' @field objectiveFunctionOptions Configures model fit assessment
    #' options. This setting influences how error and model fit are calculated
    #' Supported options and their allowable values are detailed in
    #' `ospsuite.parameteridentification::ObjectiveFunctionOptions`.
    objectiveFunctionOptions = function(inputOptions = list()) {
      if (missing(inputOptions)) {
        private$.objectiveFunctionOptions
      } else {
        validateIsOption(inputOptions, ObjectiveFunctionOptions)
        private$.objectiveFunctionOptions <- modifyList(
          private$.objectiveFunctionOptions, inputOptions
        )
      }
    },

    #' @field algorithm a string describing the optimization algorithm.
    #' Supported algorithms are listed  in
    #' `ospsuite.parameteridentification::Algorithms`.
    algorithm = function(value) {
      if (missing(value)) {
        private$.algorithm
      } else {
        ospsuite.utils::validateIsCharacter(value)
        ospsuite.utils::validateEnumValue(value, Algorithms)
        private$.algorithm <- value
      }
    },

    #' @field algorithmOptions a list of named parameters describing algorithm-specific
    #' options. Default options are listed  in `AlgorithmOptions_XYZ` where `XYZ` is the name of the algorithm.
    #' If `NULL`, default options are used.
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
    #' @description
    #' Initialize a new instance of the class
    #' @return A new `PIConfiguration` object.
    initialize = function() {
      private$.simulateSteadyState <- FALSE
      private$.steadyStateTime <- 1000
      private$.printEvaluationFeedback <- FALSE
      private$.objectiveFunctionOptions <- list(
        objectiveFunctionType = "lsq",
        residualWeightingMethod = "none",
        robustMethod = "none",
        scaleVar = FALSE,
        scaling = "lin",
        linScaleCV = 0.2,
        logScaleSD = NULL
      )
      private$.algorithm <- "BOBYQA"
    },

    #' Print
    #' @description prints a summary of the PIConfiguration.
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
