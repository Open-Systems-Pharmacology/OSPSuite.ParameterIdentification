#' @title PIConfiguration
#' @docType class
#' @description An object storing configuration for the parameter identification
#' @import ospsuite.utils
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
    #' @field printEvaluationFeedback Boolean. If `TRUE`, the objective function value
    #' will be printed after each function evaluation.
    #' Default is `FALSE`
    printEvaluationFeedback = function(value) {
      if (missing(value)) {
        private$.printEvaluationFeedback
      } else {
        validateIsLogical(value)
        private$.printEvaluationFeedback <- value
      }
    },

    #' @field simulationRunOptions Object of type `SimulationRunOptions` that will be passed
    #' to simulation runs. If `NULL`, default options are used.
    simulationRunOptions = function(value) {
      if (missing(value)) {
        private$.simulationRunOptions
      } else {
        validateIsOfType(value, "SimulationRunOptions", nullAllowed = TRUE)
        private$.simulationRunOptions <- value
      }
    },

    #' @field targetFunctionType Type of the target function used for error
    #' calculation. Supported target functions  are listed in
    #' `ospsuite.parameteridentification::ObjectiveFunctions`. Default is `lsq`.
    targetFunctionType = function(value) {
      if (missing(value)) {
        private$.targetFunctionType
      } else {
        validateIsCharacter(value)
        validateEnumValue(tolower(value), ObjectiveFunctions)
        private$.targetFunctionType <- value
      }
    },

    #' @field algorithm a string describing the optimization algorithm.
    #' Supported algorithms are listed  in
    #' `ospsuite.parameteridentification::Algorithms`.
    algorithm = function(value) {
      if (missing(value)) {
        private$.algorithm
      } else {
        validateIsCharacter(value)
        validateEnumValue(value, Algorithms)
        private$.algorithm <- value
      }
    },

    #' @field algorithmOptions a list of named parameters describing algorithm-specific
    #' options. Supported options are listed  in `ospsuite.parameteridentification::AlgorithmOptions`.
    algorithmOptions = function(value) {
      if (missing(value)) {
        private$.algorithmOptions
      } else {
        for (name in names(value)) {
          validateEnumValue(name, AlgorithmOptions)
        }
        private$.algorithmOptions <- value
      }
    }
  ),
  private = list(
    .simulateSteadyState = NULL,
    .steadyStateTime = NULL,
    .printEvaluationFeedback = NULL,
    .simulationRunOptions = NULL,
    .targetFunctionType = NULL,
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
      private$.targetFunctionType <- "lsq"
      private$.algorithm <- "BOBYQA"
      private$.algorithmOptions <- list()
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      #     private$printLine("Simulate to steady-state", private$.simulateSteadyState)
      #      private$printLine("Steady-state time [min]", private$.steadyStateTime)
      private$printLine("Print feedback after each function evaluation", private$.printEvaluationFeedback)
      private$printLine("Target function", private$.targetFunctionType)
      private$printLine("Optimization algorithm", private$.algorithm)
      invisible(self)
    }
  )
)
