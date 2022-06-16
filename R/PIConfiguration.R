#' @title PIConfiguration
#' @docType class
#' @description An object storing configuration for the parameter identification
#' @import R6 ospsuite.utils
#' @export
#' @format NULL
PIConfiguration <- R6::R6Class(
  "PIConfiguration",
  inherit = ospsuite.utils::Printable,
  cloneable = TRUE,
  active = list(
    #' @field simulateSteadyState Boolean representing whether the simulation
    #' should be brought to a steady-state first
    simulateSteadyState = function(value) {
      if (missing(value)) {
        private$.simulateSteadyState
      } else {
        validateIsLogical(value)
        private$.simulateSteadyState <- value
      }
    },
    #' @field steadyStateTime Time in minutes to simulate if simulating steady-state. May be NULL
    steadyStateTime = function(value) {
      if (missing(value)) {
        private$.steadyStateTime
      } else {
        validateIsNumeric(value)
        if (value < 0) {
          stop(paste0("steadyStateTime must be a positive numerical value, but the value is ", value))
        }
        private$.steadyStateTime <- value
      }
    },
    #' @field printIterationFeedback Boolean. If `TRUE`, the output of the
    #' residuals calculation will be printed after each iteration.
    #' Default is `FALSE`
    printIterationFeedback = function(value) {
      if (missing(value)) {
        private$.printIterationFeedback
      } else {
        validateIsLogical(value)
        private$.printIterationFeedback <- value
      }
    },

    #' @field simulationRunOptions Object of type `SimulationRunOptions` that will be passed
    #' to simulation runs. If `NULL`, default options are used
    simulationRunOptions = function(value) {
      if (missing(value)) {
        private$.simulationRunOptions
      } else {
        validateIsOfType(value, SimulationRunOptions, nullAllowed = TRUE)
        private$.simulationRunOptions <- value
      }
    }
  ),
  private = list(
    .simulateSteadyState = NULL,
    .steadyStateTime = NULL,
    .printIterationFeedback = NULL,
    .simulationRunOptions = NULL
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @return A new `PIConfiguration` object.
    initialize = function() {
      private$.simulateSteadyState <- FALSE
      private$.steadyStateTime <- 1000
      private$.printIterationFeedback <- FALSE
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Simulate to steady-state", private$.simulateSteadyState)
      private$printLine("Steady-state time [min]", private$.steadyStateTime)
      private$printLine("Print feedback after each iteration", private$.printIterationFeedback)
      invisible(self)
    }
  )
)
