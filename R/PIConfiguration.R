#' @title PIConfiguration
#' @docType class
#' @description An object storing configuration for the parameter identification
#' @export
#' @format NULL
PIConfiguration <- R6::R6Class(
  "PIConfiguration",
  inherit = ospsuite:::Printable,
  cloneable = FALSE,
  active = list(
    #' @field simulateSteadyState Boolean representing whether the simulation will be brought to a steady-state first
    simulateSteadyState = function(value) {
      if (missing(value)) {
        private$.simulateSteadyState
      } else {
        ospsuite:::validateIsLogical(value)
        private$.simulateSteadyState <- value
      }
    },
    #' @field steadyStateTime Time in minutes to simulate if simulating steady-state. May be NULL
    steadyStateTime = function(value) {
      if (missing(value)) {
        private$.steadyStateTime
      } else {
        ospsuite:::validateIsNumeric(value)
        if (value < 0) {
          stop(paste0("steadyStateTime must be a positive numerical value, but the value is ", value))
        }
        private$.steadyStateTime <- value
      }
    },
    #' @field printIterationFeedback Boolean. If TRUE, the output of the residuals calculation will be printed after each iteration.
    #' Default is FALSE
    printIterationFeedback = function(value) {
      if (missing(value)) {
        private$.printIterationFeedback
      } else {
        ospsuite:::validateIsLogical(value)
        private$.printIterationFeedback <- value
      }
    },

    #' @field numberOfCores (Maximal) number of cores to be used. This is only relevant when \code{parallellize}
    #' is \code{TRUE}.
    #' Default is getOSPSuitePISetting("maxNumberOfCores").
    numberOfCores = function(value) {
      if (missing(value)) {
        private$.numberOfCores
      } else {
        ospsuite:::validateIsInteger(value)
        if (value < 1) {
          stop(messages(errorNumberOfCoresNotPositive)())
        }
        private$.numberOfCores <- value
      }
    },

    #' @field parallelize Logical. If \code{TRUE} (default), simulations for each evaluation are executed in parallel.
    parallelize = function(value) {
      if (missing(value)) {
        private$.parallelize
      } else {
        ospsuite:::validateIsLogical(value)
        private$.parallelize <- value
      }
    }
  ),
  private = list(
    .simulateSteadyState = NULL,
    .steadyStateTime = NULL,
    .printIterationFeedback = NULL,
    .parallelize = NULL,
    .numberOfCores = NULL
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @return A new `PIConfiguration` object.
    initialize = function() {
      private$.simulateSteadyState <- FALSE
      private$.steadyStateTime <- 1000
      private$.printIterationFeedback <- FALSE
      private$.parallelize <- FALSE
      private$.numberOfCores <- getOSPSuitePISetting("maxNumberOfCores")
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Simulate to steady-state", private$.simulateSteadyState)
      private$printLine("Steady-state time [min]", private$.steadyStateTime)
      private$printLine("Print feedback after each iteration", private$.printIterationFeedback)
      private$printLine("Execute in parallel", private$.parallelize)
      private$printLine("Maximal number of cores", private$.numberOfCores)
      invisible(self)
    }
  )
)
