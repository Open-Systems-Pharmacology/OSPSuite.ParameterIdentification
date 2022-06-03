#' @title PIParameters
#' @docType class
#' @description A parameter to be optimized in a parameter identification routine
#' @import R6
#' @export
#' @format NULL
PIParameters <- R6::R6Class(
  "PIParameters",
  inherit = ospsuite.utils::Printable,
  cloneable = TRUE,
  active = list(
    #' @field parameters List of parameter object. Adding or removing parameters is currently not supported.
    parameters = function(value) {
      if (missing(value)) {
        private$.parameters
      } else {
        stop(messages$errorPropertyReadOnly("parameters"))
      }
    },

    #' @field currValue Current value of the parameters as used in the simulation. The unit of the value is \code{PIParameters$unit}.
    currValue = function(value) {
      if (missing(value)) {
        toUnit(
          quantityOrDimension = private$.parameters[[1]],
          values = private$.parameters[[1]]$value,
          targetUnit = private$.unit
        )
      } else {
        stop(messages$errorPropertyReadOnly("currValue", optionalMessage = "Use $setValue() to change the value."))
      }
    },

    #' @field startValue Start value in the optimization.
    startValue = function(value) {
      if (missing(value)) {
        private$.startValue
      } else {
        ospsuite.utils::validateIsNumeric(value)
        private$.startValue <- value
      }
    },

    #' @field minValue Minimal allowed value of the parameter.
    minValue = function(value) {
      if (missing(value)) {
        private$.minValue
      } else {
        ospsuite.utils::validateIsNumeric(value)
        if (value > private$.startValue) {
          stop(paste0("The minimal value cannot be greater than the start value!
          Provided minimal value: ", value, ". Current start value: ", private$.startValue))
        }

        private$.minValue <- value
      }
    },

    #' @field maxValue Maximal allowed value of the parameter.
    maxValue = function(value) {
      if (missing(value)) {
        private$.maxValue
      } else {
        ospsuite.utils::validateIsNumeric(value)
        if (value < private$.startValue) {
          stop(paste0("The maximal value cannot be smaller than the start value!
          Provided maximal value: ", value, ". Current start value: ", private$.startValue))
        }

        private$.maxValue <- value
      }
    },

    #' @field unit Unit of parameter values. If the unit is changed, the min/max/start values are NOT
    #' adjusted automatically.
    unit = function(value) {
      if (missing(value)) {
        private$.unit
      } else {
        validateUnit(unit = value, dimension = private$.parameters[[1]]$dimension)
        private$.unit <- value
      }
    }
  ),
  private = list(
    .parameters = NULL,
    .startValue = NULL,
    .minValue = NULL,
    .maxValue = NULL,
    .unit = NULL
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' The start value is set to the value of the first parameter at time of creation
    #' and can be changed with \code{PIParameters$startValue}
    #' @param parameters A list of objects of class \code{Parameter}.
    #' All parameters will be optimized using the same value.
    #' @return A new `PIParameters` object.
    initialize = function(parameters) {
      parameters <- c(parameters)
      ospsuite.utils::validateIsOfType(parameters, "Parameter")
      validateIsSameDimension(parameters)

      private$.parameters <- parameters
      private$.startValue <- parameters[[1]]$value
      private$.minValue <- private$.startValue * 0.1
      private$.maxValue <- private$.startValue * 10
      private$.unit <- parameters[[1]]$unit
    },

    # addParameter = function(parameter){
    # TODO. Will have to somehow hash the parameters to check if it already has been added
    # }

    #' @description
    #' Change the value of the parameter(s)
    #' The unit of the value is $unit
    #' @param value Numerical value.
    setValue = function(value) {
      ospsuite.utils::validateIsNumeric(value)
      for (param in private$.parameters) {
        param$setValue(value, private$.unit)
      }

      invisible(self)
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Number of parameters", length(self$parameters))
      private$printLine("Value", self$currValue)
      private$printLine("Start value", self$startValue)
      private$printLine("Min value", self$minValue)
      private$printLine("Max value", self$maxValue)
      private$printLine("Unit", self$unit)

      invisible(self)
    }
  )
)
