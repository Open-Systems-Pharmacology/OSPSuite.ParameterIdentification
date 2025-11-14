#' @title PIParameters
#' @docType class
#' @description A parameter to be optimized in a parameter identification
#'   routine
#' @import R6 ospsuite.utils
#' @export
#' @format NULL
PIParameters <- R6::R6Class(
  "PIParameters",
  cloneable = TRUE,
  active = list(
    #' @field parameters A list of parameter objects. Adding or removing
    #'   parameters is not supported.
    parameters = function(value) {
      if (missing(value)) {
        private$.parameters
      } else {
        stop(messages$errorPropertyReadOnly("parameters"))
      }
    },

    #' @field currValue The current parameter values for simulations, in units
    #'   specified by `$unit`.
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

    #' @field startValue Initial value used for optimization.
    startValue = function(value) {
      if (missing(value)) {
        private$.startValue
      } else {
        ospsuite.utils::validateIsNumeric(value)
        private$.startValue <- value
      }
    },

    #' @field minValue Lowest permissible parameter value.
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

    #' @field maxValue Highest permissible parameter value.
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

    #' @field unit Parameter value units. Changing the unit does NOT
    #'   automatically adjust min/max/start values.
    unit = function(value) {
      if (missing(value)) {
        private$.unit
      } else {
        ospsuite::validateUnit(unit = value, dimension = private$.parameters[[1]]$dimension)
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
    #' @description Initialize a new instance of the class. The initial start
    #'   value is derived from the first parameter upon creation, modifiable via
    #'   `PIParameters$startValue`. All parameters are optimized using this
    #'   unified value.
    #' @param parameters List of `Parameter` class objects to be optimized.
    #' @return A new `PIParameters` object.
    initialize = function(parameters) {
      parameters <- c(parameters)
      ospsuite.utils::validateIsOfType(parameters, "Parameter")
      .validateIsSameDimension(parameters)

      private$.parameters <- parameters
      private$.startValue <- parameters[[1]]$value
      if (private$.startValue > 0) {
        private$.minValue <- private$.startValue * 0.1
        private$.maxValue <- private$.startValue * 10
      } else {
        private$.minValue <- private$.startValue * 10
        private$.maxValue <- private$.startValue * 0.1
      }
      private$.unit <- parameters[[1]]$unit
    },

    #' @description Updates parameter(s) value. Value is specified in units of
    #'   `$unit`.
    #' @param value Numeric value to set.
    setValue = function(value) {
      ospsuite.utils::validateIsNumeric(value)
      for (param in private$.parameters) {
        param$setValue(value, private$.unit)
      }

      invisible(self)
    },

    #' @description Export parameter metadata and configuration to a data frame.
    #'   Returns one row per internal model parameter wrapped by the
    #'   `PIParameters` object.
    #' @param group Optional character label identifying the optimization group.
    #' @return A `data.frame` with one row per internal parameter.
    toDataFrame = function(group = NULL) {
      n <- length(private$.parameters)

      parametersDf <- data.frame(
        group = rep(group %||% NA_integer_, n),
        name = sapply(private$.parameters, `[[`, "name"),
        path = sapply(private$.parameters, `[[`, "fullPath"),
        unit = rep(self$unit, n),
        currValue = rep(self$currValue, n),
        startValue = rep(self$startValue, n),
        minValue = rep(self$minValue, n),
        maxValue = rep(self$maxValue, n),
        stringsAsFactors = FALSE
      )

      return(parametersDf)
    },

    #' @description Prints a summary of the PIParameters.
    print = function() {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "Number of parameters" = length(self$parameters),
        "Value" = format(self$currValue, digits = 4),
        "Start value" = format(self$startValue, digits = 4),
        "Min value" = format(self$minValue, digits = 4),
        "Max value" = format(self$maxValue, digits = 4),
        "Unit" = self$unit
      ))
    }
  )
)
