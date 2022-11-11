#' @title PIOutputMapping
#' @docType class
#' @description An object that links together quantities from the simulation
#' to observed data. This object is passed to the ParameterIdentification
#' objects
#' @import ospsuite.utils R6
#' @export
#' @format NULL
PIOutputMapping <- R6::R6Class(
  "PIOutputMapping",
  inherit = ospsuite.utils::Printable,
  cloneable = TRUE,
  active = list(
    #' @field observedData Named list of `DataSet` objects that will be compared
    #' to simulation results.
    observedData = function(value) {
      if (missing(value)) {
        as.list(private$.observedData)
      } else {
        stop(messages$errorPropertyReadOnly("observedData"))
      }
    },

    #' @field xFactors Named list of numeric values
    xFactors = function(value) {
      if (missing(value)) {
        private$.xFactors
      } else {
        stop(messages$errorPropertyReadOnly("xFactors"))
      }
    },

    #' @field yFactors Named list of numeric values
    yFactors = function(value) {
      if (missing(value)) {
        private$.yFactors
      } else {
        stop(messages$errorPropertyReadOnly("yFactors"))
      }
    },

    #' @field xOffsets Named list of numeric values
    xOffsets = function(value) {
      if (missing(value)) {
        private$.xOffsets
      } else {
        stop(messages$errorPropertyReadOnly("xOffsets"))
      }
    },

    #' @field yOffsets Named list of numeric values
    yOffsets = function(value) {
      if (missing(value)) {
        private$.yOffsets
      } else {
        stop(messages$errorPropertyReadOnly("yOffsets"))
      }
    },

    #' @field quantity Simulation quantities which values are matched to the
    #' observed data
    quantity = function(value) {
      if (missing(value)) {
        private$.quantity
      } else {
        stop(messages$errorPropertyReadOnly("quantity"))
      }
    },

    #' @field transformResultsFunction Function that will be applied to
    #'   simulated results. Allows to manipulate simulated values before
    #'   calculating the residuals. The function should manipulate numeric
    #'   vectors 'xVals' and 'yVals' (that being the simulated time- and
    #'   observation values) that will be than assigned to the x- and y-values
    #'   of the simulated result. The function must return a named list with key
    #'   'xVals' and 'yVals'.
    transformResultsFunction = function(value) {
      if (missing(value)) {
        private$.transformResultsFunction
      } else {
        if (!is.function(value)) {
          stop(messages$errorNotAFunction())
        }
        private$.transformResultsFunction <- value
      }
    }
  ),
  private = list(
    .quantity = NULL,
    .observedData = NULL,
    .transformResultsFunction = NULL,
    .xFactors = NULL,
    .yFactors = NULL,
    .xOffsets = NULL,
    .yOffsets = NULL
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param quantity An object of the type `Quantity`
    #' @return A new `PIOutputMapping` object.
    initialize = function(quantity) {
      validateIsOfType(quantity, "Quantity")
      private$.quantity <- quantity
      private$.observedData <- list()
      private$.xFactors <- list()
      private$.yFactors <- list()
      private$.xOffsets <- list()
      private$.yOffsets <- list()
    },

    #' Add observed data as `DataSet` objects
    #' @details If an observed data object with the same label already exists,
    #' it will be overwritten.
    #' @param data Object or a list of objects of the type
    #' `DataSet`. Each data set must be of the same dimension as the simulation
    #' quantity of the mapping.
    #' @export
    addObservedData = function(data) {
      validateIsOfType(data, "DataSet")
      data <- toList(data)
      for (idx in seq_along(data)) {
        # Test if the dimension of the data to be added can be converted to the
        # dimension of the quantity of this Output Mapping.
        invisible(ospsuite::toBaseUnit(
          quantityOrDimension = private$.quantity,
          values = 1,
          unit = data[[idx]]$yUnit,
          molWeight = data[[idx]]$molWeight
        ))
        private$.observedData[[data[[idx]]$name]] <- data[[idx]]
      }
    },

    #' @param label label of the x-y values series to be removed
    #' @description
    #' Remove the observed data.
    removeObservedData = function(label) {
      private$.observedData[[label]] <- NULL
      invisible(self)
    },

    #' @description Set the X-factors
    #' @param labels A list of labels
    #' @param factors
    setXFactors = function(labels, factors) {
      validateIsString(labels, nullAllowed = TRUE)
      validateIsNumeric(factors, nullAllowed = TRUE)
      validateIsSameLength(labels, factors)

      for (idx in seq_along(labels)) {
        private$.xFactors[[labels[[idx]]]] <- factors[[idx]]
      }
      invisible(self)
    },

    #' @description Set the Y-factors
    #' @param labels A list of labels
    #' @param factors
    setYFactors = function(labels, factors) {
      validateIsString(labels, nullAllowed = TRUE)
      validateIsNumeric(factors, nullAllowed = TRUE)
      validateIsSameLength(labels, factors)

      for (idx in seq_along(labels)) {
        private$.yFactors[[labels[[idx]]]] <- factors[[idx]]
      }
      invisible(self)
    },

    #' @description Set the X-offsets
    #' @param labels A list of labels
    #' @param offsets
    setXOffsets = function(labels, offsets) {
      validateIsString(labels, nullAllowed = TRUE)
      validateIsNumeric(offsets, nullAllowed = TRUE)
      validateIsSameLength(labels, offsets)

      for (idx in seq_along(labels)) {
        private$.xOffsets[[labels[[idx]]]] <- offsets[[idx]]
      }
      invisible(self)
    },

    #' @description Set the Y-offsets
    #' @param labels A list of labels
    #' @param offsets
    setYOffsets = function(labels, offsets) {
      validateIsString(labels, nullAllowed = TRUE)
      validateIsNumeric(offsets, nullAllowed = TRUE)
      validateIsSameLength(labels, offsets)

      for (idx in seq_along(labels)) {
        private$.yOffsets[[labels[[idx]]]] <- offsets[[idx]]
      }
      invisible(self)
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Output path", private$.quantity$path)
      private$printLine("Observed data labels", names(private$.observedData))
      invisible(self)
    }
  )
)
