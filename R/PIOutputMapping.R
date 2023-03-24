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
    #' @field observedDataSets Named list of `DataSet` objects that will be compared
    #' to simulation results.
    observedDataSets = function(value) {
      if (missing(value)) {
        as.list(private$.observedDataSets)
      } else {
        stop(messages$errorPropertyReadOnly("observedDataSets"))
      }
    },

    #' @field dataTransformations a named list of factors and offsets
    dataTransformations = function(value) {
      if (missing(value)) {
        private$.dataTransformations
      } else {
        stop(messages$errorPropertyReadOnly("dataTransformations"))
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

    #' @field scaling Linear (default) or logarithmic scaling for this output mapping
    scaling = function(value) {
      if (missing(value)) {
        private$.scaling
      } else {
        private$.scaling <- value
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
    .observedDataSets = NULL,
    .transformResultsFunction = NULL,
    .dataTransformations = NULL,
    .scaling = NULL
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param quantity An object of the type `Quantity`
    #' @return A new `PIOutputMapping` object.
    initialize = function(quantity) {
      validateIsOfType(quantity, "Quantity")
      private$.quantity <- quantity
      private$.observedDataSets <- list()
      private$.dataTransformations <- list(xOffsets = 0, yOffsets = 0, xFactors = 1, yFactors = 1)
      private$.scaling <- "lin"
    },

    #' Add observed data as `DataSet` objects
    #' @details If an observed data object with the same label already exists,
    #' it will be overwritten.
    #' @param data Object or a list of objects of the type
    #' `DataSet`. Each data set must be of the same dimension as the simulation
    #' quantity of the mapping.
    #' @export
    addObservedDataSets = function(data) {
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
        private$.observedDataSets[[data[[idx]]$name]] <- data[[idx]]
      }
    },

    #' @param label label of the x-y values series to be removed
    #' @description
    #' Remove the observed data.
    removeObservedDataSet = function(label) {
      private$.observedDataSets[[label]] <- NULL
      invisible(self)
    },

    #' @description Set the data transformations
    #' @param labels A list of labels, each corresponding to one of the datasets. If labels are not specified, data transformations are set across all datasets
    #' @param xOffsets A numeric list or a value of X-offsets
    #' @param yOffsets A numeric list or a value of Y-offsets
    #' @param xFactors A numeric list or a value of X-factors
    #' @param yFactors A numeric list or a value of Y-factors
    setDataTransformations = function(labels = NULL,
                                      xOffsets = 0,
                                      yOffsets = 0,
                                      xFactors = 1,
                                      yFactors = 1) {
      validateIsString(labels, nullAllowed = TRUE)
      validateIsNumeric(xOffsets, nullAllowed = TRUE)
      validateIsNumeric(xFactors, nullAllowed = TRUE)
      validateIsNumeric(yFactors, nullAllowed = TRUE)
      validateIsNumeric(yOffsets, nullAllowed = TRUE)

      if (missing(labels)) {
        # if no labels are given to the function, the same parameters will be used across datasets
        private$.dataTransformations$xFactors <- xFactors
        private$.dataTransformations$yFactors <- yFactors
        private$.dataTransformations$xOffsets <- xOffsets
        private$.dataTransformations$yOffsets <- yOffsets
        return(invisible(self))
      }

      # otherwise, we only assign data transformations to specific labels
      for (idx in seq_along(labels)) {
        if (length(xFactors) == 1) {
          xFactors <- rep(xFactors, length(labels))
        }
        if (length(xOffsets) == 1) {
          xOffsets <- rep(xOffsets, length(labels))
        }
        if (length(yFactors) == 1) {
          yFactors <- rep(yFactors, length(labels))
        }
        if (length(yOffsets) == 1) {
          yOffsets <- rep(yOffsets, length(labels))
        }
        private$.dataTransformations$xFactors[[labels[[idx]]]] <- xFactors[[idx]]
        private$.dataTransformations$yFactors[[labels[[idx]]]] <- yFactors[[idx]]
        private$.dataTransformations$xOffsets[[labels[[idx]]]] <- xOffsets[[idx]]
        private$.dataTransformations$yOffsets[[labels[[idx]]]] <- yOffsets[[idx]]
      }
      invisible(self)
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Output path", private$.quantity$path)
      private$printLine("Observed data labels", names(private$.observedDataSets))
      private$printLine("Scaling", private$.scaling)
      invisible(self)
    }
  )
)
