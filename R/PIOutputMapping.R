#' @title PIOutputMapping
#' @docType class
#' @description An object that combines simulation output with observed data. The parameter identification minimizes the
#' distance between the simulation output and the observed data.
#' @import ospsuite hash R6
#' @export
#' @format NULL
PIOutputMapping <- R6::R6Class(
  "PIOutputMapping",
  inherit = ospsuite.utils::Printable,
  cloneable = TRUE,
  active = list(
    #' @field observedXYData Named list with the \code{XYData} that will be
    #' compared with simulation results. Names are the labels of the \code{xySeries} objects
    observedXYData = function(value) {
      if (missing(value)) {
        as.list(private$.observedXYData)
      } else {
        stop(messages$errorPropertyReadOnly("observedXYData"))
      }
    },

    #' @field quantity Simulation quantity which results are to be compared to observed data. Read-only.
    quantity = function(value) {
      if (missing(value)) {
        private$.quantity
      } else {
        stop(messages$errorPropertyReadOnly("quantity"))
      }
    },

    #' @field transformResultsFunction Function that will be applied to simulated results. Allows to manipulate simulated values before
    #' calculating the residuals. The function should manipulate numeric vectors 'xVals' and 'yVals' (that being the simulated time- and observation values)
    #' that will be than assigned to the x- and y-values of the simulated result.
    #' The function must return a named list with key 'xVals' and 'yVals'.
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

    #' # @field residualsScaling Defines if the residuals are calculated based on the absolute distance (Simulation value - Observed value, scaling = "lin")
    #' or as log(Simulation value) - log(Observed value) (scaling = "log"). Default is "lin".
    # residualsScaling = function(value) {
    #   if (missing(value)) {
    #     private$.residualsScaling
    #   } else {
    #     if (!value %in% c("log", "lin")) {
    #       stop(messages$errorWrongResidualsScaling(value))
    #     }
    #     private$.residualsScaling <- value
    #   }
    # }
  ),
  private = list(
    .quantity = NULL,
    .observedXYData = NULL,
    .transformResultsFunction = NULL
    #    .residualsScaling = NULL
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param quantity An object of the type \code{Quantity}
    #' @return A new `PIOutputMapping` object.
    initialize = function(quantity) {
      ospsuite.utils::validateIsOfType(quantity, "Quantity")

      private$.quantity <- quantity
      private$.observedXYData <- hash::hash()
      #      private$.residualsScaling <- "lin"
    },
    #' @description
    #' Clean up upon object removal
    finalize = function() {
      hash::clear(private$.observedXYData)
    },

    #' Add observed data as \code{OSPSTimeValues} object(s).
    #' @details If an observed data object with the same label already exists, it will be overwritten.
    #' @param OSPSTimeValues Object or a list of objects of the type \code{OSPSTimeValues}. Each data set must
    #' be of the same dimension as the simulation quantity of the mapping.
    #' @export
    addObservedData = function(OSPSTimeValues) {
      ospsuite.utils::validateIsOfType(OSPSTimeValues, "XYData")
      OSPSTimeValues <- ospsuite.utils::toList(OSPSTimeValues)
      for (idx in seq_along(OSPSTimeValues)) {
        # Test if the dimension of the data to be added can be converted to the dimension of the quantity of this Output Mapping.
        invisible(ospsuite::toBaseUnit(
          quantityOrDimension = private$.quantity,
          values = 1,
          unit = OSPSTimeValues[[idx]]$yUnit,
          molWeight = OSPSTimeValues[[idx]]$MW
        ))
        private$.observedXYData[[OSPSTimeValues[[idx]]$label]] <- OSPSTimeValues[[idx]]
      }
    },

    #' @param label label of the x-y values series to be removed
    #' @description
    #' Remove the observed data.
    removeXYSeries = function(label) {
      hash::del(x = label, hash = private$.observedXYData)
      invisible(self)
    },

    #' @description Set the X-factors of x-y values by labels.
    #'
    #' @param labels A list of label of \code{XYData}
    #' @param xFactors Numeric values that will be multiplied by the x-values
    setXFactors = function(labels, xFactors) {
      ospsuite.utils::validateIsString(labels, nullAllowed = TRUE)
      ospsuite.utils::validateIsNumeric(xFactors, nullAllowed = TRUE)
      ospsuite.utils::validateIsSameLength(labels, xFactors)

      for (idx in seq_along(labels)) {
        xySeries <- self$observedXYData[[labels[[idx]]]]
        xySeries$xFactor <- xFactors[[idx]]
      }

      invisible(self)
    },

    #' @description Set the y-factors of x-y values by labels.
    #'
    #' @param labels A list of label of \code{XYData}
    #' @param yFactors Numeric values that will be multiplied by the y-values
    setYFactors = function(labels, yFactors) {
      ospsuite.utils::validateIsString(labels, nullAllowed = TRUE)
      ospsuite.utils::validateIsNumeric(yFactors, nullAllowed = TRUE)
      ospsuite.utils::validateIsSameLength(labels, yFactors)

      for (idx in seq_along(labels)) {
        xySeries <- self$observedXYData[[labels[[idx]]]]
        xySeries$yFactor <- yFactors[[idx]]
      }

      invisible(self)
    },

    #' @description Set the X-offset of x-y values by labels.
    #'
    #' @param labels A list of label of \code{XYData}
    #' @param xOffset Numeric values that will be added to the x-values
    setXOffset = function(labels, xOffset) {
      ospsuite.utils::validateIsString(labels, nullAllowed = TRUE)
      ospsuite.utils::validateIsNumeric(xOffset, nullAllowed = TRUE)
      ospsuite.utils::validateIsSameLength(labels, xOffset)

      for (idx in seq_along(labels)) {
        xySeries <- self$observedXYData[[labels[[idx]]]]
        xySeries$xOffset <- xOffset[[idx]]
      }

      invisible(self)
    },

    #' @description Apply settings stored in a dataMappingConfiguration
    #'
    #' @param dataMappingConfiguration An object of type \code{DataMappingConfiguration}
    setConfiguration = function(dataMappingConfiguration) {
      self$setXFactors(names(dataMappingConfiguration$xFactors), dataMappingConfiguration$xFactors)
      self$setYFactors(names(dataMappingConfiguration$yFactors), dataMappingConfiguration$yFactors)
      self$setXOffset(names(dataMappingConfiguration$xOffsets), dataMappingConfiguration$xOffsets)
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Output path", private$.quantity$path)
      private$printLine("Observed data labels", hash::keys(private$.observedXYData))
      #      private$printLine("Residuals scaling", hash::keys(private$.residualsScaling))
      invisible(self)
    }
  )
)
