#' @title PIOutputMapping
#' @docType class
#' @description Establishes connections between simulated quantities and corresponding
#' observed data sets. Utilized within `ParameterIdentification` instances to align
#' and compare simulation outputs with empirical data.
#' @import R6 ospsuite.utils
#' @export
#' @format NULL
PIOutputMapping <- R6::R6Class(
  "PIOutputMapping",
  inherit = ospsuite.utils::Printable,
  cloneable = TRUE,
  active = list(
    #' @field observedDataSets A named list containing `DataSet` objects for comparison
    #' with simulation outcomes.
    observedDataSets = function(value) {
      if (missing(value)) {
        as.list(private$.observedDataSets)
      } else {
        stop(messages$errorPropertyReadOnly("observedDataSets"))
      }
    },

    #' @field dataTransformations A named list of factors and offsets.
    dataTransformations = function(value) {
      if (missing(value)) {
        private$.dataTransformations
      } else {
        stop(messages$errorPropertyReadOnly("dataTransformations", optionalMessage = "Use $setDataTransformations() to change the value."))
      }
    },

    #' @field quantity Simulation quantities to be aligned with observed data values.
    quantity = function(value) {
      if (missing(value)) {
        private$.quantity
      } else {
        stop(messages$errorPropertyReadOnly("quantity"))
      }
    },

    #' @field scaling Specifies scaling for output mapping: linear (default) or logarithmic.
    scaling = function(value) {
      if (missing(value)) {
        private$.scaling
      } else {
        ospsuite.utils::validateIsCharacter(value)
        ospsuite.utils::validateEnumValue(value, ScalingOptions)
        private$.scaling <- value
      }
    },

    #' @field transformResultsFunction A function to preprocess simulated results
    #' (time and observation values) before residual calculation. It takes
    #' numeric vectors 'xVals' and 'yVals', and returns a named list with keys
    #' 'xVals' and 'yVals'.
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
    #' @description Initialize a new instance of the class.
    #' @param quantity An object of the type `Quantity`.
    #' @return A new `PIOutputMapping` object.
    initialize = function(quantity) {
      ospsuite.utils::validateIsOfType(quantity, "Quantity")
      private$.quantity <- quantity
      private$.observedDataSets <- list()
      private$.dataTransformations <- list(xOffsets = 0, yOffsets = 0, xFactors = 1, yFactors = 1)
      private$.scaling <- "lin"
    },

    #' Adds or updates observed data using `DataSet` objects.
    #' @details Replaces any existing data set with the same label.
    #' @param data A `DataSet` object or a list thereof, matching the simulation
    #' quantity dimensions.
    #' @export
    addObservedDataSets = function(data) {
      ospsuite.utils::validateIsOfType(data, "DataSet")
      data <- ospsuite.utils::toList(data)
      for (idx in seq_along(data)) {
        # Verify if the data's dimension can match the quantity's dimension
        # in this Output Mapping
        invisible(ospsuite::toBaseUnit(
          quantityOrDimension = private$.quantity,
          values = 1,
          unit = data[[idx]]$yUnit,
          molWeight = data[[idx]]$molWeight
        ))
        private$.observedDataSets[[data[[idx]]$name]] <- data[[idx]]
      }
    },

    #' @description Removes specified observed data series.
    #' @param label The label of the observed data series to remove.
    removeObservedDataSet = function(label) {
      private$.observedDataSets[[label]] <- NULL
      invisible(self)
    },

    #' @description Configures transformations for dataset(s).
    #' @param labels List of dataset labels for targeted transformations.
    #' Absence of labels applies transformations globally.
    #' @param xOffsets Numeric list/value for X-offset adjustments.
    #' @param yOffsets Numeric list/value for Y-offset adjustments.
    #' @param xFactors Numeric list/value for X-scaling factors.
    #' @param yFactors Numeric list/value for Y-scaling factors.
    setDataTransformations = function(labels = NULL,
                                      xOffsets = 0,
                                      yOffsets = 0,
                                      xFactors = 1,
                                      yFactors = 1) {
      ospsuite.utils::validateIsString(labels, nullAllowed = TRUE)
      ospsuite.utils::validateIsNumeric(xOffsets, nullAllowed = TRUE)
      ospsuite.utils::validateIsNumeric(xFactors, nullAllowed = TRUE)
      ospsuite.utils::validateIsNumeric(yFactors, nullAllowed = TRUE)
      ospsuite.utils::validateIsNumeric(yOffsets, nullAllowed = TRUE)

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

    #' @description Prints a summary of the PIOutputMapping.
    print = function() {
      private$printClass()
      private$printLine("Output path", private$.quantity$path)
      private$printLine("Observed data labels", names(private$.observedDataSets))
      private$printLine("Scaling", private$.scaling)
      invisible(self)
    }
  )
)
