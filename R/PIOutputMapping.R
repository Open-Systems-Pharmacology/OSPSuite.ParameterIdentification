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

    #' @field dataWeights A named list of y-value weights.
    dataWeights = function(value) {
      if (missing(value)) {
        private$.dataWeights
      } else {
        stop(messages$errorPropertyReadOnly("dataWeights", optionalMessage = "Use $setDataWeights() to change the value."))
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

    #' @field simId Identifier of the simulation associated with the mapped quantity.
    simId = function(value) {
      if (missing(value)) {
        private$.simId
      } else {
        stop(messages$errorPropertyReadOnly("simId"))
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
    .simId = NULL,
    .observedDataSets = NULL,
    .transformResultsFunction = NULL,
    .dataTransformations = NULL,
    .dataWeights = NULL,
    .scaling = NULL
  ),
  public = list(
    #' @description Initialize a new instance of the class.
    #' @param quantity An object of the type `Quantity`.
    #' @return A new `PIOutputMapping` object.
    initialize = function(quantity) {
      ospsuite.utils::validateIsOfType(quantity, "Quantity")
      private$.quantity <- quantity
      private$.simId <- .getSimulationContainer(quantity)$id
      private$.observedDataSets <- list()
      private$.dataTransformations <- list(xOffsets = 0, yOffsets = 0, xFactors = 1, yFactors = 1)
      private$.scaling <- "lin"
    },

    #' Adds or updates observed data using `DataSet` objects.
    #' @details Replaces any existing data set with the same label.
    #' @param data A `DataSet` object or a list thereof, matching the simulation
    #' quantity dimensions.
    #' @param weights A named list of numeric values or numeric vectors. The names
    #' must match the names of the observed datasets.
    #'
    #' @export
    addObservedDataSets = function(data, weights = NULL) {
      ospsuite.utils::validateIsOfType(data, "DataSet")
      data <- ospsuite.utils::toList(data)

      if (!is.null(private$.dataWeights)) {
        existingLabels <- names(private$.dataWeights)
        newLabels <- sapply(data, `[[`, "name")
        if (any(!newLabels %in% existingLabels)) {
          warning(messages$warningDataWeightsPresent())
        }
      }

      for (idx in seq_along(data)) {
        # Verify if the data's dimension can match the quantity's dimension
        # in this Output Mapping
        tryConvert <- function() {
          ospsuite::toBaseUnit(
            quantityOrDimension = private$.quantity,
            values = 1,
            unit = data[[idx]]$yUnit,
            molWeight = data[[idx]]$molWeight
          )
        }
        result <- try(tryConvert(), silent = TRUE)

        if (inherits(result, "try-error")) {
          result <- try(
            {
              data[[idx]]$molWeight <- ospsuite::getMolWeightFor(
                private$.quantity,
                unit = "g/mol"
              )
              tryConvert()
            },
            silent = TRUE
          )

          if (inherits(result, "try-error")) {
            stop(messages$errorUnitConversion(
              private$.quantity$name, data[[idx]]$name
            ))
          }
        }

        private$.observedDataSets[[data[[idx]]$name]] <- data[[idx]]
      }

      # Handle optional weights
      if (!is.null(weights)) {
        self$setDataWeights(weights)
      }

      return(invisible(self))
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

    #' @description Assigns weights to observed data sets for residual weighting
    #' during parameter identification.
    #'
    #' @param weights A named list of numeric values or numeric vectors. The names
    #' must match the names of the observed datasets.
    #'
    #' Each element in the list can be:
    #' - a scalar, which will be broadcast to all y-values of the corresponding
    #' dataset,
    #' - or a numeric vector matching the number of y-values for that dataset.
    #'
    #' To apply both dataset-level and point-level weights, multiply them beforehand
    #' and provide the combined result as a single numeric vector per dataset.
    setDataWeights = function(weights) {
      # Return early if no datasets are present
      if (length(private$.observedDataSets) == 0) {
        stop(messages$errorNoObservedDataSets())
      }

      ospsuite.utils::validateIsOfType(weights, "list")
      lapply(weights, ospsuite.utils::validateIsNumeric, FALSE)

      labels <- names(weights)
      if (is.null(labels) || any(!labels %in% names(private$.observedDataSets))) {
        stop(messages$errorWeightsNames())
      }

      for (label in labels) {
        yLength <- length(private$.observedDataSets[[label]]$yValues)
        weightsVec <- weights[[label]]

        if (length(weightsVec) == 1) {
          weightsVec <- rep(weightsVec, yLength)
        }

        if (length(weightsVec) != yLength) {
          stop(messages$errorWeightsLengthMismatch(label, yLength, length(weightsVec)))
        }

        private$.dataWeights[[label]] <- weightsVec
      }

      invisible(self)
    },

    #' @description Prints a summary of the PIOutputMapping.
    print = function() {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(
        list(
          "Output path" = private$.quantity$path,
          "Observed data labels" = names(private$.observedDataSets),
          "Data weight labels" = names(private$.dataWeights),
          "Scaling" = private$.scaling
        ),
        print_empty = TRUE
      )
    }
  )
)
