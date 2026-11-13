#' @title PIOutputMapping
#' @docType class
#' @description Establishes connections between simulated quantities and
#'   corresponding observed data sets. Utilized within `ParameterIdentification`
#'   instances to align and compare simulation outputs with empirical data.
#' @export
#' @format NULL
PIOutputMapping <- R6::R6Class(
  "PIOutputMapping",
  cloneable = TRUE,
  active = list(
    #' @field observedDataSets A named list containing `DataSet` objects for
    #'   comparison with simulation outcomes. Stops if a data set was renamed
    #'   after it was added, because its weights and transformations are kept
    #'   under the name it was added with.
    observedDataSets = function(value) {
      if (missing(value)) {
        dataSets <- private$.observedDataSets
        currentNames <- vapply(dataSets, function(dataSet) dataSet$name, "")
        isRenamed <- names(dataSets) != currentNames
        if (any(isRenamed)) {
          stop(messages$errorRenamedDataSets(
            names(dataSets)[isRenamed],
            unname(currentNames[isRenamed])
          ))
        }
        as.list(dataSets)
      } else {
        stop(messages$errorPropertyReadOnly("observedDataSets"))
      }
    },

    #' @field dataTransformations A list with the numeric vectors `xOffsets`,
    #'   `yOffsets`, `xFactors` and `yFactors`. Each vector has one value per
    #'   observed data set, named by the data set and in the order of
    #'   `observedDataSets`. A data set without transformations has offsets of
    #'   0 and factors of 1.
    dataTransformations = function(value) {
      if (missing(value)) {
        dataSetNames <- names(private$.observedDataSets)
        identity <- c(xOffsets = 0, yOffsets = 0, xFactors = 1, yFactors = 1)
        transformations <- list()
        for (key in names(identity)) {
          values <- rep(identity[[key]], length(dataSetNames))
          names(values) <- dataSetNames
          stored <- private$.dataTransformations[[key]]
          values[names(stored)] <- stored
          transformations[[key]] <- values
        }
        transformations
      } else {
        stop(messages$errorPropertyReadOnly(
          "dataTransformations",
          optionalMessage = "Use $setDataTransformations() to change the value."
        ))
      }
    },

    #' @field dataWeights A named list of y-value weights.
    dataWeights = function(value) {
      if (missing(value)) {
        private$.dataWeights
      } else {
        stop(messages$errorPropertyReadOnly(
          "dataWeights",
          optionalMessage = "Use $setDataWeights() to change the value."
        ))
      }
    },

    #' @field subjectCount Named integer vector with the number of subjects
    #'   behind each observed data set, `NULL` until set with
    #'   `$setSubjectCount()`.
    subjectCount = function(value) {
      if (missing(value)) {
        private$.subjectCount
      } else {
        stop(messages$errorPropertyReadOnly(
          "subjectCount",
          optionalMessage = "Use $setSubjectCount() to change the value."
        ))
      }
    },

    #' @field quantity Simulation quantities to be aligned with observed data
    #'   values.
    quantity = function(value) {
      if (missing(value)) {
        private$.quantity
      } else {
        stop(messages$errorPropertyReadOnly("quantity"))
      }
    },

    #' @field simId Identifier of the simulation associated with the mapped
    #'   quantity.
    simId = function(value) {
      if (missing(value)) {
        private$.simId
      } else {
        stop(messages$errorPropertyReadOnly("simId"))
      }
    },

    #' @field scaling Specifies scaling for output mapping: linear (default) or
    #'   logarithmic.
    scaling = function(value) {
      if (missing(value)) {
        private$.scaling
      } else {
        ospsuite.utils::validateIsCharacter(value)
        ospsuite.utils::validateEnumValue(value, ScalingOptions)
        private$.scaling <- value
      }
    },

    #' @field transformResultsFunction A function to preprocess simulated
    #'   results (time and observation values) before residual calculation. It
    #'   takes numeric vectors `xVals` and `yVals`, and returns a named list
    #'   with keys `xVals` and `yVals`.
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
    .subjectCount = NULL,
    .scaling = NULL,
    # A replaced or removed data set must not keep the count of the old one
    .dropSubjectCount = function(label) {
      private$.subjectCount <- private$.subjectCount[
        names(private$.subjectCount) != label
      ]
    }
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
      private$.dataTransformations <- list()
      private$.scaling <- "lin"
    },

    #' @description Adds or updates observed data using `DataSet` objects.
    #' @details Replaces any existing dataset with the same label. A replaced
    #'   data set keeps its weights and transformations, and loses its subject
    #'   count.
    #' @param data A `DataSet` object or a list thereof, matching the simulation
    #'   quantity dimensions.
    #' @param weights A named list of numeric values or numeric vectors. The
    #'   names must match the names of the observed datasets.
    #'
    #' @export
    addObservedDataSets = function(data, weights = NULL) {
      ospsuite.utils::validateIsOfType(data, "DataSet")
      data <- ospsuite.utils::toList(data)

      if (length(private$.dataWeights) > 0) {
        existingLabels <- names(private$.dataWeights)
        newLabels <- sapply(data, `[[`, "name")
        if (any(!newLabels %in% existingLabels)) {
          warning(messages$warningDataWeightsPresent())
        }
      }

      for (idx in seq_along(data)) {
        # Verify if the data's dimension can match the quantity's dimension in
        # this `OUtputMapping`
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
              private$.quantity$name,
              data[[idx]]$name
            ))
          }
        }

        private$.dropSubjectCount(data[[idx]]$name)
        private$.observedDataSets[[data[[idx]]$name]] <- data[[idx]]
      }

      # Handle optional weights
      if (!is.null(weights)) {
        self$setDataWeights(weights)
      }

      return(invisible(self))
    },

    #' @description Removes specified observed data series, together with its
    #'   weights, transformations and subject count.
    #' @param label The label of the observed data series to remove.
    removeObservedDataSet = function(label) {
      private$.dropSubjectCount(label)
      private$.dataWeights <- private$.dataWeights[
        names(private$.dataWeights) != label
      ]
      private$.dataTransformations <- lapply(
        private$.dataTransformations,
        function(values) values[names(values) != label]
      )
      private$.observedDataSets[[label]] <- NULL
      invisible(self)
    },

    #' @description Sets offsets and factors that transform the observed data
    #'   before they are compared with the simulation. The x-values become
    #'   `(x + xOffset) * xFactor` and the y-values `(y + yOffset) * yFactor`,
    #'   with offsets in the units of the data set.
    #'
    #' @param xOffsets,yOffsets,xFactors,yFactors `NULL` (default) to keep the
    #'   current values, one unnamed number for every observed data set, or a
    #'   numeric vector named by observed data sets. Named values change only
    #'   the named data sets and keep the values of the others. All values must
    #'   be finite.
    setDataTransformations = function(
      xOffsets = NULL,
      yOffsets = NULL,
      xFactors = NULL,
      yFactors = NULL
    ) {
      dataSetNames <- names(private$.observedDataSets)
      if (length(dataSetNames) == 0) {
        stop(messages$errorNoObservedDataSetsForTransformations())
      }

      values <- list(
        xOffsets = xOffsets,
        yOffsets = yOffsets,
        xFactors = xFactors,
        yFactors = yFactors
      )
      transformations <- private$.dataTransformations
      for (key in names(values)) {
        value <- values[[key]]
        if (is.null(value)) {
          next
        }
        if (
          !is.numeric(value) || length(value) == 0 || !all(is.finite(value))
        ) {
          stop(messages$errorTransformationValues(key))
        }
        labels <- names(value)
        if (is.null(labels) && length(value) == 1) {
          labels <- dataSetNames
        }
        if (is.null(labels) || any(!labels %in% dataSetNames)) {
          stop(messages$errorTransformationNames(key, dataSetNames))
        }
        transformations[[key]][labels] <- unname(value)
      }
      private$.dataTransformations <- transformations
      invisible(self)
    },

    #' @description Assigns weights to observed data sets for residual weighting
    #'   during parameter identification.
    #'
    #' @param weights A named list of numeric values or numeric vectors. The
    #'   names must match the names of the observed datasets.
    #'
    #'   Each element in the list can be:
    #'   - a scalar, which will be broadcast to all y-values of the
    #'   corresponding dataset,
    #'   - or a numeric vector matching the number of y-values for that dataset.
    #'
    #'   To apply both dataset-level and point-level weights, multiply them
    #'   beforehand and provide the combined result as a single numeric vector
    #'   per dataset.
    setDataWeights = function(weights) {
      # Return early if no datasets are present
      if (length(private$.observedDataSets) == 0) {
        stop(messages$errorNoObservedDataSets())
      }

      ospsuite.utils::validateIsOfType(weights, "list")
      lapply(weights, ospsuite.utils::validateIsNumeric, FALSE)

      labels <- names(weights)
      if (
        is.null(labels) || any(!labels %in% names(private$.observedDataSets))
      ) {
        stop(messages$errorWeightsNames())
      }

      for (label in labels) {
        yLength <- length(private$.observedDataSets[[label]]$yValues)
        weightsVec <- weights[[label]]

        if (length(weightsVec) == 1) {
          weightsVec <- rep(weightsVec, yLength)
        }

        if (length(weightsVec) != yLength) {
          stop(messages$errorWeightsVectorLengthMismatch(
            label,
            yLength,
            length(weightsVec)
          ))
        }

        private$.dataWeights[[label]] <- weightsVec
      }

      invisible(self)
    },

    #' @description Sets the number of subjects behind the aggregated data
    #'   sets of this mapping (data sets with `yErrorType` and error values).
    #'   Bootstrap confidence intervals redraw the means of aggregated data
    #'   from the reported mean and `SD / sqrt(n)`, so they need this number
    #'   for every aggregated data set and stop without it. See
    #'   `vignette("confidence-intervals")`.
    #'
    #' @param n A positive whole number, or a named vector of them. One unnamed
    #'   number applies to every observed data set of the mapping. Names must
    #'   match the names of the observed data sets, and set the number of each
    #'   named data set while keeping the others.
    setSubjectCount = function(n) {
      if (length(private$.observedDataSets) == 0) {
        stop(messages$errorNoObservedDataSetsForSubjectCount())
      }
      ospsuite.utils::validateIsNumeric(n)
      if (
        any(!is.finite(n) | n < 1 | n != round(n) | n > .Machine$integer.max)
      ) {
        stop(messages$errorSubjectCount(n))
      }

      labels <- names(n)
      if (is.null(labels) && length(n) == 1) {
        labels <- names(private$.observedDataSets)
      }
      if (
        is.null(labels) || any(!labels %in% names(private$.observedDataSets))
      ) {
        stop(messages$errorSubjectCountNames(names(private$.observedDataSets)))
      }

      private$.subjectCount[labels] <- as.integer(n)
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
