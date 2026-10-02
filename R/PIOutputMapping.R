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
    #'   comparison with simulation outcomes.
    observedDataSets = function(value) {
      if (missing(value)) {
        as.list(private$.observedDataSets)
      } else {
        stop(messages$errorPropertyReadOnly("observedDataSets"))
      }
    },

    #' @field dataTransformations A named list of the offsets and factors of
    #'   the data transformations, `xOffsets`, `yOffsets`, `xFactors` and
    #'   `yFactors`. Each holds the values of the last call of
    #'   `setDataTransformations()` without labels, or, after a call with
    #'   labels, one value per observed data set, named by the data sets.
    dataTransformations = function(value) {
      if (missing(value)) {
        private$.dataTransformations
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
    # The transformations of the last call of `setDataTransformations()`
    # without labels, which a data set added later gets when the
    # transformations are set by data set
    .transformationDefaults = NULL,
    .dataWeights = NULL,
    .scaling = NULL,

    # Gives a new data set the default transformations, where the
    # transformations are set by data set, and keeps those of a data set that
    # replaces one with its name
    .addDataSetTransformations = function(label) {
      for (name in names(private$.dataTransformations)) {
        values <- private$.dataTransformations[[name]]
        if (!is.null(names(values)) && !label %in% names(values)) {
          values[[label]] <- private$.transformationDefaults[[name]]
          private$.dataTransformations[[name]] <- values
        }
      }
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
      private$.dataTransformations <- .noDataTransformations
      private$.transformationDefaults <- .noDataTransformations
      private$.scaling <- "lin"
    },

    #' @description Adds or updates observed data using `DataSet` objects.
    #' @details Replaces any existing dataset with the same label.
    #' @param data A `DataSet` object or a list thereof, matching the simulation
    #'   quantity dimensions.
    #' @param weights A named list of numeric values or numeric vectors. The
    #'   names must match the names of the observed datasets.
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

        private$.observedDataSets[[data[[idx]]$name]] <- data[[idx]]
        private$.addDataSetTransformations(data[[idx]]$name)
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
      # Transformations set by data set lose the value of the data set
      for (name in names(private$.dataTransformations)) {
        values <- private$.dataTransformations[[name]]
        if (!is.null(names(values))) {
          private$.dataTransformations[[name]] <- values[names(values) != label]
        }
      }
      invisible(self)
    },

    #' @description Configures transformations for datasets. X and y values
    #'   are transformed as `(value + offset) * factor`, and the objective
    #'   function uses the transformed values. The other columns of the
    #'   observed data follow the y values:
    #'
    #'   - The LLOQ is transformed like the y values, and so are the values
    #'     below it, which the importer stores as half the LLOQ. With a
    #'     negative `yFactors`, the LLOQ is set to `NA`, and the values of the
    #'     data set are used without an LLOQ, also its values below the LLOQ.
    #'   - Arithmetic standard deviations are multiplied by `abs(yFactors)`.
    #'   - Geometric standard deviations are not changed by `yFactors`. A y
    #'     offset adjusts them approximately, and sets them to `NA` where a y
    #'     value is not positive before or after the offset.
    #'
    #'   A negative y offset can make the LLOQ, or the values below it, 0 or
    #'   negative. With log scaling, the LLOQ must stay positive to have a
    #'   logarithm, so the y offset must be greater than minus the LLOQ. With
    #'   `objectiveFunctionType = "m3"`, the values below the LLOQ enter the
    #'   cost too and must stay positive, so the y offset must be greater than
    #'   minus half the LLOQ. Otherwise the parameter identification stops
    #'   with an error. See `ospsuite::DataCombined` for the limits of the
    #'   approximation for geometric standard deviations.
    #'
    #'   Each call sets all four transformations of the data sets it applies
    #'   to: an offset or factor that is not given is set to its default, also
    #'   with labels.
    #' @param labels Names of the observed data sets to transform. Without
    #'   labels, the transformations apply to all data sets of the output
    #'   mapping, and single values also to data sets added later. With labels,
    #'   they apply only to these data sets, which must be added to the output
    #'   mapping first, and the other data sets keep their transformations. A
    #'   data set added after a call with labels gets the single values of the
    #'   last call without labels, and no transformation where that call gave
    #'   one value per data set.
    #' @param xOffsets Numeric, the offset of the x values. Default is `0`.
    #' @param yOffsets Numeric, the offset of the y values. Default is `0`.
    #' @param xFactors Numeric, the factor of the x values. Default is `1`.
    #' @param yFactors Numeric, the factor of the y values. Default is `1`.
    #'
    #'   Each offset and factor is one value, or one value per label, in the
    #'   order of the labels. Without labels, it can also be one value per data
    #'   set, in the order of the data sets. Such values do not apply to data
    #'   sets added later: after adding or removing a data set, set them again.
    #'   Values named by the labels, or without labels by the data sets, are
    #'   taken by their names, in any order. Other names are ignored.
    setDataTransformations = function(
      labels = NULL,
      xOffsets = 0,
      yOffsets = 0,
      xFactors = 1,
      yFactors = 1
    ) {
      ospsuite.utils::validateIsString(labels, nullAllowed = TRUE)
      ospsuite.utils::validateIsNumeric(xOffsets, nullAllowed = TRUE)
      ospsuite.utils::validateIsNumeric(xFactors, nullAllowed = TRUE)
      ospsuite.utils::validateIsNumeric(yFactors, nullAllowed = TRUE)
      ospsuite.utils::validateIsNumeric(yOffsets, nullAllowed = TRUE)

      if (is.list(labels)) {
        labels <- as.character(unlist(labels))
      }

      if (is.null(labels)) {
        # If no labels are given, reuse parameters across datasets. Values
        # without labels apply by position, unless they are named by the
        # data sets, so they are stored without names: only values set with
        # labels are named by the data sets.
        dataSetNames <- names(private$.observedDataSets)
        private$.dataTransformations$xFactors <- .valuesInOrder(
          xFactors,
          dataSetNames
        )
        private$.dataTransformations$yFactors <- .valuesInOrder(
          yFactors,
          dataSetNames
        )
        private$.dataTransformations$xOffsets <- .valuesInOrder(
          xOffsets,
          dataSetNames
        )
        private$.dataTransformations$yOffsets <- .valuesInOrder(
          yOffsets,
          dataSetNames
        )
        # Data sets added later get single values, and no transformation
        # instead of values given per data set
        for (name in names(.noDataTransformations)) {
          value <- private$.dataTransformations[[name]]
          private$.transformationDefaults[[name]] <- if (length(value) == 1) {
            value
          } else {
            .noDataTransformations[[name]]
          }
        }
        return(invisible(self))
      }

      # Otherwise, apply transformations only to labeled data. Labels
      # computed from other names can be empty, which sets nothing.
      if (length(labels) == 0) {
        return(invisible(self))
      }
      if (anyDuplicated(labels) > 0) {
        stop(
          messages$errorTransformationDuplicateLabels(
            unique(labels[duplicated(labels)])
          ),
          call. = FALSE
        )
      }
      dataSetNames <- names(private$.observedDataSets)
      unknownLabels <- setdiff(labels, dataSetNames)
      if (length(unknownLabels) > 0) {
        stop(
          messages$errorTransformationLabels(unknownLabels, dataSetNames),
          call. = FALSE
        )
      }
      values <- list(
        xOffsets = xOffsets,
        yOffsets = yOffsets,
        xFactors = xFactors,
        yFactors = yFactors
      )
      for (name in names(values)) {
        if (!length(values[[name]]) %in% c(1, length(labels))) {
          stop(
            messages$errorTransformationValues(
              name,
              length(values[[name]]),
              length(labels)
            ),
            call. = FALSE
          )
        }
      }
      # One value per data set, in the order of the data sets: the labeled
      # data sets get the given values, the others keep theirs. Values of an
      # earlier call without labels that are neither one value nor one value
      # per data set cannot be kept.
      transformations <- .transformationsByDataSet(
        private$.dataTransformations,
        dataSetNames
      )
      for (name in names(values)) {
        if (!identical(names(transformations[[name]]), dataSetNames)) {
          stop(
            messages$errorTransformationValuesPerDataSet(
              name,
              length(private$.dataTransformations[[name]]),
              length(dataSetNames)
            ),
            call. = FALSE
          )
        }
      }
      for (name in names(values)) {
        transformations[[name]][labels] <- .valuesInOrder(
          values[[name]],
          labels
        )
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

# The data transformations of an output mapping without a transformation
.noDataTransformations <- list(
  xOffsets = 0,
  yOffsets = 0,
  xFactors = 1,
  yFactors = 1
)

#' Values of a data transformation in a given order
#'
#' @description Values named by `targetNames`, each once and in any order, are
#'   put in the order of `targetNames`. Other values are kept in their order.
#'
#' @param values The values of an offset or a factor.
#' @param targetNames The names of the data sets or the labels the values are
#'   for, in their order.
#'
#' @return `values` without names.
#' @keywords internal
#' @noRd
.valuesInOrder <- function(values, targetNames) {
  valueNames <- names(values)
  if (
    length(values) > 1 &&
      length(values) == length(targetNames) &&
      !anyDuplicated(valueNames) &&
      setequal(valueNames, targetNames)
  ) {
    values <- values[targetNames]
  }
  unname(values)
}

#' Data transformations of every data set of an output mapping
#'
#' @description The offsets and factors of the data transformations of an
#'   output mapping, one value per observed data set, in the order of the data
#'   sets. A single value, set without labels, applies to every data set.
#'   Values set by data set, with labels, are taken by the name of the data
#'   set. Values given per data set without labels are kept in their order,
#'   and are left unnamed when their number differs from that of the data
#'   sets, for `.batchInitialization()` or `DataCombined` to stop with an
#'   error.
#'
#' @param transformations The data transformations of the output mapping
#'   (`PIOutputMapping$dataTransformations`).
#' @param dataSetNames The names of the observed data sets of the output
#'   mapping, in their order.
#'
#' @return A list with `xOffsets`, `yOffsets`, `xFactors` and `yFactors`, each
#'   with one value per data set, named by the data sets.
#' @keywords internal
#' @noRd
.transformationsByDataSet <- function(transformations, dataSetNames) {
  lapply(transformations, function(values) {
    if (!is.null(names(values))) {
      values <- values[dataSetNames]
    } else if (length(values) == 1) {
      values <- rep(values, length(dataSetNames))
    }
    if (length(values) == length(dataSetNames)) {
      names(values) <- dataSetNames
    }
    values
  })
}
