#' @title PIObservations
#' @docType class
#' @description The observed data of the output mappings of a task, read once
#'   and never changed. `resample()` returns the data of one bootstrap
#'   replicate as a new instance, so the `DataSet` objects of the user are
#'   never written. The `ParameterIdentification` facade builds one instance
#'   per public call and `PIObjective` reads the prepared data from it.
#'
#' @keywords internal
#' @noRd
PIObservations <- R6::R6Class(
  "PIObservations",
  cloneable = FALSE,
  private = list(
    # List of `PIOutputMapping` objects: those of the task, or clones carrying
    # the copied, resampled data sets and the resampled weights
    .outputMappings = NULL,
    # The prepared observed data of each output mapping (see
    # `.prepareObservedData()`)
    .observedData = NULL,

    # The means of an aggregated data set redrawn for `subjectCount` subjects
    # (see `.resampleDataSetValues()`), the values of other data sets
    .redrawMeans = function(dataSet, subjectCount) {
      yErrorValues <- dataSet$yErrorValues
      # An arithmetic SD may be in another unit than the means
      if (identical(dataSet$yErrorType, "ArithmeticStdDev")) {
        yErrorValues <- ospsuite::toUnit(
          dataSet$yDimension,
          yErrorValues,
          targetUnit = dataSet$yUnit,
          sourceUnit = dataSet$yErrorUnit
        )
      }
      .resampleDataSetValues(
        list(
          yValues = dataSet$yValues,
          yErrorValues = dataSet$yErrorValues,
          yErrorType = dataSet$yErrorType
        ),
        subjectCount = subjectCount,
        yErrorValues = yErrorValues
      )$yValues
    }
  ),
  active = list(
    # The output mappings the data were read from (read-only)
    outputMappings = function(value) {
      if (missing(value)) {
        private$.outputMappings
      } else {
        stop(messages$errorPropertyReadOnly("outputMappings"))
      }
    },
    # The prepared observed data of each output mapping (read-only)
    observedData = function(value) {
      if (missing(value)) {
        private$.observedData
      } else {
        stop(messages$errorPropertyReadOnly("observedData"))
      }
    }
  ),
  public = list(
    # @param outputMappings List of `PIOutputMapping` objects.
    initialize = function(outputMappings) {
      ospsuite.utils::validateIsOfType(outputMappings, "PIOutputMapping")
      private$.outputMappings <- outputMappings
      private$.observedData <- lapply(outputMappings, .prepareObservedData)
    },

    # The data of one bootstrap replicate: the weights of the individual data
    # sets of every output mapping resampled (see `.resampleDataSetWeights()`)
    # and the means of the aggregated data sets with a subject count redrawn,
    # all under `seed`. Aggregated data sets with error values keep their
    # weights. The global random state is left as it was.
    # @param seed Integer seed of the replicate.
    # @return A new `PIObservations` object.
    resample = function(seed) {
      ospsuite.utils::validateIsNumeric(seed)
      mappings <- lapply(private$.outputMappings, function(mapping) {
        mapping$clone()
      })
      .withSeed(seed, {
        weights <- lapply(mappings, function(mapping) {
          weights <- .completeDataWeights(mapping)
          isIndividual <- !vapply(
            mapping$observedDataSets,
            .isAggregatedWithErrors,
            logical(1)
          )
          if (any(isIndividual)) {
            weights[isIndividual] <- .resampleDataSetWeights(
              weights[isIndividual]
            )
          }
          weights
        })
        for (idx in seq_along(mappings)) {
          mapping <- mappings[[idx]]
          # Weights for every data set first, so that replacing a data set
          # below does not warn about weights of other data sets
          mapping$setDataWeights(weights[[idx]])
          subjectCounts <- mapping$subjectCount %||% integer()
          for (dataSet in mapping$observedDataSets) {
            subjectCount <- unname(subjectCounts[dataSet$name])
            if (is.na(subjectCount)) {
              next
            }
            # Replacing the data set drops its count, restored below
            mapping$addObservedDataSets(.copyDataSet(
              dataSet,
              yValues = private$.redrawMeans(dataSet, subjectCount)
            ))
          }
          if (length(subjectCounts) > 0) {
            mapping$setSubjectCount(subjectCounts)
          }
        }
      })
      PIObservations$new(mappings)
    }
  )
)

#' Copy a data set
#'
#' @param dataSet A `DataSet` object.
#' @param yValues The y values of the copy, in the unit of `dataSet`.
#' @return A new `DataSet` with the name, dimensions, units, molecular weight,
#'   error type and LLOQ of `dataSet`, its x and error values and `yValues`.
#' @keywords internal
#' @noRd
.copyDataSet <- function(dataSet, yValues = dataSet$yValues) {
  copy <- ospsuite::DataSet$new(name = dataSet$name)
  copy$xDimension <- dataSet$xDimension
  copy$xUnit <- dataSet$xUnit
  copy$yDimension <- dataSet$yDimension
  copy$yUnit <- dataSet$yUnit
  if (!is.null(dataSet$molWeight)) {
    copy$molWeight <- dataSet$molWeight
  }
  copy$setValues(dataSet$xValues, yValues, dataSet$yErrorValues)
  if (!is.null(dataSet$yErrorType)) {
    copy$yErrorType <- dataSet$yErrorType
    copy$yErrorUnit <- dataSet$yErrorUnit
  }
  if (!is.null(dataSet$LLOQ)) {
    copy$LLOQ <- dataSet$LLOQ
  }
  copy
}

#' The data weights of every data set of an output mapping
#'
#' @param mapping A `PIOutputMapping` object.
#' @return A named list in the order of `mapping$observedDataSets`, with the
#'   weights of the mapping or a weight of 1 per y value where it has none.
#' @keywords internal
#' @noRd
.completeDataWeights <- function(mapping) {
  dataSets <- mapping$observedDataSets
  weights <- mapping$dataWeights
  lapply(stats::setNames(nm = names(dataSets)), function(name) {
    weights[[name]] %||% rep(1, length(dataSets[[name]]$yValues))
  })
}
