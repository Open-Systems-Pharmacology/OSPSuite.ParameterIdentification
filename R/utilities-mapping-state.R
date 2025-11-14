# MAPPING STATE: EXTRACT

#' Extract Output Mapping State
#'
#' Captures the original weights and values of all datasets in the output
#' mappings. Used to restore state after bootstrap resampling.
#'
#' @param outputMappings A list of `PIOutputMapping` objects.
#' @return A named list with fields `dataSetWeights` and `dataSetValues`.
#' @keywords internal
#' @noRd
.extractOutputMappingState <- function(outputMappings) {
  list(
    dataSetWeights = .extractMappingWeights(outputMappings),
    dataSetValues = .extractMappingValues(outputMappings)
  )
}

#' Map over output mappings
#'
#' Applies a function to each output mapping in a list and returns the results.
#'
#' @param outputMappings A list of `PIOutputMapping` objects.
#' @param fn A function to apply to each output mapping.
#' @return A list of results with the same length and (optional) names as
#'   `outputMappings`.
#' @keywords internal
#' @noRd
.mapOverOutputMappings <- function(outputMappings, fn) {
  results <- vector("list", length(outputMappings))
  names(results) <- names(outputMappings) %||% NULL

  for (i in seq_along(outputMappings)) {
    results[[i]] <- fn(outputMappings[[i]])
  }

  return(results)
}

#' Extract Initial Mapping Weights
#'
#' Extracts dataset weights from each output mapping. If weights are missing,
#' they are initialized to 1 for each observed y-value.
#'
#' @param outputMappings A list of `PIOutputMapping` objects.
#' @return A list of named dataset weight lists, one per output mapping. Each
#'   dataset weight list contains numeric vectors of y-value weights.
#' @keywords internal
#' @noRd
.extractMappingWeights <- function(outputMappings) {
  .mapOverOutputMappings(outputMappings, function(mapping) {
    observedDataSets <- mapping$observedDataSets
    currentWeights <- mapping$dataWeights

    weights <- vector("list", length(observedDataSets))
    names(weights) <- names(observedDataSets)

    for (dataSet in observedDataSets) {
      weights[[dataSet$name]] <- if (!is.null(currentWeights[[dataSet$name]])) {
        currentWeights[[dataSet$name]]
      } else {
        rep(1, length(dataSet$yValues))
      }
    }

    return(weights)
  })
}

#' Extract Original Dataset Values
#'
#' Stores x/y/yError values and yErrorType for each dataset to allow restoring
#' later.
#'
#' @param outputMappings A list of `PIOutputMapping` objects.
#' @return A list of named dataset value lists, one per output mapping. Each
#'   dataset value list contains x/y/yError values and yErrorType.
#' @keywords internal
#' @noRd
.extractMappingValues <- function(outputMappings) {
  .mapOverOutputMappings(outputMappings, function(mapping) {
    observedDataSets <- mapping$observedDataSets

    values <- vector("list", length(observedDataSets))
    names(values) <- names(observedDataSets)

    for (dataSet in observedDataSets) {
      values[[dataSet$name]] <- .extractDataSetValues(dataSet)
    }

    return(values)
  })
}

#' Extract values from a dataset
#'
#' Returns the x/y/yError values and yErrorType of a single dataset.
#'
#' @param dataSet A `DataSet` object.
#' @return A named list with elements `xValues`, `yValues`, `yErrorValues`, and
#'   `yErrorType`.
#' @keywords internal
#' @noRd
.extractDataSetValues <- function(dataSet) {
  list(
    xValues = dataSet$xValues,
    yValues = dataSet$yValues,
    yErrorValues = dataSet$yErrorValues,
    yErrorType = dataSet$yErrorType
  )
}


# MAPPING STATE: RESTORE

#' Restore Output Mapping State (Weights and Values)
#'
#' Applies both dataset weights and values back to a list of output mappings.
#'
#' @param outputMappings A list of `PIOutputMapping` objects to modify.
#' @param mappingState A named list with fields `dataSetWeights` and
#'   `dataSetValues`, typically created by `.extractOutputMappingState()`.
#' @return The modified list of `PIOutputMapping` objects.
#' @keywords internal
#' @noRd
.applyOutputMappingState <- function(outputMappings, mappingState) {
  outputMappings <- .applyMappingWeights(outputMappings, mappingState$dataSetWeights)
  outputMappings <- .applyMappingValues(outputMappings, mappingState$dataSetValues)

  return(outputMappings)
}

#' Apply Dataset Weights to Output Mappings
#'
#' Assigns dataset weights to each output mapping using the mapping-level weight
#' structure.
#'
#' @param outputMappings A list of `PIOutputMapping` objects.
#' @param mappingWeights A list of dataset weight lists, one per output mapping.
#' @return The updated list of `PIOutputMapping` objects with applied weights.
#' @keywords internal
#' @noRd
.applyMappingWeights <- function(outputMappings, mappingWeights) {
  for (idx in seq_along(outputMappings)) {
    outputMappings[[idx]]$setDataWeights(mappingWeights[[idx]])
  }
  return(outputMappings)
}

#' Apply Dataset Values to Output Mappings
#'
#' Restores xValues, yValues, yErrorValues, and yErrorType to each dataset.
#'
#' @param outputMappings A list of `PIOutputMapping` objects.
#' @param mappingValues A list of dataset value lists, one per output mapping.
#' @return The updated list of output mappings.
#' @keywords internal
#' @noRd
.applyMappingValues <- function(outputMappings, mappingValues) {
  for (idx in seq_along(outputMappings)) {
    observedDataSets <- outputMappings[[idx]]$observedDataSets
    newValues <- mappingValues[[idx]]

    for (dataSet in observedDataSets) {
      .restoreDataSetValues(
        dataSet,
        newValues[[dataSet$name]]
      )
    }
  }
  return(outputMappings)
}

#' Restore values to a DataSet
#'
#' Replaces x/y/yError values and optionally yErrorType in a dataset.
#'
#' @param dataSet A `DataSet` object.
#' @param values A list containing `xValues`, `yValues`, `yErrorValues`, and
#'   optionally `yErrorType`.
#' @keywords internal
#' @noRd
.restoreDataSetValues <- function(dataSet, values) {
  dataSet$setValues(
    xValues = values$xValues,
    yValues = values$yValues,
    yErrorValues = values$yErrorValues
  )
  if (!is.null(values$yErrorType)) {
    dataSet$yErrorType <- values$yErrorType
  }
}
