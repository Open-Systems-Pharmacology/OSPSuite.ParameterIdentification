#' Check if Observed Dataset is Aggregated
#'
#' Determines whether a dataset is aggregated based on the presence
#' of a defined `yErrorType`.
#'
#' @param data A `DataSet` (typically `XYData`) object.
#' @return Logical value indicating if the dataset is aggregated.
#'
#' @keyword internal
#' @noRd
.isAggregated <- function(data) {
  !is.null(data$yErrorType)
}

#' Classify Observed Datasets for Bootstrap
#'
#' Evaluates all observed datasets in the output mappings and classifies them
#' as either individual or aggregated based on the presence of `yErrorType`.
#' Prints a status message with the number of individual and aggregated datasets,
#' and warns if too few individual datasets are available for stable bootstrap CI.
#'
#' @param outputMappings A list of `PIOutputMapping` objects to classify.
#'
#' @keyword internal
#' @noRd
.classifyObservedData <- function(outputMappings) {
  dataSets <- unlist(
    lapply(outputMappings, function(mapping) mapping$observedDataSets),
    recursive = FALSE
  )

  isAggregated <- vapply(dataSets, .isAggregated, logical(1))
  nAggregated <- sum(isAggregated)
  nIndividual <- length(dataSets) - nAggregated

  message(sprintf("PI detected %d individual and %d aggregated datasets.", nIndividual, nAggregated))

  if (nIndividual < 3) {
    warning("Fewer than 3 individual datasets detected — bootstrap CI may be unreliable.")
  }
}

#' Extract Initial Mapping Weights
#'
#' Extracts dataset weights from each output mapping. If weights are missing,
#' they are initialized to 1 for each observed y-value. The result is a list
#' of named lists, one per output mapping, containing weights for each dataset.
#'
#' @param outputMappings A list of `PIOutputMapping` objects.
#' @return A named list of dataset weight lists, one per output mapping.
#'
#' @keyword internal
#' @noRd
.extractMappingWeights <- function(outputMappings) {
  mappingWeights <- vector("list", length(outputMappings))
  names(mappingWeights) <- names(outputMappings)

  for (idx in seq_along(outputMappings)) {
    mapping <- outputMappings[[idx]]
    currentWeights <- mapping$dataWeights
    observedDataSets <- mapping$observedDataSets

    mappingWeights[[idx]] <- list()
    for (dataSetName in names(observedDataSets)) {
      mappingWeights[[idx]][[dataSetName]] <- if (!is.null(currentWeights[[dataSetName]])) {
        currentWeights[[dataSetName]]
      } else {
        rep(1, length(observedDataSets[[dataSetName]]$yValues))
      }
    }
  }

  return(mappingWeights)
}

#' Apply DataSet Weights to Output Mappings
#'
#' Assigns dataset weights to each output mapping using the mapping-level
#' weight structure. Assumes that weight structure matches the observed datasets.
#'
#' @param outputMappings A list of `PIOutputMapping` objects.
#' @param mappingWeights A list of dataset weight lists, one per output mapping.
#' @return The updated list of `PIOutputMapping` objects with applied weights.
#'
#' @keyword internal
#' @noRd
.applyMappingWeights <- function(outputMappings, mappingWeights) {
  for (idx in seq_along(outputMappings)) {
    outputMappings[[idx]]$setDataWeights(mappingWeights[[idx]])
  }
  return(outputMappings)
}

#' Resample and Apply Weights to Output Mappings
#'
#' Resamples dataset weights based on a bootstrap seed and applies the resampled
#' weights to each output mapping.
#'
#' @param outputMappings A list of `PIOutputMapping` objects.
#' @param mappingWeights A list of initial dataset weight lists, one per output mapping.
#' @param seed An integer seed used for bootstrap resampling.
#' @return The updated list of `PIOutputMapping` objects with resampled weights.
#'
#' @keyword internal
#' @noRd
.resampleAndApplyMappingWeights <- function(outputMappings, mappingWeights, seed) {
  resampledMappingWeights <- .resampleMappingWeights(outputMappings, mappingWeights, seed)
  .applyMappingWeights(outputMappings, resampledMappingWeights)
}

