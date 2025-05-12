#' Check if Observed Dataset is Aggregated
#'
#' Determines whether a dataset is aggregated based on the presence
#' of a defined `yErrorType`.
#'
#' @param data A `DataSet` (typically `XYData`) object.
#' @return Logical value indicating if the dataset is aggregated.
#'
#' @keywords internal
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
#' @keywords internal
#' @noRd
.classifyObservedData <- function(outputMappings) {
  observedDataSets <- unlist(
    lapply(outputMappings, function(mapping) mapping$observedDataSets),
    recursive = FALSE
  )

  isAggregated <- vapply(observedDataSets, .isAggregated, logical(1))
  nAggregated <- sum(isAggregated)
  nIndividual <- length(dataSets) - nAggregated

  message(messages$statusObservedDataClassification(nIndividual, nAggregated))

  if (nIndividual < 3) {
    warning(messages$warningLowIndividualData())
  }
}

#'
#'
#' @keywords internal
#' @noRd


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
#' @keywords internal
#' @noRd
.resampleAndApplyMappingWeights <- function(outputMappings, mappingWeights, seed) {
  resampledMappingWeights <- .resampleMappingWeights(outputMappings, mappingWeights, seed)
  .applyMappingWeights(outputMappings, resampledMappingWeights)
}

#'
#'
#'
#' @param outputMappings A list of `PIOutputMapping` objects.
#' @param mappingWeights A list of dataset weight lists, one per output mapping.
#' @param seed An integer seed used for bootstrap resampling.
#' @return A list of resampled dataset weights, one per output mapping.
#'
#' @keywords internal
#' @noRd
.resampleMappingWeights <- function(outputMappings, mappingWeights, seed) {

  resampledMappingWeights <- vector("list", length(outputMappings))

  for (idx in seq_along(outputMappings)) {
    mapping <- outputMappings[[idx]]
    dataSetWeights <- mappingWeights[[idx]]

    isAggregated <- vapply(mapping$observedDataSets, .isAggregated, logical(1))

    if (
      !ospsuite.utils::isSameLength(dataSetWeights, mapping$observedDataSets) ||
        !ospsuite.utils::isIncluded(names(dataSetWeights), names(mapping$observedDataSets))
    ) {
      stop(messages$errorDataSetWeightsMismatch())
    }

    # Generate resampled weights for individual and aggregated data sets
    individualWeights <- dataSetWeights[!isAggregated]
    aggregatedWeights <- dataSetWeights[isAggregated]

    resampledDataSetWeights <- list()

    if (length(individualWeights) > 0) {
      sampledIndividualWeights <- .resampleDataSetWeights(individualWeights, seed)
      individualNames <- names(sampledIndividualWeights)
      resampledDataSetWeights[individualNames] <- sampledIndividualWeights
    }

    if (length(aggregatedWeights) > 0) {
      stop("bootstrap with aggregated data not supported yet")
      # Later:
      # sampledAggregatedWeights <- .resampleAggregatedWeights(aggregatedWeights, seed)
      # aggregatedNames <- names(sampledAggregatedWeights)
      # resampledDataSetWeights[aggregatedNames] <- sampledAggregatedWeights
    }

    resampledMappingWeights[[idx]] <- resampledDataSetWeights
  }

  return(resampledMappingWeights)
}

#' Resample Weights for Individual Datasets
#'
#' Performs bootstrap resampling of individual datasets by adjusting their dataset-level
#' weights. Each dataset's total weight is broken down into dataset-level and point-level
#' components. The dataset-level weights are resampled with replacement and then
#' recombined with the point-level weights to generate new weights.
#'
#' @param dataSetWeights A named list of numeric weight vectors (one per dataset).
#' @param seed An integer seed used to control the resampling.
#' @return A named list of resampled weight vectors, matching the structure of `dataSetWeights`.
#'
#' @keywords internal
#' @noRd
.resampleDataSetWeights <- function(dataSetWeights, seed) {
  ospsuite.utils::validateIsNotEmpty(dataSetWeights)

  weightsSummary <- vector("list", length(dataSetWeights))
  names(weightsSummary) <- names(dataSetWeights)

  # Decompose each dataset's weights into dataset-level and point-level components
  for (dataSetName in names(dataSetWeights)) {
    weightsVector <- dataSetWeights[[dataSetName]]
    weightsVector[is.na(weightsVector)] <- 0

    datasetWeight <- min(weightsVector[weightsVector >= 1], na.rm = TRUE)

    pointWeights <- weightsVector / datasetWeight
    pointWeights[!is.finite(pointWeights)] <- 0

    weightsSummary[[dataSetName]] <- list(
      datasetWeight = datasetWeight,
      pointWeights = pointWeights
    )
  }

  # Resample dataset-level weights, then recombine with point-level weights
  weightsPool <- unlist(
    mapply(rep, names(weightsSummary), lapply(weightsSummary, `[[`, "datasetWeight")),
    use.names = FALSE
  )

  set.seed(seed)
  resampledNames <- sample(weightsPool, size = length(weightsPool), replace = TRUE)
  datasetCounts <- table(resampledNames)

  resampledDataSetWeights <- vector("list", length(dataSetWeights))
  names(resampledDataSetWeights) <- names(dataSetWeights)

  for (dataSetName in names(weightsSummary)) {
    nSelected <- as.integer(datasetCounts[dataSetName])
    if (is.na(nSelected)) nSelected <- 0
    resampledDataSetWeights[[dataSetName]] <-
      weightsSummary[[dataSetName]]$pointWeights * nSelected
  }

  return(resampledDataSetWeights)
}
