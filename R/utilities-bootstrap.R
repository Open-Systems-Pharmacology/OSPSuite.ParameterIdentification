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

  message(messages$statusObservedDataClassification(nIndividual, nAggregated))

  if (nIndividual < 3) {
    warning(messages$warningLowIndividualData())
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

#' Resample Mapping Weights for Bootstrap
#'
#' Resamples dataset weights for each output mapping based on the bootstrap seed.
#' Applies different resampling strategies to individual and aggregated datasets.
#'
#' Individual and aggregated datasets are resampled separately:
#' - Individual datasets are resampled as full datasets, controlled by adjusting
#' dataset-level weights (`.resampleDataSetWeights`).
#' - Aggregated datasets (mean ± error) are resampled by fitting a Gaussian Process
#' Regression (GPR)
#'   model to the mean and error, then generating synthetic data points, controlled
#'   by adjusting point-level weights (`.resampleAggregatedWeights`).
#'
#' @param outputMappings A list of `PIOutputMapping` objects.
#' @param mappingWeights A list of dataset weight lists, one per output mapping.
#' @param seed An integer seed used for bootstrap resampling.
#' @return A list of resampled dataset weights, one per output mapping.
#'
#' @keyword internal
#' @noRd
.resampleMappingWeights <- function(outputMappings, mappingWeights, seed) {
  if (length(mappingWeights) != length(outputMappings)) {
    stop(messages$errorWeightGroupLengthMismatch(
      length(outputMappings), length(mappingWeights)
    ))
  }

  resampledMappingWeights <- vector("list", length(outputMappings))
  names(resampledMappingWeights) <- names(outputMappings)

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
#' @keyword internal
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
