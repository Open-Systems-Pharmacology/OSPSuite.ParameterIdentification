# CLASSIFICATION

#' Check if Observed Dataset is Aggregated
#'
#' Determines whether a dataset is aggregated based on the presence
#' of a defined `yErrorType`.
#'
#' @param data A `DataSet` object.
#' @return Logical value indicating if the dataset is aggregated.
#' @keywords internal
#' @noRd
.isAggregated <- function(data) {
  !is.null(data$yErrorType)
}

#' Check if an aggregated data set has error values to resample from
#'
#' @param dataSet A `DataSet` object.
#' @return `TRUE` when `yErrorType` is set and at least one error value is
#'   finite.
#' @keywords internal
#' @noRd
.isAggregatedWithErrors <- function(dataSet) {
  .isAggregated(dataSet) && any(is.finite(dataSet$yErrorValues))
}

#' Validate the subject counts of the output mappings
#'
#' Stops when an output mapping holds an aggregated data set with error values
#' but has no subject count (see `PIOutputMapping$setSubjectCount()`).
#'
#' @param outputMappings A list of `PIOutputMapping` objects.
#' @return The output mappings, invisibly.
#' @keywords internal
#' @noRd
.validateSubjectCounts <- function(outputMappings) {
  missingNames <- character()
  for (mapping in outputMappings) {
    if (is.null(mapping$subjectCount)) {
      aggregated <- Filter(.isAggregatedWithErrors, mapping$observedDataSets)
      missingNames <- c(missingNames, names(aggregated))
    }
  }
  if (length(missingNames) > 0) {
    stop(messages$errorMissingSubjectCount(missingNames))
  }
  invisible(outputMappings)
}

#' Classify Observed Datasets for Bootstrap
#'
#' Evaluates all observed datasets in the output mappings and classifies them as
#' either individual or aggregated based on the presence of `yErrorType`. Prints
#' a status message with the number of individual and aggregated datasets, and
#' warns if too few individual datasets are available for stable bootstrap CI.
#'
#' @param outputMappings A list of `PIOutputMapping` objects to classify.
#' @keywords internal
#' @noRd
.classifyObservedData <- function(outputMappings) {
  observedDataSets <- unlist(
    lapply(outputMappings, function(mapping) mapping$observedDataSets),
    recursive = FALSE
  )

  isAggregated <- vapply(observedDataSets, .isAggregated, logical(1))
  nAggregated <- sum(isAggregated)
  nIndividual <- length(observedDataSets) - nAggregated

  message(messages$statusObservedDataClassification(nIndividual, nAggregated))

  if (nAggregated < 1 && nIndividual < 3) {
    warning(messages$warningLowIndividualData())
  }
}


# RESAMPLING: APPLY STATE

#' Apply resampled data set weights
#'
#' Resamples the data set weights of every output mapping for a bootstrap
#' sample and applies them. The values of the data sets are not changed.
#'
#' @param outputMappings List of `PIOutputMapping` objects.
#' @param mappingState A named list with `dataSetWeights` and `dataSetValues`.
#' @param seed Integer used to drive resampling.
#' @return Updated outputMappings list.
#' @keywords internal
#' @noRd
.resampleAndApplyMappingState <- function(outputMappings, mappingState, seed) {
  ospsuite.utils::validateIsSameLength(
    outputMappings,
    mappingState$dataSetWeights
  )
  .resampleAndApplyMappingWeights(
    outputMappings = outputMappings,
    mappingWeights = mappingState$dataSetWeights,
    seed = seed
  )
}

#' Resample and apply dataset weights
#'
#' Resamples dataset weights based on a bootstrap seed and applies them to each
#' output mapping.
#'
#' @param outputMappings A list of `PIOutputMapping` objects.
#' @param mappingWeights A list of initial dataset weight lists, one per output
#'   mapping.
#' @param seed An integer seed used for bootstrap resampling.
#' @return Updated list of `PIOutputMapping` objects.
#' @keywords internal
#' @noRd
.resampleAndApplyMappingWeights <- function(
  outputMappings,
  mappingWeights,
  seed
) {
  resampledMappingWeights <- .resampleMappingWeights(
    outputMappings,
    mappingWeights,
    seed
  )
  .applyMappingWeights(outputMappings, resampledMappingWeights)
}


# RESAMPLING: WEIGHTS

#' Resample mapping-level dataset weights
#'
#' Resamples dataset weights for each output mapping using a shared bootstrap
#' strategy. Ensures mapping weights align with observed datasets and applies
#' point-level resampling.
#'
#' @param outputMappings A list of `PIOutputMapping` objects.
#' @param mappingWeights A list of named dataset weight lists, one per output
#'   mapping.
#' @param seed Integer used for bootstrap resampling.
#' @return A list of resampled dataset weight lists, one per output mapping.
#' @keywords internal
#' @noRd
.resampleMappingWeights <- function(outputMappings, mappingWeights, seed) {
  ospsuite.utils::validateIsSameLength(mappingWeights, outputMappings)

  resampledMappingWeights <- vector("list", length(outputMappings))

  for (idx in seq_along(outputMappings)) {
    mapping <- outputMappings[[idx]]
    dataSetWeights <- mappingWeights[[idx]]

    if (
      !ospsuite.utils::isSameLength(dataSetWeights, mapping$observedDataSets) ||
        !ospsuite.utils::isIncluded(
          names(dataSetWeights),
          names(mapping$observedDataSets)
        )
    ) {
      stop(messages$errorDataSetWeightsMismatch())
    }

    # All datasets are resampled equally using a single strategy
    resampledMappingWeights[[idx]] <- .resampleDataSetWeights(
      dataSetWeights,
      seed
    )
  }

  return(resampledMappingWeights)
}

#' Resample dataset-level weights using bootstrap
#'
#' Performs bootstrap resampling by adjusting dataset-level weights. Each
#' dataset’s total weight is split into a dataset-level weight and normalized
#' point-level weights. Sampling is performed with replacement, and point
#' weights are scaled accordingly.
#'
#' @param dataSetWeights A named list of numeric weight vectors (one per
#'   dataset).
#' @param seed Integer used to control the resampling.
#' @return A named list of resampled weight vectors.
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

    if (!is.finite(datasetWeight) || datasetWeight <= 0) {
      datasetWeight <- 1
    }

    pointWeights <- weightsVector / datasetWeight
    pointWeights[!is.finite(pointWeights)] <- 0

    weightsSummary[[dataSetName]] <- list(
      datasetWeight = datasetWeight,
      pointWeights = pointWeights
    )
  }

  # Resample dataset-level weights, then recombine with point-level weights
  weightsPool <- unlist(
    mapply(
      rep,
      names(weightsSummary),
      lapply(weightsSummary, `[[`, "datasetWeight")
    ),
    use.names = FALSE
  )

  set.seed(seed)
  resampledNames <- sample(
    weightsPool,
    size = length(weightsPool),
    replace = TRUE
  )
  datasetCounts <- table(resampledNames)

  resampledDataSetWeights <- vector("list", length(dataSetWeights))
  names(resampledDataSetWeights) <- names(dataSetWeights)

  for (dataSetName in names(weightsSummary)) {
    nSelected <- as.integer(datasetCounts[dataSetName])
    if (is.na(nSelected)) {
      nSelected <- 0
    }
    resampledDataSetWeights[[dataSetName]] <-
      weightsSummary[[dataSetName]]$pointWeights * nSelected
  }

  return(resampledDataSetWeights)
}

#' Stabilize bootstrap coefficient of variation by minimal symmetric trimming
#'
#' Dynamically trims extreme values from both ends of a sorted bootstrap
#' distribution to reach the point where the coefficient of variation (CV)
#' stabilizes.
#'
#' @param values Numeric vector of bootstrap estimates.
#' @param maxTrimFraction Maximum fraction of values to trim from each tail.
#'   Default is 0.05.
#'
#' @return Trimmed numeric vector.
#' @keywords internal
#' @noRd
.stabilizeBootstrapCV <- function(values, maxTrimFraction = 0.05) {
  values <- values[is.finite(values)]
  n <- length(values)
  maxTrim <- floor(n * maxTrimFraction)

  if (maxTrim < 1) {
    return(values)
  }

  sorted <- sort(values)
  cvs <- numeric(maxTrim + 1)

  # Compute CV for increasing amounts of symmetric trimming
  for (i in 0:maxTrim) {
    trimmed <- sorted[(i + 1):(n - i)]
    cvs[i + 1] <- stats::sd(trimmed) / abs(mean(trimmed))
  }

  # Find point where CV stops decreasing sharply (plateau starts)
  diffs <- diff(cvs)
  relDiffs <- diffs / abs(cvs[-length(cvs)])
  elbowIndex <- which.min(relDiffs) + 1

  # Trim minimal values needed to reach CV stabilization plateau
  trimCount <- elbowIndex - 1
  sorted[(trimCount + 1):(n - trimCount)]
}
