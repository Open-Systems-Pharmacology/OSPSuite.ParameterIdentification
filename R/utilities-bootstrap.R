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

