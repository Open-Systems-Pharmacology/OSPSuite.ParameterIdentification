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

#' Fit GPR models for aggregated datasets
#'
#' Applies .fitGPRModel to each aggregated dataset in each output mapping.
#'
#' @param outputMappings A list of `PIOutputMapping` objects.
#' @return A nested list of fitted GPR models (or NULL for non-aggregated datasets).
#'
#' @keywords internal
#' @noRd
.prepareGPRModels <- function(outputMappings) {
  gprModels <- vector("list", length(outputMappings))

  for (idx in seq_along(outputMappings)) {
    mapping <- outputMappings[[idx]]
    dataSets <- mapping$observedDataSets

    dataSetModels <- vector("list", length(dataSets))
    names(dataSetModels) <- names(dataSets)

    for (dataSet in dataSets) {
      if (.isAggregated(dataSet)) {
        fit <- .fitGPRModel(
          xValues = dataSet$xValues,
          yValues = dataSet$yValues,
          yErrorValues = dataSet$yErrorValues,
          yErrorType = dataSet$yErrorType
        )

        if (is.null(fit)) {
          stop(messages$errorGPRModelConvergence(dataSet$name))
        }

        message(messages$statusGPRModelFitted(dataSet$name))
        dataSetModels[[dataSet$name]] <- fit
      } else {
        dataSetModels[[dataSet$name]] <- NULL
      }
    }

    gprModels[[idx]] <- dataSetModels
  }

  return(gprModels)
}

#' Fit Gaussian Process Regression (GPR) Model to Aggregated Data
#'
#' Fits a GPR model to log-transformed y-values with error-based noise variance.
#' Handles both arithmetic and geometric standard deviations. If model fitting
#' fails due to extreme variance, progressively capped versions of the noise
#' variance are attempted using quantile-based thresholds.
#'
#' @param xValues Numeric vector of x values (e.g., time).
#' @param yValues Numeric vector of y values (means).
#' @param yErrorValues Numeric vector of error values.
#' @param yErrorType Character; either "GeometricStdDev" or "ArithmeticStdDev".
#' @param kernelType Character; kernel type for GPR (default is "matern5_2").
#' @param minProb Numeric between 0 and 1. Lowest quantile used for capping noise
#' variance. Default is 0.5.
#' @return A fitted `DiceKriging::km` object.
#'
#' @keywords internal
#' @noRd
.fitGPRModel <- function(xValues, yValues, yErrorValues, yErrorType,
                         kernelType = "matern5_2", minProb = 0.5) {
  ospsuite.utils::validateIsSameLength(xValues, yValues)
  ospsuite.utils::validateIsSameLength(yValues, yErrorValues)

  xValues[xValues < 0] <- 0

  # Compute noise variance in log-space based on error type
  noiseVar <- switch(yErrorType,
    ArithmeticStdDev = {
      # For arithmetic SD: Var[log(y)] ≈ (σ / y)^2 using the delta method
      # Missing or non-finite relative errors are imputed using the mean relative error
      relError <- yErrorValues / yValues
      relError[!is.finite(relError)] <- NA
      relError[is.na(relError)] <- mean(relError, na.rm = TRUE)
      relError^2
    },
    GeometricStdDev = {
      # For geometric SD: Var[log(y)] = log(GSD)^2 (log-normal assumption)
      # Missing or non-finite GSDs are imputed using the geometric mean
      gsd <- yErrorValues
      gsd[!is.finite(gsd)] <- NA
      gsd[is.na(gsd)] <- exp(mean(log(gsd[!is.na(gsd)]), na.rm = TRUE))
      ospsuite.utils::logSafe(gsd)^2
    },
    stop("Unsupported yErrorType: must be 'GeometricStdDev' or 'ArithmeticStdDev'.")
  )

  # DiceKriging::km may fail to converge with extreme noise values (e.g., at
  # late timepoints). Progressively capped versions of the noise variance are
  # generated using quantile-based thresholds.
  quantiles <- seq(1, minProb, by = -0.05)
  cappedVariants <- lapply(quantiles, function(q) {
    cap <- quantile(noiseVar, probs = q, na.rm = TRUE)
    pmin(noiseVar, cap)
  })

  logY <- ospsuite.utils::logSafe(yValues)

  .safeKm <- purrr::safely(.fitKm)

  # Fit with each capped noise variant
  fits <- purrr::map(
    cappedVariants,
    ~ .safeKm(
      xValues = xValues,
      logY = logY,
      noiseVar = .x,
      kernelType = kernelType
    )
  )

  fit <- purrr::detect(fits, ~ is.null(.x$error))

  return(fit$result)
}

#' Fit GPR model (low-level)
#'
#' Wrapper around `DiceKriging::km()` for log-scale GPR fitting.
#'
#' @keywords internal
#' @noRd
.fitKm <- function(xValues, logY, noiseVar, kernelType) {
  invisible(capture.output(
    result <- DiceKriging::km(
      formula = ~1,
      design = data.frame(time = xValues),
      response = data.frame(logY = logY),
      noise.var = noiseVar,
      covtype = kernelType,
      nugget.estim = FALSE
    )
  ))
  return(result)
}
