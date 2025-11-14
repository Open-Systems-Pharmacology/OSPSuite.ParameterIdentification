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

#' Check if any aggregated dataset exists
#'
#' Returns TRUE if at least one dataset across all output mappings is
#' aggregated.
#'
#' @param outputMappings A list of `PIOutputMapping` objects.
#' @return Logical scalar.
#' @keywords internal
#' @noRd
.hasAggregatedData <- function(outputMappings) {
  any(vapply(
    outputMappings,
    function(mapping) {
      any(vapply(mapping$observedDataSets, .isAggregated, logical(1)))
    },
    logical(1)
  ))
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


# RESAMPLING: APPLY COMBINED STATE

#' Apply Resampled Dataset Weights and Values
#'
#' Applies both weights and values to each dataset, as stored in the original
#' output mapping state. Aggregated datasets are resampled using GPR models.
#'
#' @param outputMappings List of `PIOutputMapping` objects.
#' @param mappingState A named list with `dataSetWeights` and `dataSetValues`.
#' @param gprModels List of GPR models, one per output mapping.
#' @param seed Integer used to drive resampling.
#' @return Updated outputMappings list.
#' @keywords internal
#' @noRd
.resampleAndApplyMappingState <- function(
  outputMappings,
  mappingState,
  gprModels,
  seed
) {
  ospsuite.utils::validateIsSameLength(
    outputMappings,
    mappingState$dataSetWeights
  )
  ospsuite.utils::validateIsSameLength(
    outputMappings,
    mappingState$dataSetValues
  )
  ospsuite.utils::validateIsSameLength(outputMappings, gprModels)

  # Resample and apply new dataset weights for all mappings.
  # This adjusts the relative influence of datasets.
  outputMappings <- .resampleAndApplyMappingWeights(
    outputMappings = outputMappings,
    mappingWeights = mappingState$dataSetWeights,
    seed = seed
  )

  # Resample and apply synthetic y-values for aggregated datasets using GPR.
  # This replaces mean values with simulated realizations that preserve temporal
  # correlation, thereby reflecting realistic sampling uncertainty in
  # time-series data.
  if (.hasAggregatedData(outputMappings)) {
    outputMappings <- .resampleAndApplyMappingValues(
      outputMappings = outputMappings,
      mappingValues = mappingState$dataSetValues,
      gprModels = gprModels,
      seed = seed
    )
  }

  return(outputMappings)
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

#' Resample and apply dataset values
#'
#' Resamples dataset values (used for aggregated datasets) using GPR models and
#' applies them to each output mapping.
#'
#' @param outputMappings A list of `PIOutputMapping` objects.
#' @param mappingValues A list of original dataset values, one per output
#'   mapping.
#' @param gprModels List of GPR models, one per output mapping.
#' @param seed An integer seed used for bootstrap resampling.
#' @return Updated list of `PIOutputMapping` objects.
#' @keywords internal
#' @noRd
.resampleAndApplyMappingValues <- function(
  outputMappings,
  mappingValues,
  gprModels,
  seed
) {
  resampledMappingValues <- .resampleMappingValues(
    outputMappings,
    mappingValues,
    gprModels,
    seed
  )
  .applyMappingValues(outputMappings, resampledMappingValues)
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


# RESAMPLING: VALUES

#' Resample mapping-level dataset values using GPR
#'
#' Resamples dataset values for each output mapping. Aggregated datasets are
#' simulated from GPR models; others are returned unchanged.
#'
#' @param outputMappings A list of `PIOutputMapping` objects.
#' @param mappingValues A list of dataset value lists, one per output mapping.
#' @param gprModelMappings A list of GPR model lists, one per output mapping.
#' @param seed An integer seed for reproducible simulation.
#' @return A list of resampled dataset value lists, one per output mapping.
#' @keywords internal
#' @noRd
.resampleMappingValues <- function(
  outputMappings,
  mappingValues,
  gprModelMappings,
  seed
) {
  resampledMappingValues <- vector("list", length(outputMappings))

  for (idx in seq_along(outputMappings)) {
    mapping <- outputMappings[[idx]]
    observedDataSets <- mapping$observedDataSets
    dataSetValues <- mappingValues[[idx]]
    gprModels <- gprModelMappings[[idx]]

    observedDataSetsAggr <- observedDataSets[
      vapply(observedDataSets, .isAggregated, logical(1))
    ]

    if (
      !ospsuite.utils::isSameLength(dataSetValues, observedDataSets) ||
        !ospsuite.utils::isSameLength(gprModels, observedDataSetsAggr) ||
        !ospsuite.utils::isIncluded(
          names(observedDataSets),
          names(dataSetValues)
        ) ||
        !ospsuite.utils::isIncluded(names(gprModels), names(observedDataSets))
    ) {
      stop(messages$errorDataSetValuesMismatch())
    }

    #
    resampledMappingValues[[idx]] <- .resampleDataSetValues(
      dataSetValues,
      gprModels,
      seed
    )
  }

  return(resampledMappingValues)
}

#' Resample dataset-level values using GPR
#'
#' Resamples synthetic y-values for each dataset using a fitted GPR model.
#' Datasets without a GPR model are returned unchanged.
#'
#' @param dataSetValues A named list of dataset value structures.
#' @param gprModels A named list of GPR models (or NULL for non-aggregated
#'   datasets).
#' @param seed An integer seed for reproducibility.
#' @return A named list of dataset value structures with updated y-values.
#' @keywords internal
#' @noRd
.resampleDataSetValues <- function(dataSetValues, gprModels, seed) {
  valueList <- vector("list", length(dataSetValues))
  names(valueList) <- names(dataSetValues)

  for (dataSetName in names(dataSetValues)) {
    gprModel <- gprModels[[dataSetName]]

    if (is.null(gprModel)) {
      valueList[[dataSetName]] <- dataSetValues[[dataSetName]]
      next
    }

    xValues <- dataSetValues[[dataSetName]]$xValues

    # Simulate synthetic datasets in log-space
    logPredictedValues <- DiceKriging::simulate(
      object = gprModel,
      nsim = 1,
      seed = seed,
      newdata = data.frame(time = xValues),
      cond = TRUE,
      nugget.sim = 1e-6
    )

    # Model was fit with log values - convert back to original scale
    syntheticValues <- exp(as.numeric(logPredictedValues))

    valueList[[dataSetName]] <- list(
      xValues = xValues,
      yValues = syntheticValues,
      yErrorValues = dataSetValues[[dataSetName]]$yErrorValues,
      yErrorType = dataSetValues[[dataSetName]]$yErrorType
    )
  }

  return(valueList)
}


# GPR MODELING
#
# Aggregated datasets contain summary statistics (e.g., means and SD/GSD)
# rather than individual-level measurements. During bootstrap CI estimation,
# we must generate synthetic data to reflect sampling variability.
#
# However, we cannot resample directly from single aggregated points, since
# they are temporally correlated (e.g., a concentration-time profile).
#
# To address this, we use Gaussian Process Regression (GPR) to simulate
# smooth, realistic trajectories consistent with the observed mean trend
# and uncertainty. This provides synthetic individual-level realizations
# while preserving time correlations and measurement error structure.

#' Fit GPR models for aggregated datasets
#'
#' Applies `.fitGPRModel` to each aggregated dataset in each output mapping.
#'
#' @param outputMappings A list of `PIOutputMapping` objects.
#' @return A nested list of fitted GPR models (or NULL for non-aggregated
#'   datasets).
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
#' Aggregated datasets provide only summary statistics (mean and variability),
#' without individual observations. Because measurements are time-correlated,
#' simple pointwise sampling is inappropriate. This function fits a GPR model to
#' log-transformed y-values and their uncertainty to enable the simulation of
#' smooth, realistic trajectories during bootstrap resampling.
#'
#' Handles both arithmetic and geometric standard deviations. If model fitting
#' fails due to extreme variance, progressively capped versions of the noise
#' variance are attempted using quantile-based thresholds.
#'
#' @param xValues Numeric vector of x values (e.g., time).
#' @param yValues Numeric vector of y values (means).
#' @param yErrorValues Numeric vector of error values.
#' @param yErrorType Character; either `GeometricStdDev` or `ArithmeticStdDev`.
#' @param kernelType Character; kernel type for GPR. Default is "matern5_2".
#' @param minProb Numeric between 0 and 1. Lowest quantile used for capping
#'   noise variance. Default is 0.5.
#' @return A fitted `DiceKriging::km` object.
#' @keywords internal
#' @noRd
.fitGPRModel <- function(
  xValues,
  yValues,
  yErrorValues,
  yErrorType,
  kernelType = "matern5_2",
  minProb = 0.5
) {
  ospsuite.utils::validateIsSameLength(xValues, yValues)
  ospsuite.utils::validateIsSameLength(yValues, yErrorValues)

  xValues[xValues < 0] <- 0

  # Compute noise variance in log-space based on error type
  noiseVar <- switch(
    yErrorType,
    ArithmeticStdDev = {
      # For arithmetic SD: Var[log(y)] ≈ (σ / y)^2 using the delta method
      # Missing or non-finite relative errors are imputed using the mean
      # relative error
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
    stop(messages$errorUnsupportedErrorType())
  )

  # `DiceKriging::km` may fail to converge with extreme noise values (e.g., at
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
  invisible(utils::capture.output(
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
    cvs[i + 1] <- sd(trimmed) / abs(mean(trimmed))
  }

  # Find point where CV stops decreasing sharply (plateau starts)
  diffs <- diff(cvs)
  relDiffs <- diffs / abs(cvs[-length(cvs)])
  elbowIndex <- which.min(relDiffs) + 1

  # Trim minimal values needed to reach CV stabilization plateau
  trimCount <- elbowIndex - 1
  sorted[(trimCount + 1):(n - trimCount)]
}
