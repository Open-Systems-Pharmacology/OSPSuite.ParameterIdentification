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
#' @return `TRUE` when `yErrorType` is set and at least one point can be
#'   redrawn (see `.redrawablePoints()`).
#' @keywords internal
#' @noRd
.isAggregatedWithErrors <- function(dataSet) {
  .isAggregated(dataSet) &&
    any(.redrawablePoints(
      dataSet$yValues,
      dataSet$yErrorValues,
      dataSet$yErrorType
    ))
}

#' Points of an aggregated data set whose means can be redrawn
#'
#' A point can be redrawn when its mean is positive and its error is an SD
#' above 0 or, for `GeometricStdDev`, a GSD above 1. The sign of the error
#' does not depend on its unit.
#'
#' @param yValues,yErrorValues The means and errors of the data set.
#' @param yErrorType The `yErrorType` of the data set.
#' @return A logical vector, one entry per point.
#' @keywords internal
#' @noRd
.redrawablePoints <- function(yValues, yErrorValues, yErrorType) {
  minError <- if (identical(yErrorType, "GeometricStdDev")) 1 else 0
  is.finite(yErrorValues) &
    yErrorValues > minError &
    is.finite(yValues) &
    yValues > 0
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
#' either individual or aggregated (a `yErrorType` with error values, see
#' `.isAggregatedWithErrors()`). A data set with a `yErrorType` but no error
#' values counts as individual and gets a message. Prints a status message with
#' the number of individual and aggregated datasets, and warns if too few
#' individual datasets are available for stable bootstrap CI.
#'
#' @param outputMappings A list of `PIOutputMapping` objects to classify.
#' @keywords internal
#' @noRd
.classifyObservedData <- function(outputMappings) {
  observedDataSets <- unlist(
    lapply(outputMappings, function(mapping) mapping$observedDataSets),
    recursive = FALSE
  )

  isAggregated <- vapply(observedDataSets, .isAggregatedWithErrors, logical(1))
  withoutErrors <- vapply(observedDataSets, .isAggregated, logical(1)) &
    !isAggregated
  for (dataSet in observedDataSets[withoutErrors]) {
    message(messages$statusAggregatedWithoutErrors(dataSet$name))
  }
  nAggregated <- sum(isAggregated)
  nIndividual <- length(observedDataSets) - nAggregated

  message(messages$statusObservedDataClassification(nIndividual, nAggregated))

  if (nAggregated < 1 && nIndividual < 3) {
    warning(messages$warningLowIndividualData())
  }
}


# RESAMPLING: SEED

#' Evaluate an expression under a seed without touching the global stream
#'
#' @param seed Integer seed for `set.seed()`.
#' @param expr Expression to evaluate.
#' @return The value of `expr`.
#' @keywords internal
#' @noRd
.withSeed <- function(seed, expr) {
  hadSeed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  savedSeed <- if (hadSeed) get(".Random.seed", envir = globalenv())
  on.exit(
    if (hadSeed) {
      assign(".Random.seed", savedSeed, envir = globalenv())
    } else {
      rm(".Random.seed", envir = globalenv())
    },
    add = TRUE
  )
  set.seed(seed)
  expr
}


# RESAMPLING: WEIGHTS

#' Resample dataset-level weights using bootstrap
#'
#' Performs bootstrap resampling by adjusting dataset-level weights. Each
#' dataset’s total weight is split into a dataset-level weight and normalized
#' point-level weights. Data sets are drawn with replacement, a data set with
#' the level weight `w`, the smallest point weight of at least 1 rounded down,
#' entering the pool `w` times. The weights of a data set drawn `k` times
#' become its original weights times `sqrt(k / w)`: the cost squares the
#' weighted residuals, so the data set then counts `k / w` times its usual
#' share, and at the expected pick count `k = w` a replicate keeps the user's
#' weights. Draws come from the current random stream, which the
#' caller seeds (see `.withSeed()`).
#'
#' @param dataSetWeights A named list of numeric weight vectors (one per
#'   dataset).
#' @return A named list of resampled weight vectors.
#' @keywords internal
#' @noRd
.resampleDataSetWeights <- function(dataSetWeights) {
  ospsuite.utils::validateIsNotEmpty(dataSetWeights)

  weightsSummary <- vector("list", length(dataSetWeights))
  names(weightsSummary) <- names(dataSetWeights)

  # Decompose each dataset's weights into dataset-level and point-level components
  for (dataSetName in names(dataSetWeights)) {
    weightsVector <- dataSetWeights[[dataSetName]]
    weightsVector[is.na(weightsVector)] <- 0

    wholeWeights <- weightsVector[is.finite(weightsVector) & weightsVector >= 1]
    # Whole number, so the pool multiplicity and the scaling below agree
    datasetWeight <- if (length(wholeWeights) > 0) {
      floor(min(wholeWeights))
    } else {
      1
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
    summary <- weightsSummary[[dataSetName]]
    resampledDataSetWeights[[dataSetName]] <-
      summary$pointWeights * sqrt(nSelected * summary$datasetWeight)
  }

  return(resampledDataSetWeights)
}

# RESAMPLING: VALUES

#' Redraw the means of an aggregated data set
#'
#' A parametric bootstrap of the reported means of a data set with
#' `yErrorType`, for `n` subjects. With independent standard normal draws
#' `e_j` per time point from the current random stream (see `.withSeed()`):
#'
#' - `GeometricStdDev`, geometric mean `G_j` and `GSD_j`:
#'   `log(y_j*) = log(G_j) + log(GSD_j) / sqrt(n) * e_j`, the sampling
#'   distribution of the mean of the logs.
#' - `ArithmeticStdDev`, arithmetic mean `mu_j` and SD `sigma_j`:
#'   `log(y_j*) = m_j + s_j * e_j` with `s_j^2 = log(1 + (sigma_j / mu_j)^2 / n)`
#'   and `m_j = log(mu_j) - s_j^2 / 2`, the lognormal with exactly the mean
#'   `mu_j` and the SD `sigma_j / sqrt(n)` of the mean of `n` subjects. The
#'   `- s_j^2 / 2` keeps the mean of the draws at `mu_j`. Without it the draws
#'   would be centred on the median, below `mu_j`.
#'
#' A point with a non-finite error, a non-positive mean or a `GSD` not above 1
#' keeps its value. A data set without `yErrorType` or without any finite
#' error value is returned unchanged.
#'
#' @param dataSetValues A list with `xValues`, `yValues`, `yErrorValues` and
#'   `yErrorType` of one data set.
#' @param subjectCount The number of subjects `n` behind the data set.
#' @param yErrorValues The error values in the unit of `yValues`.
#' @return `dataSetValues` with new `yValues`.
#' @keywords internal
#' @noRd
.resampleDataSetValues <- function(
  dataSetValues,
  subjectCount,
  yErrorValues = dataSetValues$yErrorValues
) {
  errorType <- dataSetValues$yErrorType
  y <- dataSetValues$yValues
  err <- yErrorValues
  if (is.null(errorType) || !any(is.finite(err))) {
    return(dataSetValues)
  }
  e <- stats::rnorm(length(y))
  redraw <- .redrawablePoints(y, err, errorType)
  if (errorType == "GeometricStdDev") {
    logSd <- log(err[redraw]) / sqrt(subjectCount)
    y[redraw] <- exp(log(y[redraw]) + logSd * e[redraw])
  } else {
    s2 <- log(1 + (err[redraw] / y[redraw])^2 / subjectCount)
    y[redraw] <- exp(log(y[redraw]) - s2 / 2 + sqrt(s2) * e[redraw])
  }
  dataSetValues$yValues <- y
  dataSetValues
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
