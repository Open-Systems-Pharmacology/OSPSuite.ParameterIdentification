#' @title Calculate Cost Metrics for Model Evaluation
#'
#' @description Internal utility to calculate the residual-based cost metrics
#' of one output mapping from a data frame of simulated and observed data, for
#' example that of a `DataCombined` object. It does not apply the LLOQ rule of
#' the objective function. The objective function calculates the cost on
#' numeric vectors with `.mappingCostTerms()`. Both use `.costKernel()`, and
#' the tests compare both by `identical()` with a reference implementation on
#' data frames, the objective function for the data on which the reference
#' handles the LLOQ alike (see
#' `tests/testthat/helper-frozen-objective-function.R`).
#'
#' @param df A dataframe containing the combined data for simulation and
#'   observation. Supports dataframes created from a `DataCombined` object via
#'   `$toDataFrame()`. Must include columns for `dataType`, `xValues`,
#'   `yValues`, and optionally `yErrorValues` and `yErrorType` if
#'   `residualWeightingMethod = "error"`. The error type must be one of
#'   `"ArithmeticStdDev"`, `"GeometricStdDev"`.
#' @param objectiveFunctionType A string indicating the objective function type
#'   for calculating model cost. Options include `"lsq"` (least squares,
#'   default) and `"m3"` for handling censored data.
#' @param residualWeightingMethod A string indicating the method to weight the
#'   residuals. Options include `"none"` (default) and `"error"`.
#' @param robustMethod A string indicating the robust method to apply to the
#'   residuals. Options include `"none"` (default), `"huber"`, and `"bisquare"`.
#' @param scaleVar A boolean indicating whether to scale residuals by the number
#'   of observations. Defaults to `FALSE`.
#' @param index Output-mapping index stored on every `residualDetails` row.
#'   Defaults to `NA_real_`.
#' @param ... Additional arguments passed to `.calculateCensoredContribution`,
#'   including `scaling`, `linScaleCV`, and `logScaleSD`.
#'
#' @details The function calculates the residuals between the simulated and
#' observed values, applies the specified weighting method, and computes the
#' cost metrics.
#'
#' @return A cost metrics summary list containing the following fields:
#' - `modelCost`: The total cost calculated from the scaled sum of squared residuals.
#' - `minLogProbability`: The minimum log probability indicating the model fit.
#' - `costVariables`: A dataframe with details on the cost calculations.
#' - `residualDetails`: A dataframe with the calculated residuals and their weights.
#' The summary has the class `modelCost`.
#'
#' @examples
#' \dontrun{
#'
#' # Assuming DataCombined is a valid ospsuite DataCombined object
#' df <- DataCombined$toDataFrame()
#'
#' # Calculate cost metrics
#' costMetrics <- .calculateCostMetrics(df, residualWeightingMethod = "error", scaleVar = TRUE)
#'
#' # View model cost
#' print(costMetrics$modelCost)
#' }
#'
#' @keywords internal
#' @noRd
.calculateCostMetrics <- function(
  df,
  objectiveFunctionType = "lsq",
  residualWeightingMethod = "none",
  robustMethod = "none",
  scaleVar = FALSE,
  index = NA_real_,
  ...
) {
  additionalArgs <- list(...)

  # Validate input dataframe structure
  ospsuite.utils::validateIsOfType(df, "tbl_df")
  ospsuite.utils::validateIsIncluded(
    c(
      "dataType",
      "xDimension",
      "yDimension",
      "xValues",
      "yValues",
      "xUnit",
      "yUnit"
    ),
    colnames(df)
  )
  ospsuite.utils::validateIsOfLength(unique(df$xUnit), 1)
  ospsuite.utils::validateIsOfLength(unique(df$yUnit), 1)
  ospsuite.utils::validateIsIncluded(unique(df$xDimension), "Time")

  # Ensure methods are recognized
  ospsuite.utils::validateEnumValue(
    residualWeightingMethod,
    residualWeightingOptions
  )
  if (residualWeightingMethod == "error") {
    ospsuite.utils::validateIsIncluded(
      c("yErrorValues", "yErrorUnit", "yErrorType"),
      colnames(df)
    )
  }
  ospsuite.utils::validateEnumValue(robustMethod, robustMethodOptions)

  # Handle infinite values and negative times
  df <- df[.finiteValues(df$xValues, df$yValues), ]

  # Splitting dataframe into simulated and observed data
  simulatedData <- df[df$dataType == "simulated", ]
  simulatedData <- simulatedData[!is.na(simulatedData$yValues), ]
  observedData <- df[df$dataType == "observed", ]
  observedData <- observedData[!is.na(observedData$yValues), ]

  # Ensuring there is enough data to perform calculations
  if (NROW(simulatedData) < 1 | is.null(simulatedData)) {
    stop(messages$errorNoDataForCost("simulated"))
  }
  if (NROW(observedData) < 1 | is.null(observedData)) {
    stop(messages$errorNoDataForCost("observed"))
  }

  # Applying M3 method for censored error calculation
  censoredContribution <- 0
  if (objectiveFunctionType == "m3") {
    censoredContribution <- .calculateCensoredContribution(
      observed = observedData,
      simulated = simulatedData,
      scaling = additionalArgs$scaling,
      linScaleCV = additionalArgs$linScaleCV %||% NULL,
      logScaleSD = additionalArgs$logScaleSD %||% NULL
    )
  }

  costTerms <- .costKernel(
    simulatedYApprox = .simulatedAtObservedTimes(
      simulatedData[["xValues"]],
      simulatedData[["yValues"]],
      observedData[["xValues"]]
    ),
    observedX = observedData[["xValues"]],
    observedY = observedData[["yValues"]],
    userWeights = observedData$weights,
    yErrorValues = observedData[["yErrorValues"]],
    yErrorType = observedData[["yErrorType"]],
    residualWeightingMethod = residualWeightingMethod,
    robustMethod = robustMethod,
    scaleVar = scaleVar,
    censoredContribution = censoredContribution,
    index = index
  )

  do.call(.newModelCost, costTerms)
}

#' Cost terms of one output mapping
#'
#' @description Calculates the residuals, their weights and the cost of one
#'   output mapping from the simulated values at the observed times. Shared by
#'   `.calculateCostMetrics()` and the objective function. The observed values
#'   must be finite, with times of at least zero.
#'
#' @param simulatedYApprox The simulated values at the observed times (see
#'   `.simulatedAtObservedTimes()`).
#' @param observedX,observedY Observed times and values.
#' @param referenceY The values the simulated values are compared with, the
#'   observed values by default. The LLOQ rule of the objective function
#'   changes them for the values below the LLOQ (see `.mappingCostTerms()`).
#'   The error weights and `yObserved` stay based on `observedY`.
#' @param userWeights Data weights of the observations, `NA` for none.
#' @param yErrorValues,yErrorType Error values and error types of the
#'   observations, used when `residualWeightingMethod` is `"error"`.
#' @param residualWeightingMethod,robustMethod,scaleVar Options of the
#'   objective function, see `.calculateCostMetrics()`.
#' @param censoredContribution Contribution of the censored observations
#'   (M3 method), 0 otherwise.
#' @param index Output-mapping index stored on every `residualDetails` row.
#'
#' @return A list of the arguments of `.newModelCost()`. When the model cost
#'   is `NA`, a warning and the terms of `.createErrorCostStructure()`.
#' @keywords internal
#' @noRd
.costKernel <- function(
  simulatedYApprox,
  observedX,
  observedY,
  userWeights,
  yErrorValues,
  yErrorType,
  residualWeightingMethod,
  robustMethod,
  scaleVar,
  censoredContribution,
  index,
  referenceY = observedY
) {
  # Calculate raw residuals
  rawResiduals <- simulatedYApprox - referenceY

  # Scaling residuals by the number of observations if requested
  scaleFactor <- if (scaleVar) 1 / length(observedY) else 1
  normalizedResiduals <- rawResiduals * scaleFactor

  # Compute user-defined weights if available
  userWeights[is.na(userWeights)] <- 1

  # Determining the method for residual weighting
  errorWeights <-
    switch(
      residualWeightingMethod,
      "none" = 1,
      "error" = .computeErrorWeights(
        yValues = observedY,
        yErrorValues = yErrorValues,
        yErrorType = yErrorType
      )
    )

  # Calculate robust weights based on the specified robust method
  robustWeights <- switch(
    robustMethod,
    "huber" = .calculateHuberWeights(normalizedResiduals),
    "bisquare" = .calculateBisquareWeights(normalizedResiduals),
    rep(1, length(normalizedResiduals))
  )

  # Weight and organizing residuals
  totalWeights <- errorWeights * userWeights * robustWeights
  weightedResiduals <- normalizedResiduals * totalWeights

  weightedSSR <- sum(weightedResiduals^2)

  # Calculating log probability to evaluate model fit
  logProbability <- -sum(stats::dnorm(
    simulatedYApprox,
    referenceY,
    1 / totalWeights,
    log = TRUE
  ))

  # Ensure that the modelCost calculation does not result in NA
  if (is.na(weightedSSR + censoredContribution)) {
    warning(
      "Invalid model cost detected (NA). ",
      "Returning infinite error cost structure."
    )
    return(.errorCostTerms(index = index))
  }

  list(
    modelCost = weightedSSR + censoredContribution,
    minLogProbability = logProbability,
    nObservations = length(rawResiduals),
    M3Contribution = censoredContribution,
    rawSSR = sum(rawResiduals^2),
    weightedSSR = weightedSSR,
    x = observedX,
    yObserved = observedY,
    ySimulated = simulatedYApprox,
    scaleFactor = scaleFactor,
    errorWeights = round(errorWeights, 2),
    robustWeights = round(robustWeights, 2),
    userWeights = userWeights,
    totalWeights = round(totalWeights, 2),
    rawResiduals = rawResiduals,
    weightedResiduals = weightedResiduals,
    index = index
  )
}

#' Relative tolerance of single precision
#'
#' @description ospsuite stores the values of observed data and the simulation
#'   engine returns the simulated times in single precision, with a relative
#'   rounding error of about 6e-8. Values computed from them in double
#'   precision can differ from the single-precision values by that much, and
#'   are compared with this relative tolerance.
#' @keywords internal
#' @noRd
.singlePrecisionTolerance <- 1e-6

#' Observed times within the simulated times
#'
#' @description Moves an observed time that lies before the first or after
#'   the last simulated time by less than `.singlePrecisionTolerance` of that
#'   time to it. The simulated times are single-precision values of the
#'   output time points, which include the observed times, so an observed
#'   time can lie outside them only by their rounding. Other times are kept.
#'
#' @param observedX Observed times.
#' @param simulatedX Simulated times, finite.
#'
#' @return `observedX`, with the times outside the simulated times only by
#'   single-precision rounding replaced by the first or the last simulated
#'   time.
#' @keywords internal
#' @noRd
.withinSimulatedTimes <- function(observedX, simulatedX) {
  if (length(simulatedX) == 0) {
    return(observedX)
  }
  firstX <- min(simulatedX)
  lastX <- max(simulatedX)
  afterLast <- which(
    observedX > lastX &
      observedX - lastX <= .singlePrecisionTolerance * abs(lastX)
  )
  beforeFirst <- which(
    observedX < firstX &
      firstX - observedX <= .singlePrecisionTolerance * abs(firstX)
  )
  observedX[afterLast] <- lastX
  observedX[beforeFirst] <- firstX
  observedX
}

#' Simulated values at the observed times
#'
#' @description Interpolates the simulated values linearly at the observed
#'   times. With a single simulated time, takes the simulated value at an
#'   observed time equal to it, and `NA` at other times. An observed time
#'   outside the simulated times only by single-precision rounding takes the
#'   value at the first or the last simulated time (see
#'   `.withinSimulatedTimes()`). Other times outside them get `NA`.
#'
#' @param simulatedX,simulatedY Simulated times and values, finite and with
#'   times of at least zero.
#' @param observedX Observed times.
#'
#' @return A numeric vector with one value per observed time.
#' @keywords internal
#' @noRd
.simulatedAtObservedTimes <- function(simulatedX, simulatedY, observedX) {
  observedX <- .withinSimulatedTimes(observedX, simulatedX)
  if (length(unique(simulatedX)) > 1) {
    return(stats::approx(simulatedX, simulatedY, xout = observedX)$y)
  }
  simulatedY[match(observedX, simulatedX)]
}

#' Cost terms of one output mapping in the objective function
#'
#' @description Applies the LLOQ rule, the log transformation and the data
#'   weights to the simulated values and the prepared observed data of one
#'   output mapping and calculates its cost terms with `.costKernel()`.
#'
#'   The LLOQ rule of `objectiveFunctionType = "lsq"` changes the values that
#'   the simulated values are compared with, not the simulated values. An
#'   observed value at or above its LLOQ, and a value without an LLOQ, is
#'   compared with the simulated value. An observed value below its LLOQ is
#'   censored: its residual is 0 when the simulated value is below the LLOQ
#'   too, and the simulated value minus the LLOQ otherwise, on the scale of
#'   the cost. The value stored for a censored value (`blqValue`, see
#'   `.prepareObservedData()`) so does not enter its residual, and the
#'   residual is continuous in the simulated value. Data from which a y
#'   offset removes a baseline so give the same residuals as the same data
#'   without the baseline.
#'
#'   When the observed values of the output mapping have no LLOQ, or with
#'   `"m3"` when no observed value with an LLOQ has a y offset, the steps and
#'   their order are those of the reference objective function of the tests
#'   on data frames, so the results are identical (see
#'   `tests/testthat/helper-frozen-objective-function.R`).
#'
#' @param simulated A list with `xValues` and `yValues`, the simulated values
#'   in base units (see `.simulatedValues()`).
#' @param observed The prepared observed data of the output mapping (see
#'   `.prepareObservedData()`).
#' @param dataWeights The data weights of the output mapping, a named list by
#'   data set.
#' @param costControl The objective function options, with the scaling of the
#'   output mapping as `scaling`.
#' @param index Index of the output mapping.
#' @param quantityPath Path of the quantity of the output mapping, for the
#'   messages when no simulated or observed values enter the cost or when an
#'   LLOQ is not positive.
#'
#' @return A list of the arguments of `.newModelCost()`.
#' @keywords internal
#' @noRd
.mappingCostTerms <- function(
  simulated,
  observed,
  dataWeights,
  costControl,
  index,
  quantityPath = NULL
) {
  isLog <- costControl$scaling == "log"
  lloqRule <- costControl$objectiveFunctionType == "lsq" && observed$hasLloq

  if (isLog) {
    observedY <- observed$logYValues
    lloq <- observed$logLloq
  } else {
    observedY <- observed$yValues
    lloq <- observed$lloq
  }

  # Data weights by data set, NA where the output mapping has none
  userWeights <- rep(NA_real_, length(observedY))
  for (dataSet in names(dataWeights)) {
    userWeights[observed$name == dataSet] <- dataWeights[[dataSet]]
  }

  # The simulated values on the scale of the cost that enter it, with their
  # times
  simulatedY <- simulated$yValues
  if (isLog) {
    simulatedY <- ospsuite.utils::logSafe(
      simulatedY,
      epsilon = observed$logEpsilon,
      base = exp(1)
    )
  }
  keepSimulated <- .finiteValues(simulated$xValues, simulatedY)
  curve <- list(
    xValues = simulated$xValues[keepSimulated],
    yValues = simulatedY[keepSimulated]
  )
  keepObserved <- .finiteValues(observed$xValues, observedY)
  observedX <- observed$xValues[keepObserved]
  observedY <- observedY[keepObserved]

  # Ensuring there is enough data to perform calculations
  if (length(curve$xValues) < 1) {
    stop(
      messages$errorNoDataForCost("simulated", index, quantityPath),
      call. = FALSE
    )
  }
  if (length(observedX) < 1) {
    stop(
      messages$errorNoDataForCost("observed", index, quantityPath),
      call. = FALSE
    )
  }
  .validateLloq(observed, costControl, keepObserved, index, quantityPath)

  simulatedYApprox <- .simulatedAtObservedTimes(
    curve$xValues,
    curve$yValues,
    observedX
  )

  # The LLOQ rule: a censored value is compared with the simulated value
  # while it is below the LLOQ, and with the LLOQ above it. The censoring
  # compares the values before the log transformation, which can map values
  # close to 0 to the same value.
  referenceY <- observedY
  if (lloqRule) {
    observedLloq <- observed$lloq[keepObserved]
    censored <- is.finite(observedLloq) &
      observed$yValues[keepObserved] < observedLloq
    costLloq <- lloq[keepObserved]
    belowLloq <- which(censored & simulatedYApprox < costLloq)
    aboveLloq <- which(censored & simulatedYApprox >= costLloq)
    referenceY[belowLloq] <- simulatedYApprox[belowLloq]
    referenceY[aboveLloq] <- costLloq[aboveLloq]
  }

  # Applying M3 method for censored error calculation
  censoredContribution <- 0
  if (costControl$objectiveFunctionType == "m3") {
    censoredContribution <- .calculateCensoredContribution(
      observed = data.frame(
        xValues = observedX,
        yValues = observedY,
        lloq = lloq[keepObserved],
        transformedZero = observed$transformedZero[keepObserved]
      ),
      simulated = curve,
      scaling = costControl$scaling,
      linScaleCV = costControl$linScaleCV %||% NULL,
      logScaleSD = costControl$logScaleSD %||% NULL
    )
  }

  .costKernel(
    simulatedYApprox = simulatedYApprox,
    observedX = observedX,
    observedY = observedY,
    userWeights = userWeights[keepObserved],
    yErrorValues = observed$yErrorValues[keepObserved],
    yErrorType = observed$yErrorType[keepObserved],
    residualWeightingMethod = costControl$residualWeightingMethod,
    robustMethod = costControl$robustMethod,
    scaleVar = costControl$scaleVar,
    censoredContribution = censoredContribution,
    index = index,
    referenceY = referenceY
  )
}

#' Check that the LLOQs of an output mapping are positive
#'
#' @description Stops when an LLOQ of the observed values of an output mapping
#'   that enter its cost is not positive where the cost needs a positive one,
#'   and, with `objectiveFunctionType = "m3"`, when none of them has an LLOQ.
#'
#'   With log scaling, the LLOQ after the data transformations must be
#'   positive to have a logarithm: the LLOQ rule of `"lsq"` and the censoring
#'   of `"m3"` compare the simulated values with it. A y offset of minus the
#'   LLOQ or less makes it 0 or negative. With `"m3"`, the observed values
#'   below the LLOQ also enter the sum of squared residuals with their stored
#'   value (`blqValue`, see `.prepareObservedData()`), which must be positive
#'   too. A y offset of minus half the LLOQ or less makes it 0 or negative.
#'   As the value below the LLOQ is below the LLOQ, checking it checks both.
#'   A value within single precision of 0 counts as 0.
#'
#'   With `objectiveFunctionType = "m3"` and linear scaling, the LLOQ before
#'   the data transformations must be positive: the standard deviation of the
#'   censored values is calculated from it (see
#'   `.calculateCensoredContribution()`).
#'
#' @param observed The prepared observed data of the output mapping (see
#'   `.prepareObservedData()`).
#' @param costControl The objective function options, with the scaling of the
#'   output mapping as `scaling`.
#' @param keep Whether each observed value enters the cost.
#' @param index,quantityPath Index of the output mapping and path of its
#'   quantity, for the message.
#'
#' @return `observed`, invisibly.
#' @keywords internal
#' @noRd
.validateLloq <- function(
  observed,
  costControl,
  keep,
  index,
  quantityPath = NULL
) {
  hasLloq <- keep &
    is.finite(observed$lloq) &
    is.finite(observed$transformedZero)
  isM3 <- costControl$objectiveFunctionType == "m3"
  if (isM3 && !any(hasLloq)) {
    stop(messages$errorNoLloqForM3(index, quantityPath), call. = FALSE)
  }
  # The LLOQ before the data transformations, times the absolute y factor
  lloqBefore <- observed$lloq - observed$transformedZero
  if (costControl$scaling == "log") {
    # ospsuite stores the LLOQ in single precision, so a y offset of minus
    # the LLOQ, or minus half the LLOQ for the value below it, leaves a value
    # close to 0 instead of 0
    lowest <- if (isM3) observed$blqValue else observed$lloq
    notPositive <- hasLloq &
      lowest <= .singlePrecisionTolerance * abs(lloqBefore)
  } else if (isM3) {
    notPositive <- hasLloq & lloqBefore <= 0
  } else {
    return(invisible(observed))
  }
  if (any(notPositive)) {
    stop(
      messages$errorLloqNotPositive(
        unique(observed$name[notPositive]),
        costControl$scaling,
        costControl$objectiveFunctionType,
        index,
        quantityPath
      ),
      call. = FALSE
    )
  }
  invisible(observed)
}

#' Observed times without simulated values
#'
#' @description Whether observed data of an output mapping that enter its cost
#'   are at times that were not output time points of the simulation when its
#'   batch was built, and have no simulated value there. That is the case for
#'   such a time outside the simulated times, beyond their single-precision
#'   rounding, where the simulated values cannot be interpolated (see
#'   `.simulatedAtObservedTimes()`). The cost of the output mapping is then
#'   infinite. Times inside the simulated times are interpolated, also for
#'   the censored values of the M3 method.
#'
#' @param simulated The simulated values of the output mapping (see
#'   `.simulatedValues()`).
#' @param observed The prepared observed data of the output mapping (see
#'   `.prepareObservedData()`).
#' @param costControl The objective function options, with the scaling of the
#'   output mapping as `scaling`.
#' @param outputTimePoints The observed times, in min, that were added to the
#'   output time points of the simulation when its batch was built. They are
#'   calculated from the same values as the prepared observed times.
#'
#' @return `TRUE` or `FALSE`.
#' @keywords internal
#' @noRd
.hasUnsimulatedObservedTimes <- function(
  simulated,
  observed,
  costControl,
  outputTimePoints
) {
  observedY <- if (costControl$scaling == "log") {
    observed$logYValues
  } else {
    observed$yValues
  }
  # The observed values that enter the cost (see `.mappingCostTerms()`), at
  # times that were no output time points
  enters <- .finiteValues(observed$xValues, observedY)
  newTimes <- enters & !(observed$xValues %in% outputTimePoints)
  simulatedX <- simulated$xValues[
    .finiteValues(simulated$xValues, simulated$yValues)
  ]
  if (!any(newTimes) || length(simulatedX) == 0) {
    return(FALSE)
  }

  observedX <- .withinSimulatedTimes(observed$xValues, simulatedX)
  outside <- observedX < min(simulatedX) | observedX > max(simulatedX)
  any(newTimes & outside)
}

#' Values that enter the cost
#'
#' @description Keeps the values with a finite time of at least zero and a
#'   finite value. Used by `.calculateCostMetrics()` and the objective
#'   function.
#'
#' @param xValues,yValues Times and values.
#' @return A logical vector.
#' @keywords internal
#' @noRd
.finiteValues <- function(xValues, yValues) {
  xValues[xValues == Inf | xValues == -Inf] <- NA
  yValues[yValues == Inf | yValues == -Inf] <- NA
  xValues[xValues < 0] <- NA
  !(is.na(xValues) | is.na(yValues))
}

#' Combine the cost terms of all output mappings
#'
#' @description Sums the scalar cost terms of the output mappings in their
#'   order and binds their per-observation terms into one `modelCost` object,
#'   in one step. The result is identical to adding the `modelCost` objects of
#'   the output mappings one after the other and binding their rows.
#'
#' @param costTerms A list of results of `.costKernel()`, one per output
#'   mapping.
#' @return A `modelCost` object.
#' @keywords internal
#' @noRd
.combineCostTerms <- function(costTerms) {
  sumOf <- function(field) Reduce(`+`, lapply(costTerms, `[[`, field))
  rowFields <- c(
    "x",
    "yObserved",
    "ySimulated",
    "scaleFactor",
    "errorWeights",
    "robustWeights",
    "userWeights",
    "totalWeights",
    "rawResiduals",
    "weightedResiduals",
    "index"
  )
  rows <- lapply(rowFields, function(field) {
    unlist(
      lapply(costTerms, function(terms) {
        rep_len(terms[[field]], length(terms$x))
      }),
      use.names = FALSE
    )
  })
  names(rows) <- rowFields

  do.call(
    .newModelCost,
    c(
      list(
        modelCost = sumOf("modelCost"),
        minLogProbability = sumOf("minLogProbability"),
        nObservations = sumOf("nObservations"),
        weightedSSR = sumOf("weightedSSR"),
        rawSSR = sumOf("rawSSR"),
        M3Contribution = sumOf("M3Contribution")
      ),
      rows
    )
  )
}

#' Add the observed data of an output mapping to a `DataCombined`
#'
#' @description Adds the observed data sets of a `PIOutputMapping` to a
#'   `DataCombined` object, in the group named by the path of the mapped
#'   quantity, and applies the data transformations of the mapping. Used for
#'   the observed data of the objective function and of the plots.
#'
#' @param dataCombined A `DataCombined` object.
#' @param outputMapping A `PIOutputMapping` object.
#'
#' @return `dataCombined`, invisibly.
#' @keywords internal
#' @noRd
.addObservedData <- function(dataCombined, outputMapping) {
  observedDataSets <- outputMapping$observedDataSets
  # `DataCombined` takes the values in the order of `forNames`
  transformations <- .transformationsByDataSet(
    outputMapping$dataTransformations,
    names(observedDataSets)
  )
  dataCombined$addDataSets(
    observedDataSets,
    groups = outputMapping$quantity$path
  )
  dataCombined$setDataTransformations(
    forNames = names(observedDataSets),
    xOffsets = transformations$xOffsets,
    xScaleFactors = transformations$xFactors,
    yOffsets = transformations$yOffsets,
    yScaleFactors = transformations$yFactors
  )
  invisible(dataCombined)
}

#' Epsilon of the log transformation
#'
#' @description The ospsuite setting `LOG_SAFE_EPSILON` in the unit of the
#'   values to transform, converted with a molecular weight of 1. Values
#'   below it are replaced by it before the log transformation (see
#'   `ospsuite.utils::logSafe()`).
#'
#' @param dimension Dimension of the values.
#' @param unit Unit of the values.
#'
#' @return A numeric value.
#' @keywords internal
#' @noRd
.logEpsilon <- function(dimension, unit) {
  ospsuite::toUnit(
    quantityOrDimension = dimension,
    values = ospsuite::getOSPSuiteSetting("LOG_SAFE_EPSILON"),
    targetUnit = unit,
    molWeight = 1
  )
}

#' Prepare the observed data of an output mapping
#'
#' @description Reads the observed data sets of a `PIOutputMapping`, applies
#'   its data transformations and converts x values, y values, LLOQ and
#'   arithmetic error values to the base units of time and of the mapped
#'   quantity. It uses the same `DataCombined` transformations and unit
#'   conversion as a full evaluation, so the values are identical.
#'
#' @param outputMapping A `PIOutputMapping` object.
#'
#' @details The data transformations of `DataCombined` transform the LLOQ like
#'   the y values and set it to `NA` for a negative y factor. The importer
#'   stores a value below the LLOQ as half the LLOQ, so before the
#'   transformations the values below the LLOQ are in `[0, LLOQ)`, and that
#'   value is in its middle. After the transformations, they are in
#'   `[transformedZero, lloq)`, where `transformedZero` is the transformed
#'   value of 0, `yOffset * yFactor` in the unit of the data set, and the
#'   value stored for them is `blqValue = (transformedZero + lloq) / 2`, the
#'   transformed half LLOQ. Without a y offset, `transformedZero` is 0.
#'
#' @return A list with one entry per observation in `name` (the data set),
#'   `xValues`, `xDimension`, `yValues`, `yErrorValues`, `yErrorType`,
#'   `transformedZero`, `lloq` and `blqValue` (both `NA` without an LLOQ), the
#'   log-transformed `logYValues` and `logLloq`, and `hasLloq`, `logEpsilon`
#'   and `xUnit`, the unit of the x values.
#' @keywords internal
#' @noRd
.prepareObservedData <- function(outputMapping) {
  dataCombined <- .addObservedData(ospsuite::DataCombined$new(), outputMapping)

  yDimension <- outputMapping$quantity$dimension
  xUnit <- ospsuite::getBaseUnit("Time")
  yUnit <- ospsuite::getBaseUnit(yDimension)
  data <- dataCombined$toDataFrame()
  rows <- ospsuite:::.unitConverter(data, xUnit = xUnit, yUnit = yUnit)

  # The transformed value of 0 in the unit of the data set, converted to the
  # base unit as the LLOQ is
  transformations <- dataCombined$dataTransformations
  rowTransformations <- transformations[
    match(data$name, transformations$name),
  ]
  zeroData <- data[c(
    "xValues",
    "xUnit",
    "xDimension",
    "yValues",
    "yUnit",
    "yDimension",
    "molWeight"
  )]
  zeroData$lloq <- rowTransformations$yOffsets *
    rowTransformations$yScaleFactors
  transformedZero <- ospsuite:::.unitConverter(
    zeroData,
    xUnit = xUnit,
    yUnit = yUnit
  )$lloq

  # Values for the LLOQ rule and the log transformation, as in the data frames
  lloq <- rows$lloq
  hasLloq <- sum(is.finite(lloq)) > 0
  logEpsilon <- .logEpsilon(yDimension, yUnit)

  list(
    name = as.character(rows$name),
    xValues = rows$xValues,
    xDimension = rows$xDimension,
    yValues = rows$yValues,
    yErrorValues = rows$yErrorValues,
    yErrorType = rows$yErrorType,
    lloq = lloq,
    transformedZero = transformedZero,
    blqValue = (transformedZero + lloq) / 2,
    hasLloq = hasLloq,
    logYValues = ospsuite.utils::logSafe(
      rows$yValues,
      epsilon = logEpsilon,
      base = exp(1)
    ),
    logLloq = ospsuite.utils::logSafe(
      lloq,
      epsilon = logEpsilon,
      base = exp(1)
    ),
    logEpsilon = logEpsilon,
    xUnit = xUnit
  )
}

#' Construct a canonical `modelCost` object
#'
#' Single constructor for the `modelCost` schema. Every producer (the kernel
#' happy path and the error/failure substitute) routes through it so
#' `costVariables` and `residualDetails` always share one fixed column set,
#' which keeps the cost terms of several output mappings safe to combine (see
#' `.combineCostTerms()`). The constructor owns the `index` field.
#'
#' @param modelCost Total scalar cost the optimizer minimizes.
#' @param minLogProbability Scalar negative log probability of the fit.
#' @param nObservations Number of observations entering the cost.
#' @param weightedSSR Weighted sum of squared residuals.
#' @param rawSSR Unweighted sum of squared residuals.
#' @param M3Contribution Censored-data contribution to the cost.
#' @param x,yObserved,ySimulated,scaleFactor,errorWeights,robustWeights,userWeights,totalWeights,rawResiduals,weightedResiduals
#'   Per-observation vectors forming `residualDetails`. Default to `NA_real_` for
#'   the failure substitute.
#' @param index Output-mapping index stored on every `residualDetails` row.
#' @return A `modelCost` object: a list with `modelCost`, `minLogProbability`,
#'   `costVariables`, and `residualDetails`.
#' @keywords internal
#' @noRd
.newModelCost <- function(
  modelCost,
  minLogProbability,
  nObservations,
  weightedSSR,
  rawSSR = NA_real_,
  M3Contribution = 0,
  x = NA_real_,
  yObserved = NA_real_,
  ySimulated = NA_real_,
  scaleFactor = NA_real_,
  errorWeights = NA_real_,
  robustWeights = NA_real_,
  userWeights = NA_real_,
  totalWeights = NA_real_,
  rawResiduals = NA_real_,
  weightedResiduals = NA_real_,
  index = NA_real_
) {
  costVariables <- data.frame(
    nObservations = nObservations,
    M3Contribution = M3Contribution,
    rawSSR = rawSSR,
    weightedSSR = weightedSSR
  )

  residualDetails <- data.frame(
    index = index,
    x = x,
    yObserved = yObserved,
    ySimulated = ySimulated,
    scaleFactor = scaleFactor,
    errorWeights = errorWeights,
    robustWeights = robustWeights,
    userWeights = userWeights,
    totalWeights = totalWeights,
    rawResiduals = rawResiduals,
    weightedResiduals = weightedResiduals
  )

  structure(
    list(
      modelCost = modelCost,
      minLogProbability = minLogProbability,
      costVariables = costVariables,
      residualDetails = residualDetails
    ),
    class = "modelCost"
  )
}

#' Compute error-based residual weights
#'
#' @param yValues Vector of y-values, required for conversion
#' @param yErrorValues Vector of y-value errors
#' @param yErrorType Vector of error type strings (`ArithmeticStdDev`,
#'   `GeometricStdDev`)
#' @param defaultWeight Fallback weight value when inputs are missing or invalid
#' @return Numeric vector of residual weights computed as 1 / StdDev
#'
#' @keywords internal
#' @noRd
.computeErrorWeights <- function(
  yValues,
  yErrorValues,
  yErrorType,
  defaultWeight = 1
) {
  ospsuite.utils::validateIsNumeric(yValues)
  ospsuite.utils::validateIsNumeric(yErrorValues)
  ospsuite.utils::validateIsCharacter(yErrorType)
  ospsuite.utils::isSameLength(yValues, yErrorValues)
  ospsuite.utils::isSameLength(yValues, yErrorType)

  weights <- rep(defaultWeight, length(yValues))

  idxArith <- which(
    yErrorType == "ArithmeticStdDev" & yValues > 0 & yErrorValues > 0
  )
  if (length(idxArith) > 0) {
    weights[idxArith] <- 1 / yErrorValues[idxArith]
  }

  idxGSD <- which(
    yErrorType == "GeometricStdDev" & yValues > 0 & yErrorValues > 1
  )
  if (length(idxGSD) > 0) {
    # SD = mean * sqrt(e^(sigma^2) - 1), sigma = log(GSD)
    stDev <- yValues[idxGSD] * sqrt(exp(log(yErrorValues[idxGSD])^2) - 1)
    weights[idxGSD] <- 1 / stDev
  }

  nEligible <- sum(
    yErrorType %in% c("ArithmeticStdDev", "GeometricStdDev") & yValues > 0
  )
  if (length(idxArith) + length(idxGSD) < nEligible) {
    warning(messages$warningNoValidErrorValues())
  }

  return(weights)
}

#' Plot Model Cost Residuals
#'
#' Plots raw residuals and, if different, weighted residuals from a `modelCost`
#' object.
#'
#' @param x A `modelCost` object containing residuals to plot.
#' @param legpos Position of the legend; default is "topright". Use NA to omit
#'   the legend.
#' @param ... Additional arguments passed to the plot function.
#' @return Generates a plot.
#' @examples
#' # Assuming modelCostObj is a valid `modelCost` object
#' \dontrun{
#' plot.modelCost(modelCostObj)
#' }
#' @export
plot.modelCost <- function(x, legpos = "topright", ...) {
  if (!inherits(x, "modelCost")) {
    stop("x must be a 'modelCost' object.")
  }

  # Ensure 'residualDetails' is present
  if (!"residualDetails" %in% names(x)) {
    stop("'residualDetails' component missing in 'modelCost' object.")
  }

  # Extracting residuals data
  residualsData <- x$residualDetails

  if (all(is.na(residualsData$rawResiduals))) {
    stop(messages$errorNoResidualsToPlot())
  }

  showWeighted <- any(
    residualsData$rawResiduals != residualsData$weightedResiduals,
    na.rm = TRUE
  )

  # Setup base plot
  plot(
    residualsData$x,
    residualsData$rawResiduals,
    xlab = "x",
    ylab = "Residuals",
    pch = 16,
    col = "black",
    ...
  )

  if (showWeighted) {
    graphics::points(
      residualsData$x,
      residualsData$weightedResiduals,
      pch = 17,
      col = "red",
      ...
    )
  }

  # Legend
  legends <- "Raw Residuals"
  colors <- "black"
  pchValues <- 16

  if (showWeighted) {
    legends <- c(legends, "Weighted Residuals")
    colors <- c(colors, "red")
    pchValues <- c(pchValues, 17)
  }

  if (!is.na(legpos)) {
    graphics::legend(legpos, legend = legends, col = colors, pch = pchValues)
  }
}

#' Constructs Model Cost Summary for Error Handling
#'
#' Creates an infinite-cost `modelCost` object for simulation or objective
#' function failures, routed through `.newModelCost()` so it shares the
#' canonical schema.
#'
#' @param index Output-mapping index stored on the `residualDetails` row.
#'   Defaults to `NA_real_`.
#' @return A `modelCost` object filled with infinite cost values.
#' @keywords internal
#' @noRd
.createErrorCostStructure <- function(index = NA_real_) {
  do.call(.newModelCost, .errorCostTerms(index = index))
}

#' Cost terms of a failed evaluation
#'
#' @description The terms of `.createErrorCostStructure()`: infinite costs
#'   and one `residualDetails` row of `NA` values.
#'
#' @param index Output-mapping index stored on the `residualDetails` row.
#' @return A list of the arguments of `.newModelCost()`.
#' @keywords internal
#' @noRd
.errorCostTerms <- function(index = NA_real_) {
  list(
    modelCost = Inf,
    minLogProbability = Inf,
    nObservations = 1,
    weightedSSR = Inf,
    rawSSR = Inf,
    M3Contribution = Inf,
    x = NA_real_,
    yObserved = NA_real_,
    ySimulated = NA_real_,
    scaleFactor = NA_real_,
    errorWeights = NA_real_,
    robustWeights = NA_real_,
    userWeights = NA_real_,
    totalWeights = NA_real_,
    rawResiduals = NA_real_,
    weightedResiduals = NA_real_,
    index = index
  )
}

#' Calculate Contribution of Censored Data
#'
#'
#' Evaluates the impact of censored data (below quantification limit, BQL) on
#' model cost, employing maximum likelihood estimation to integrate BQL
#' observations effectively. By acknowledging BQL data as censored observations,
#' this method ensures such data contribute to model accuracy without
#' misrepresenting actual concentrations. It applies linear or logarithmic
#' scaling to calculate standard deviations for censored probabilities,
#' enhancing overall model cost assessment with respect to detection limits.
#'
#' @param observed Data frame containing observed data, must include 'lloq',
#'   'xValues' and 'yValues' columns. An observation below its LLOQ is
#'   censored. An observation at its LLOQ, or without an LLOQ (`NA`), is not
#'   censored. An optional 'transformedZero' column holds the transformed
#'   value of 0 of each observation (see `.prepareObservedData()`), 0 if it
#'   is missing.
#' @param simulated Data frame or list containing simulated data, with
#'   'xValues' and 'yValues', in the same units as the observed data. The
#'   simulated value of a censored observation is interpolated at its time
#'   with `.simulatedAtObservedTimes()`, as for the least-squares part of the
#'   cost, so the simulated times do not need to include it.
#' @param scaling Character string specifying the scaling method; should be one
#'   of the predefined scaling options.
#' @param linScaleCV Numeric, coefficient used to calculate standard deviation
#'   for linear scaling, applied to the LLOQ of each censored observation
#'   before a y offset: `linScaleCV * (lloq - transformedZero)`. A y offset
#'   shifts the values but does not change their standard deviation.
#' @param logScaleSD Numeric, standard deviation for logarithmic scaling: the
#'   standard deviation of the natural logarithm of the values, applied
#'   uniformly to all censored observations. For a coefficient of variation
#'   `CV`, it is `sqrt(log(1 + CV^2))`.
#' @return Numeric value representing the sum of squared errors for censored
#'   observations, contributing to the model's total cost.
#' @keywords internal
#' @examples
#' \dontrun{
#' .calculateCensoredContribution(observedData, simulatedData, scaling = "lin", linScaleCV = 0.2)
#' }
.calculateCensoredContribution <- function(
  observed,
  simulated,
  scaling,
  linScaleCV = NULL,
  logScaleSD = NULL
) {
  ospsuite.utils::validateIsIncluded(c("lloq", "xValues"), colnames(observed))
  ospsuite.utils::validateIsIncluded(c("xValues", "yValues"), names(simulated))
  ospsuite.utils::validateIsNumeric(c(linScaleCV, logScaleSD))
  ospsuite.utils::validateEnumValue(scaling, ScalingOptions)

  lloq <- unique(stats::na.omit(observed$lloq))
  ospsuite.utils::validateIsNumeric(lloq)

  if (length(lloq) == 0) {
    stop(messages$errorNoLloqForM3())
  }
  if (!"transformedZero" %in% colnames(observed)) {
    observed$transformedZero <- 0
  }

  # Identify censored observations based on LLOQ
  observedCensored <- observed[
    !is.na(observed$lloq) &
      (observed$yValues < observed$lloq),
  ]

  # No censored data to process
  if (nrow(observedCensored) == 0) {
    return(0)
  }

  if (scaling == "lin" && !is.null(linScaleCV)) {
    stDev <- abs(
      linScaleCV * (observedCensored$lloq - observedCensored$transformedZero)
    )
  } else if (scaling == "log" && !is.null(logScaleSD)) {
    stDev <- logScaleSD
  } else {
    stop("Scaling method and scaling parameters are not compatible.")
  }

  # The simulated values at the times of the censored values, interpolated
  # as for the least-squares part of the cost, so that a censored value needs
  # no simulated value at exactly its time
  simulatedCensored <- .simulatedAtObservedTimes(
    simulated$xValues,
    simulated$yValues,
    observedCensored$xValues
  )
  censoredProbabilities <- stats::pnorm(
    (observedCensored$lloq - simulatedCensored) / stDev
  )
  censoredProbabilities[censoredProbabilities == 0] <- .Machine$double.xmin
  censoredErrorVector <- -2 * log(censoredProbabilities, base = 10)
  censoredErrorVector <- sqrt(censoredErrorVector)

  return(sum(censoredErrorVector^2))
}

#' Calculate Huber Weights for Residuals
#'
#' This function calculates Huber weights for residuals, reducing the influence
#' of outliers. Uses MAD for scaling and applies a cutoff at `k` times MAD.
#' @param residuals Numeric vector of residuals.
#' @param k Tuning parameter for outlier cutoff. Default is 1.345.
#' @return Numeric vector of Huber weights.
#' @keywords internal
.calculateHuberWeights <- function(residuals, k = 1.345) {
  # Calculate the scale of the residuals (MAD = Median Absolute Deviation)
  mad <- mad(residuals, constant = 1.4826)
  # Scale residuals
  standardizedResiduals <- residuals / (k * mad)
  # Huber weights
  weights <- ifelse(
    abs(standardizedResiduals) <= 1,
    1,
    1 / abs(standardizedResiduals)
  )
  return(weights)
}

#' Calculate Bisquare Weights for Residuals
#'
#' This function calculates Bisquare (Tukey's biweight) weights for residuals,
#' aggressively reducing outlier influence. Scales residuals using MAD with a
#' cutoff at `c` times MAD.
#' @param residuals Numeric vector of residuals.
#' @param c Tuning parameter for outlier exclusion. Default is 4.685.
#' @return Numeric vector of Bisquare weights.
#' @keywords internal
.calculateBisquareWeights <- function(residuals, c = 4.685) {
  # Calculate the scale of the residuals (MAD = Median Absolute Deviation)
  mad <- mad(residuals, constant = 1.4826)
  # Scale residuals
  standardizedResiduals <- residuals / (c * mad)
  # Bisquare weights
  weights <- ifelse(
    abs(standardizedResiduals) < 1,
    (1 - standardizedResiduals^2)^2,
    0
  )
  return(weights)
}
