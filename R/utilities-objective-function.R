#' @title Calculate Cost Metrics for Model Evaluation
#'
#' @description Internal utility to calculate the residual-based cost metrics
#' of one output mapping from a data frame of simulated and observed data, for
#' example that of a `DataCombined` object. The objective function calculates
#' the same cost on numeric vectors with `.mappingCostTerms()`. Both use
#' `.costKernel()`, which the tests pin through this function.
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
    simulatedX = simulatedData[["xValues"]],
    simulatedY = simulatedData[["yValues"]],
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
#' @description Interpolates the simulated values at the observed times and
#'   calculates the residuals, their weights and the cost of one output
#'   mapping. Shared by `.calculateCostMetrics()` and the objective function.
#'   The values must be finite, with times of at least zero.
#'
#' @param simulatedX,simulatedY Simulated times and values.
#' @param observedX,observedY Observed times and values.
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
  simulatedX,
  simulatedY,
  observedX,
  observedY,
  userWeights,
  yErrorValues,
  yErrorType,
  residualWeightingMethod,
  robustMethod,
  scaleVar,
  censoredContribution,
  index
) {
  # Interpolating simulated Y values based on observed X values if applicable
  if (length(unique(simulatedX)) > 1) {
    simulatedYApprox <- stats::approx(
      simulatedX,
      simulatedY,
      xout = observedX
    )$y
  } else {
    simulatedYApprox <- simulatedY[match(observedX, simulatedX)]
  }

  # Calculate raw residuals
  rawResiduals <- simulatedYApprox - observedY

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
    observedY,
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

#' Cost terms of one output mapping in the objective function
#'
#' @description Applies the LLOQ rule, the log transformation and the data
#'   weights to the simulated values and the prepared observed data of one
#'   output mapping and calculates its cost terms with `.costKernel()`, in the
#'   same order as the objective function of version 2.2.0.9009 did on data
#'   frames before `.calculateCostMetrics()`.
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
#'
#' @return A list of the arguments of `.newModelCost()`.
#' @keywords internal
#' @noRd
.mappingCostTerms <- function(
  simulated,
  observed,
  dataWeights,
  costControl,
  index
) {
  simulatedY <- simulated$yValues
  # For LSQ, simulated values below the LLOQ are replaced by LLOQ / 2
  if (costControl$objectiveFunctionType == "lsq" && observed$hasLloq) {
    belowLloq <- simulatedY < observed$lloqMin
    if (anyNA(belowLloq)) {
      stop(messages$errorSimulatedValuesMissing())
    }
    simulatedY[belowLloq] <- observed$lloqMin / 2
  }

  if (costControl$scaling == "log") {
    simulatedY <- ospsuite.utils::logSafe(
      simulatedY,
      epsilon = observed$logEpsilon,
      base = exp(1)
    )
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

  keepSimulated <- .finiteValues(simulated$xValues, simulatedY)
  keepObserved <- .finiteValues(observed$xValues, observedY)
  simulatedX <- simulated$xValues[keepSimulated]
  simulatedY <- simulatedY[keepSimulated]
  observedX <- observed$xValues[keepObserved]
  observedY <- observedY[keepObserved]

  # Ensuring there is enough data to perform calculations
  if (length(simulatedX) < 1) {
    stop(messages$errorNoDataForCost("simulated"))
  }
  if (length(observedX) < 1) {
    stop(messages$errorNoDataForCost("observed"))
  }

  # Applying M3 method for censored error calculation
  censoredContribution <- 0
  if (costControl$objectiveFunctionType == "m3") {
    censoredContribution <- .calculateCensoredContribution(
      observed = data.frame(
        xValues = observedX,
        xUnit = observed$xUnit,
        xDimension = observed$xDimension[keepObserved],
        yValues = observedY,
        lloq = lloq[keepObserved]
      ),
      simulated = data.frame(
        xValues = simulatedX,
        xUnit = observed$xUnit,
        xDimension = ospsuite::ospDimensions$Time,
        yValues = simulatedY
      ),
      scaling = costControl$scaling,
      linScaleCV = costControl$linScaleCV %||% NULL,
      logScaleSD = costControl$logScaleSD %||% NULL
    )
  }

  .costKernel(
    simulatedX = simulatedX,
    simulatedY = simulatedY,
    observedX = observedX,
    observedY = observedY,
    userWeights = userWeights[keepObserved],
    yErrorValues = observed$yErrorValues[keepObserved],
    yErrorType = observed$yErrorType[keepObserved],
    residualWeightingMethod = costControl$residualWeightingMethod,
    robustMethod = costControl$robustMethod,
    scaleVar = costControl$scaleVar,
    censoredContribution = censoredContribution,
    index = index
  )
}

#' Observed times without simulated values
#'
#' @description Whether observed data of an output mapping that enter its cost
#'   are at times that were not output time points of the simulation when its
#'   batch was built, and have no simulated value there. That is the case for
#'   such a time outside the simulated times, where the simulated values
#'   cannot be interpolated, and, with the M3 method, for a censored value at
#'   such a time that was not simulated, because
#'   `.calculateCensoredContribution()` needs a simulated value at exactly its
#'   time. The cost of the output mapping is then infinite.
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
  if (costControl$scaling == "log") {
    observedY <- observed$logYValues
    lloq <- observed$logLloq
  } else {
    observedY <- observed$yValues
    lloq <- observed$lloq
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

  outside <- observed$xValues < min(simulatedX) |
    observed$xValues > max(simulatedX)
  if (any(newTimes & outside)) {
    return(TRUE)
  }
  if (costControl$objectiveFunctionType != "m3" || all(is.na(lloq[enters]))) {
    return(FALSE)
  }
  # As in `.calculateCensoredContribution()`: a value without an LLOQ takes
  # the lowest LLOQ, and `merge()` finds the simulated value of a censored
  # value by its time, compared as by `as.character()`
  lloq[is.na(lloq)] <- min(lloq[enters], na.rm = TRUE)
  censored <- observedY <= lloq
  simulatedTimes <- as.character(simulatedX)
  notSimulated <- !(as.character(observed$xValues) %in% simulatedTimes)
  any(newTimes & censored & notSimulated)
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
#'   the output mappings one after the other and binding their rows, as the
#'   objective function of version 2.2.0.9009 did.
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
  transformations <- outputMapping$dataTransformations
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
#' @return A list with one entry per observation in `name` (the data set),
#'   `xValues`, `xDimension`, `yValues`, `yErrorValues`, `yErrorType` and
#'   `lloq`, the log-transformed `logYValues` and `logLloq`, and `hasLloq`,
#'   `lloqMin`, `logEpsilon` and `xUnit`, the unit of the x values.
#' @keywords internal
#' @noRd
.prepareObservedData <- function(outputMapping) {
  dataCombined <- .addObservedData(ospsuite::DataCombined$new(), outputMapping)

  yDimension <- outputMapping$quantity$dimension
  xUnit <- ospsuite::getBaseUnit("Time")
  yUnit <- ospsuite::getBaseUnit(yDimension)
  rows <- ospsuite:::.unitConverter(
    dataCombined$toDataFrame(),
    xUnit = xUnit,
    yUnit = yUnit
  )

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
    hasLloq = hasLloq,
    lloqMin = if (hasLloq) min(lloq, na.rm = TRUE) else NA_real_,
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
#'   'xValues', 'xUnit', 'xDimension', and 'yValues' columns.
#' @param simulated Data frame containing simulated data, must include
#'   'xValues', 'xUnit', 'xDimension', and 'yValues' columns.
#' @param scaling Character string specifying the scaling method; should be one
#'   of the predefined scaling options.
#' @param linScaleCV Numeric, coefficient used to calculate standard deviation
#'   for linear scaling, applied to 'lloq' values.
#' @param logScaleSD Numeric, standard deviation for logarithmic scaling,
#'   applied uniformly to all censored observations.
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
  ospsuite.utils::validateIsNumeric(c(linScaleCV, logScaleSD))
  ospsuite.utils::validateEnumValue(scaling, ScalingOptions)

  lloq <- unique(stats::na.omit(observed$lloq))
  ospsuite.utils::validateIsNumeric(lloq)

  if (length(lloq) == 0) {
    stop("LLOQ value not provided with the data.")
  } else if (any(is.na(observed$lloq))) {
    observed$lloq[is.na(observed$lloq)] <- min(lloq, na.rm = TRUE)
  }

  # Identify censored and uncensored observations based on LLOQ
  observedUncensored <- observed[
    is.na(observed$lloq) |
      (observed$yValues > observed$lloq),
  ]
  observedCensored <- observed[
    !is.na(observed$lloq) &
      (observed$yValues <= observed$lloq),
  ]
  simulatedCensored <- merge(
    observedCensored[c("xValues", "xUnit", "xDimension")],
    simulated,
    by = c("xValues", "xUnit", "xDimension"),
    all.x = TRUE
  )

  # No censored data to process
  if (nrow(simulatedCensored) == 0) {
    return(0)
  }

  if (scaling == "lin" && !is.null(linScaleCV)) {
    stDev <- abs(linScaleCV * lloq)
  } else if (scaling == "log" && !is.null(logScaleSD)) {
    stDev <- logScaleSD
  } else {
    stop("Scaling method and scaling parameters are not compatible.")
  }

  censoredProbabilities <- stats::pnorm(
    (observedCensored$lloq - simulatedCensored$yValues) / stDev
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
