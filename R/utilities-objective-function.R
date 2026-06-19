#' @title Calculate Cost Metrics for Model Evaluation
#'
#' @description Internal utility to calculate residual-based cost metrics for
#' model fit assessment. Used within parameter estimation routines.
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

  # Handle infinite values
  df$xValues[df$xValues == Inf | df$xValues == -Inf] <- NA
  df$yValues[df$yValues == Inf | df$yValues == -Inf] <- NA
  df$xValues[df$xValues < 0] <- NA
  idx <- is.na(df$xValues) | is.na(df$yValues)
  df <- df[!idx, ]

  # Splitting dataframe into simulated and observed data
  simulatedData <- df[df$dataType == "simulated", ]
  simulatedData <- simulatedData[!is.na(simulatedData$yValues), ]
  observedData <- df[df$dataType == "observed", ]
  observedData <- observedData[!is.na(observedData$yValues), ]

  # Ensuring there is enough data to perform calculations
  if (NROW(simulatedData) < 1 | is.null(simulatedData)) {
    stop("No simulated data found when calculating cost function.")
  }
  if (NROW(observedData) < 1 | is.null(observedData)) {
    stop("No observed data found when calculating cost function.")
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

  # Extracting values for interpolation or direct matching
  simulatedXVal <- simulatedData[["xValues"]]
  simulatedYVal <- simulatedData[["yValues"]]
  observedXVal <- observedData[["xValues"]]
  observedYVal <- observedData[["yValues"]]

  # Interpolating simulated Y values based on observed X values if applicable
  if (length(unique(simulatedXVal)) > 1) {
    simulatedYValApprox <- stats::approx(
      simulatedXVal,
      simulatedYVal,
      xout = observedXVal
    )$y
  } else {
    simulatedYValApprox <- simulatedYVal[match(observedXVal, simulatedXVal)]
  }

  # Calculate raw residuals
  rawResiduals <- simulatedYValApprox - observedYVal

  # Scaling residuals by the number of observations if requested
  scaleFactor <- if (scaleVar) 1 / length(observedYVal) else 1
  normalizedResiduals <- rawResiduals * scaleFactor

  # Compute user-defined weights if available
  userWeights <- observedData$weights
  userWeights[is.na(userWeights)] <- 1

  # Determining the method for residual weighting
  errorWeights <-
    switch(
      residualWeightingMethod,
      "none" = 1,
      "error" = .computeErrorWeights(
        yValues = observedData[["yValues"]],
        yErrorValues = observedData[["yErrorValues"]],
        yErrorType = observedData[["yErrorType"]]
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
    simulatedYValApprox,
    observedYVal,
    1 / totalWeights,
    log = TRUE
  ))

  modelCost <- .newModelCost(
    modelCost = weightedSSR + censoredContribution,
    minLogProbability = logProbability,
    nObservations = length(rawResiduals),
    M3Contribution = censoredContribution,
    rawSSR = sum(rawResiduals^2),
    weightedSSR = weightedSSR,
    x = observedXVal,
    yObserved = observedYVal,
    ySimulated = simulatedYValApprox,
    scaleFactor = scaleFactor,
    errorWeights = round(errorWeights, 2),
    robustWeights = round(robustWeights, 2),
    userWeights = userWeights,
    totalWeights = round(totalWeights, 2),
    rawResiduals = rawResiduals,
    weightedResiduals = weightedResiduals,
    index = index
  )

  # Ensure that the modelCost calculation does not result in NA
  if (is.na(modelCost$modelCost)) {
    warning(
      "Invalid model cost detected (NA). Returning infinite error cost structure."
    )
    return(.createErrorCostStructure(index = index))
  }

  return(modelCost)
}

#' Construct a canonical `modelCost` object
#'
#' Single constructor for the `modelCost` schema. Every producer (the kernel
#' happy path and the error/failure substitute) routes through it so
#' `costVariables` and `residualDetails` always share one fixed column set,
#' which keeps `.summarizeCostLists()` safe to aggregate. The constructor owns
#' the `index` field.
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
  .newModelCost(
    modelCost = Inf,
    minLogProbability = Inf,
    nObservations = 1,
    weightedSSR = Inf,
    rawSSR = Inf,
    M3Contribution = Inf,
    index = index
  )
}

#' Apply Log Transformation to Data Frame
#'
#' Transforms the `yValues` and `lloq` columns in the given data frame using a
#' log transformation. Currently, this function only supports `obsVsPredDf` data
#' frames, which must contain `yDimension`, `yUnit`, `yValues`, and `lloq`
#' columns.
#'
#' @param df A `tbl_df` representing the observed vs predicted data frame
#'   (`obsVsPredDf`).
#' @param base A positive numeric value specifying the logarithm base. Defaults
#'   to natural logarithm (`exp(1)`).
#'
#' @return A transformed data frame with log-transformed `yValues` and `lloq`.
#' @keywords internal
#'
#' @examples
#' # Assuming df is a valid obsVsPredDf data frame
#' \dontrun{
#' transformedDf <- applyLogTransformation(df)
#' }
.applyLogTransformation <- function(df, base = exp(1)) {
  ospsuite.utils::validateIsOfType(df, "tbl_df")
  ospsuite.utils::validateIsNumeric(base)
  ospsuite.utils::validateIsIncluded(
    c("yDimension", "yUnit", "yValues", "lloq"),
    colnames(df)
  )

  UNITS_EPSILON <- ospsuite::toUnit(
    quantityOrDimension = df$yDimension[1],
    values = ospsuite::getOSPSuiteSetting("LOG_SAFE_EPSILON"),
    targetUnit = df$yUnit[1],
    molWeight = 1
  )

  df$yValues <- ospsuite.utils::logSafe(
    df$yValues,
    epsilon = UNITS_EPSILON,
    base = base
  )
  df$lloq <- ospsuite.utils::logSafe(
    df$lloq,
    epsilon = UNITS_EPSILON,
    base = base
  )

  return(df)
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

#' Summarize Cost Lists
#'
#' This function takes two lists, each being the output of the
#' `.calculateCostMetrics` function, and summarizes them. It aggregates model
#' costs and min log probabilities, and combines cost and residual details by
#' row-binding.
#'
#' @param list1 The first list, containing the output of the
#'   `.calculateCostMetrics` function, which includes `modelCost`,
#'   `minLogProbability`, `costVariables`, and `residualDetails`.
#' @param list2 The second list, containing the output of the
#'   `.calculateCostMetrics` function, which includes `modelCost`,
#'   `minLogProbability`, `costVariables`, and `residualDetails`.
#'
#' @return Returns a list that includes the sum of `modelCosts`, the sum of
#'   `minLogProbabilities`, a row-bound combination of `costVariables`, and a
#'   row-bound combination of `residualDetails`.
#'
#' @keywords internal
.summarizeCostLists <- function(list1, list2) {
  mergedList <- list(
    modelCost = list1$modelCost + list2$modelCost,
    minLogProbability = list1$minLogProbability + list2$minLogProbability,
    costVariables = list1$costVariables + list2$costVariables,
    residualDetails = rbind(list1$residualDetails, list2$residualDetails)
  )
  class(mergedList) <- class(list1)

  return(mergedList)
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
