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
#'   residuals. Options include `"none"` (default), `"std"`, `"mean"`, and
#'   `"error"`.
#' @param robustMethod A string indicating the robust method to apply to the
#'   residuals. Options include `"none"` (default), `"huber"`, and `"bisquare"`.
#' @param scaleVar A boolean indicating whether to scale residuals by the number
#'   of observations. Defaults to `FALSE`.
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
#' - `costDetails`: A dataframe with details on the cost calculations.
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
#' costMetrics <- .calculateCostMetrics(df, residualWeightingMethod = "std", scaleVar = TRUE)
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
  if (NROW(simulatedData) < 2 | is.null(simulatedData)) {
    stop("No simulated data found when calculating cost function.")
  }
  if (NROW(observedData) < 2 | is.null(observedData)) {
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
      ),
      "std" = {
        if (length(unique(observedYVal)) == 1) {
          sqrt(.Machine$double.eps)
        } else {
          stats::sd(observedYVal, na.rm = TRUE)
        }
      },
      "mean" = {
        meanVal <- mean(abs(observedYVal), na.rm = TRUE)
        if (meanVal == 0) 1 else meanVal
      }
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

  residualsData <- data.frame(
    index = NA_real_,
    x = observedXVal,
    yObserved = observedYVal,
    ySimulated = simulatedYValApprox,
    scaleFactor = scaleFactor,
    errorWeights = round(errorWeights, 2),
    robustWeights = round(robustWeights, 2),
    userWeights = userWeights,
    totalWeights = round(totalWeights, 2),
    rawResiduals = rawResiduals,
    weightedResiduals = weightedResiduals
  )

  # Compiling cost variables based on residuals
  costVariables <- data.frame(
    # scaleFactor = scaleFactor,
    nObservations = length(rawResiduals),
    M3Contribution = censoredContribution,
    rawSSR = sum(rawResiduals^2),
    weightedSSR = sum(weightedResiduals^2)
  )

  # Calculating log probability to evaluate model fit
  logProbability <- -sum(log(pmax(
    0,
    stats::dnorm(
      residualsData$ySimulated,
      residualsData$yObserved,
      1 / residualsData$totalWeights
    )
  )))

  # Organizing output with model evaluation metrics
  modelCost <- list(
    modelCost = costVariables$weightedSSR + censoredContribution,
    minLogProbability = logProbability,
    costVariables = costVariables,
    residualDetails = residualsData
  )

  # Ensure that the modelCost calculation does not result in NA
  if (is.na(modelCost$modelCost)) {
    warning(
      "Invalid model cost detected (NA). Returning infinite error cost structure."
    )
    return(.createErrorCostStructure(infinite = TRUE))
  } else {
    class(modelCost) <- "modelCost"
    return(modelCost)
  }
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

  n <- numeric(length(yValues))

  weights <- rep(defaultWeight, length(yValues))

  idx <- which(
    yErrorType == "ArithmeticStdDev" & yValues > 0 & yErrorValues > 0
  )
  if (length(idx) > 0) {
    weights[idx] <- 1 / yErrorValues[idx]
  }

  idx <- which(yErrorType == "GeometricStdDev" & yValues > 0 & yErrorValues > 1)
  if (length(idx) > 0) {
    # SD = mean * sqrt(e^(σ^2) - 1) with approximation e^(σ^2) = GSD^2
    stDev <- yValues[idx] * sqrt(yErrorValues[idx]^2 - 1)
    weights[idx] <- 1 / stDev
  }

  return(weights)
}

#' Plot Model Cost Residuals
#'
#' Plots raw residuals and, if different, weighted and robust weighted residuals
#' from a `modelCost` object.
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

  # Setup base plot
  plot(
    residualsData$x,
    residualsData$residuals,
    xlab = "x",
    ylab = "Residuals",
    pch = 16,
    col = "black",
    ...
  )

  # Add weightedResiduals if different from rawResiduals
  if (!all(residualsData$residuals == residualsData$weightedResiduals)) {
    graphics::points(
      residualsData$x,
      residualsData$weightedResiduals,
      pch = 17,
      col = "red",
      ...
    )
  }

  # Add robustWeightedResiduals if different from rawResiduals
  if (!all(residualsData$residuals == residualsData$robustWeightedResiduals)) {
    graphics::points(
      residualsData$x,
      residualsData$robustWeightedResiduals,
      pch = 18,
      col = "blue",
      ...
    )
  }

  # Legend
  legends <- c("Raw Residuals")
  colors <- c("black")
  pch_values <- c(16)

  if (!all(residualsData$residuals == residualsData$weightedResiduals)) {
    legends <- c(legends, "Weighted Residuals")
    colors <- c(colors, "red")
    pch_values <- c(pch_values, 17)
  }

  if (!all(residualsData$residuals == residualsData$robustWeightedResiduals)) {
    legends <- c(legends, "Robust Weighted Residuals")
    colors <- c(colors, "blue")
    pch_values <- c(pch_values, 18)
  }

  if (!is.na(legpos)) {
    graphics::legend(legpos, legend = legends, col = colors, pch = pch_values)
  }
}

#' Constructs Model Cost Summary for Error Handling
#'
#' Creates a model cost summary compatible with the structure returned by
#' `.calculateCostMetrics`, filled with either infinite values (for simulation
#' failures) or zeros (for objective function failures).
#'
#' @param infinite Logical flag indicating if the structure should contain
#'   infinite values (TRUE) or zeros (FALSE).
#' @return A model cost summary structured identically to the output of
#'   `.calculateCostMetrics`, with fields for model cost, minimum log
#'   probability, statistical measures, and detailed residuals, tailored for
#'   failure scenarios or initial setup.
#' @keywords internal
.createErrorCostStructure <- function(infinite = FALSE) {
  if (infinite) {
    costValue <- Inf
    logProbability <- Inf
    nObservations <- 1
  } else {
    costValue <- 0
    logProbability <- 0
    nObservations <- 0
  }

  errorCostStructure <- list(
    modelCost = costValue,
    minLogProbability = logProbability,
    costVariables = data.frame(
      scaleFactor = 1,
      nObservations = nObservations,
      M3Contribution = costValue,
      SSR = costValue,
      weightedSSR = costValue,
      normalizedSSR = costValue,
      robustSSR = costValue
    ),
    residualDetails = data.frame(
      x = NA,
      yObserved = NA,
      ySimulated = NA,
      weight = NA,
      residuals = NA,
      weightedResiduals = NA,
      normalizedResiduals = NA,
      robustWeightedResiduals = NA
    )
  )
  class(errorCostStructure) <- "modelCost"
  return(errorCostStructure)
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
