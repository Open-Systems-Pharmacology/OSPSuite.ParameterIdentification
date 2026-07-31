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
#' @param blqMethod A string selecting how retained BLQ observations contribute
#'   to the cost. `"lloq"`/`"lloqHalf"` substitute the observed BLQ value with
#'   the LLOQ (or half the LLOQ) before the least-squares term; the simulated
#'   prediction is never modified. `"m3"` excludes BLQ rows from the
#'   least-squares term and instead adds their censored-likelihood
#'   contribution; any other value applies no substitution or censored
#'   handling.
#' @param residualWeightingMethod A string indicating the method to weight the
#'   residuals. Options include `"none"` (default) and `"error"`.
#' @param robustMethod A string indicating the robust method to apply to the
#'   residuals. Options include `"none"` (default), `"huber"`, and `"bisquare"`.
#' @param scaleVar A boolean indicating whether to scale residuals by the number
#'   of observations. Defaults to `FALSE`.
#' @param index Output-mapping index stored on every `residualDetails` row.
#'   Defaults to `NA_real_`.
#' @param scaling Character string specifying the scaling method for the BLQ
#'   substitution target (lin vs log) for non-m3 methods and the censored
#'   likelihood calculation. Defaults to `"lin"`.
#' @param linScaleCV Numeric, coefficient of variation used in linear scaling
#'   for the censored contribution calculation. Defaults to `NULL`.
#' @param logScaleSD Numeric, standard deviation used in logarithmic scaling
#'   for the censored contribution calculation. Defaults to `NULL`.
#' @param objectiveType A string naming the objective function type, one of
#'   [`ospsuite.parameteridentification::ObjectiveTypes`]. Stamped onto the
#'   returned `modelCost` object. Defaults to `"lsq"`.
#'
#' @details The function calculates the residuals between the simulated and
#' observed values, applies the specified weighting method, and computes the
#' cost metrics.
#'
#' @return A cost metrics summary list containing the following fields:
#' - `modelCost`: The total cost calculated from the scaled sum of squared residuals.
#' - `minLogProbability`: The minimum log probability indicating the model fit.
#' - `objectiveType`: The objective function type tag.
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
  blqMethod = "none",
  residualWeightingMethod = "none",
  robustMethod = "none",
  scaleVar = FALSE,
  index = NA_real_,
  scaling = "lin",
  linScaleCV = NULL,
  logScaleSD = NULL,
  objectiveType = "lsq"
) {
  ospsuite.utils::validateEnumValue(objectiveType, ObjectiveTypes)

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

  # BLQ substitution (blqMethod none/lloq/lloqHalf): substitute below-LLOQ
  # observed values against the per-point LLOQ. Observed-only; the simulated
  # prediction is never modified. Passthrough for none and m3.
  observedYVal <- .applyBlqSubstitution(
    observedYVal,
    observedData$lloq,
    blqMethod,
    scaling
  )
  # Write the substituted observed values back so downstream error-weighting
  # (which reads observedData$yValues) sees the same values as the residuals.
  observedData$yValues <- observedYVal

  # M3 censored handling: compute one shared BLQ mask, score the censored rows
  # via the censored likelihood, and exclude them from the least-squares term so
  # they are not double counted. A row whose own LLOQ is missing is not censored,
  # the same rule `blqRemove` applied upstream, so both stages classify the
  # identical row set. Only a mapping with no LLOQ at all is a misconfiguration.
  censoredContribution <- 0
  if (blqMethod == "m3") {
    if (all(is.na(observedData$lloq))) {
      stop("LLOQ value not provided with the data.")
    }
    censoredMask <- .isBlq(observedData)
    censoredContribution <- .calculateCensoredContribution(
      lloq = observedData$lloq[censoredMask],
      simulated = simulatedYValApprox[censoredMask],
      scaling = scaling,
      linScaleCV = linScaleCV,
      logScaleSD = logScaleSD
    )
    keep <- !censoredMask
    observedData <- observedData[keep, , drop = FALSE]
    observedXVal <- observedXVal[keep]
    observedYVal <- observedYVal[keep]
    simulatedYValApprox <- simulatedYValApprox[keep]
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
        # `.computeErrorWeights()` needs the untransformed observed value, which
        # under log scaling only `yValuesLinear` holds. Keyed on `scaling` rather
        # than on the column being present, so a log-scaled frame that never
        # went through `.applyLogTransformation()` fails loudly instead of
        # silently deriving the coefficient of variation from a log-scale value.
        yValues = if (scaling == "log") {
          observedData[["yValuesLinear"]]
        } else {
          observedData[["yValues"]]
        },
        yErrorValues = observedData[["yErrorValues"]],
        yErrorType = observedData[["yErrorType"]],
        scaling = scaling
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

  # Section 6: sigma_i is 1 / (scaleFactor * totalWeights_i) up to the error
  # model's scale, so sum(log(sigma_i)) is the negated sum below. A row whose
  # total weight is non-positive has infinite sigma and carries no likelihood
  # information, so it is dropped here rather than contributing -Inf. Under
  # `mle` such a row cannot occur, because the configuration rejects robust
  # weighting and non-positive dataset weights.
  appliedWeights <- scaleFactor * totalWeights
  sumLogSigma <- -sum(log(appliedWeights[appliedWeights > 0]))

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
    sumLogSigma = sumLogSigma,
    objectiveType = objectiveType,
    M3Contribution = censoredContribution,
    rawSSR = sum(rawResiduals^2),
    weightedSSR = weightedSSR,
    x = observedXVal,
    yObserved = observedYVal,
    ySimulated = simulatedYValApprox,
    scaleFactor = rep(scaleFactor, length.out = length(rawResiduals)),
    errorWeights = rep(
      round(errorWeights, 2),
      length.out = length(rawResiduals)
    ),
    robustWeights = round(robustWeights, 2),
    userWeights = userWeights,
    totalWeights = round(totalWeights, 2),
    rawResiduals = rawResiduals,
    weightedResiduals = weightedResiduals,
    index = rep(index, length.out = length(rawResiduals))
  )

  # Ensure that the modelCost calculation does not result in NA
  if (is.na(modelCost$modelCost)) {
    warning(
      "Invalid model cost detected (NA). Returning infinite error cost structure."
    )
    return(.createErrorCostStructure(
      index = index,
      objectiveType = objectiveType
    ))
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
#' @param sumLogSigma Negated sum of the log of the applied per-observation
#'   weights, the sufficient statistic the likelihood needs. Defaults to `0`,
#'   the additive identity, for the failure substitute.
#' @param objectiveType A string naming the objective function type, one of
#'   [`ospsuite.parameteridentification::ObjectiveTypes`]. Stamped onto the
#'   returned `modelCost` object. Defaults to `"lsq"`.
#' @param x,yObserved,ySimulated,scaleFactor,errorWeights,robustWeights,userWeights,totalWeights,rawResiduals,weightedResiduals
#'   Per-observation vectors forming `residualDetails`. Default to `NA_real_` for
#'   the failure substitute.
#' @param index Output-mapping index stored on every `residualDetails` row.
#' @return A `modelCost` object: a list with `modelCost`, `minLogProbability`,
#'   `objectiveType`, `costVariables`, and `residualDetails`.
#' @keywords internal
#' @noRd
.newModelCost <- function(
  modelCost,
  minLogProbability,
  nObservations,
  weightedSSR,
  rawSSR = NA_real_,
  M3Contribution = 0,
  sumLogSigma = 0,
  objectiveType = "lsq",
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
    weightedSSR = weightedSSR,
    sumLogSigma = sumLogSigma
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
      objectiveType = objectiveType,
      costVariables = costVariables,
      residualDetails = residualDetails
    ),
    class = "modelCost"
  )
}

#' Compute error-based residual weights
#'
#' @param yValues Vector of y-values, required for conversion. Must be the
#'   untransformed observed values even when the residuals are log-scaled.
#' @param yErrorValues Vector of y-value errors
#' @param yErrorType Vector of error type strings (`ArithmeticStdDev`,
#'   `GeometricStdDev`)
#' @param scaling Character string specifying the scale the residual is on
#'   (`"lin"` or `"log"`). Under `"log"`, the weight is the reciprocal of the
#'   log-scale standard deviation, so it standardizes a natural-log residual.
#'   Defaults to `"lin"`.
#' @param defaultWeight Fallback weight value when inputs are missing or invalid
#' @return Numeric vector of residual weights computed as 1 / StdDev
#'
#' @keywords internal
#' @noRd
.computeErrorWeights <- function(
  yValues,
  yErrorValues,
  yErrorType,
  scaling = "lin",
  defaultWeight = 1
) {
  ospsuite.utils::validateIsNumeric(yValues)
  ospsuite.utils::validateIsNumeric(yErrorValues)
  ospsuite.utils::validateIsCharacter(yErrorType)
  ospsuite.utils::validateEnumValue(scaling, ScalingOptions)
  ospsuite.utils::isSameLength(yValues, yErrorValues)
  ospsuite.utils::isSameLength(yValues, yErrorType)

  weights <- rep(defaultWeight, length(yValues))

  idxArith <- which(
    yErrorType == "ArithmeticStdDev" & yValues > 0 & yErrorValues > 0
  )
  if (length(idxArith) > 0) {
    weights[idxArith] <- if (scaling == "log") {
      # A residual in natural-log units needs the log-scale spread of a
      # lognormal observation with this coefficient of variation.
      cv <- yErrorValues[idxArith] / yValues[idxArith]
      1 / sqrt(log(1 + cv^2))
    } else {
      1 / yErrorValues[idxArith]
    }
  }

  idxGSD <- which(
    yErrorType == "GeometricStdDev" & yValues > 0 & yErrorValues > 1
  )
  if (length(idxGSD) > 0) {
    weights[idxGSD] <- if (scaling == "log") {
      # A geometric standard deviation is already a multiplicative spread.
      1 / log(yErrorValues[idxGSD])
    } else {
      # SD = mean * sqrt(e^(sigma^2) - 1), sigma = log(GSD)
      stDev <- yValues[idxGSD] * sqrt(exp(log(yErrorValues[idxGSD])^2) - 1)
      1 / stDev
    }
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
#' @param objectiveType A string naming the objective function type, one of
#'   [`ospsuite.parameteridentification::ObjectiveTypes`]. Stamped onto the
#'   returned `modelCost` object. Defaults to `"lsq"`.
#' @return A `modelCost` object filled with infinite cost values.
#' @keywords internal
#' @noRd
.createErrorCostStructure <- function(index = NA_real_, objectiveType = "lsq") {
  .newModelCost(
    modelCost = Inf,
    minLogProbability = Inf,
    nObservations = 1,
    weightedSSR = Inf,
    rawSSR = Inf,
    M3Contribution = Inf,
    sumLogSigma = 0,
    index = index,
    objectiveType = objectiveType
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
#'   The pre-transform observed values are preserved in `yValuesLinear`.
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

  df$yValuesLinear <- df$yValues
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
#' @param lloq Numeric vector of the per-row LLOQ for the censored rows only.
#' @param simulated Numeric vector of (interpolated) simulated values, aligned
#'   element-for-element with `lloq`.
#' @param scaling Character string specifying the scaling method; should be one
#'   of the predefined scaling options.
#' @param linScaleCV Numeric, coefficient used to calculate standard deviation
#'   for linear scaling, applied to 'lloq' values.
#' @param logScaleSD Numeric, standard deviation for logarithmic scaling,
#'   applied uniformly to all censored observations.
#' @return Numeric value representing the censored-likelihood contribution,
#'   contributing to the model's total cost.
#' @keywords internal
#' @examples
#' \dontrun{
#' .calculateCensoredContribution(lloq, simulated, scaling = "lin", linScaleCV = 0.2)
#' }
.calculateCensoredContribution <- function(
  lloq,
  simulated,
  scaling,
  linScaleCV = NULL,
  logScaleSD = NULL
) {
  ospsuite.utils::validateEnumValue(scaling, ScalingOptions)
  ospsuite.utils::validateIsNumeric(c(linScaleCV, logScaleSD))
  if (length(lloq) == 0) {
    return(0)
  }
  if (scaling == "lin" && !is.null(linScaleCV)) {
    stDev <- abs(linScaleCV * lloq)
  } else if (scaling == "log" && !is.null(logScaleSD)) {
    stDev <- logScaleSD
  } else {
    stop("Scaling method and scaling parameters are not compatible.")
  }
  censoredProbabilities <- stats::pnorm((lloq - simulated) / stDev)
  censoredProbabilities[censoredProbabilities == 0] <- .Machine$double.xmin
  sum(-2 * log(censoredProbabilities))
}

#' Summarize Cost Lists
#'
#' This function takes two lists, each being the output of the
#' `.calculateCostMetrics` function, and summarizes them. It aggregates model
#' costs, min log probabilities, and the cost variables, and combines the
#' residual details by row-binding.
#'
#' @param list1 The first list, containing the output of the
#'   `.calculateCostMetrics` function, which includes `modelCost`,
#'   `minLogProbability`, `objectiveType`, `costVariables`, and
#'   `residualDetails`.
#' @param list2 The second list, containing the output of the
#'   `.calculateCostMetrics` function, which includes `modelCost`,
#'   `minLogProbability`, `objectiveType`, `costVariables`, and
#'   `residualDetails`.
#'
#' @return Returns a list that includes the sum of `modelCosts`, the sum of
#'   `minLogProbabilities`, the `objectiveType` taken from `list1`, the
#'   element-wise sum of `costVariables` (both frames share one fixed single-row
#'   column set, so every statistic aggregates additively), and a row-bound
#'   combination of `residualDetails`.
#'
#' @keywords internal
#' @noRd
.summarizeCostLists <- function(list1, list2) {
  mergedList <- list(
    modelCost = list1$modelCost + list2$modelCost,
    minLogProbability = list1$minLogProbability + list2$minLogProbability,
    objectiveType = list1$objectiveType,
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
