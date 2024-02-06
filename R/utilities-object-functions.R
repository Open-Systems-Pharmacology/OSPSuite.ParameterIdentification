#' @name calculateCostMetrics
#' @title Calculate Cost Metrics for Model Evaluation
#'
#' @description
#' This function calculates various cost metrics to evaluate the fit of a model
#' by comparing simulated data against observed data. It supports different
#' methods for weighting residuals.
#'
#' @param df A dataframe containing the combined data for simulation and
#' observation. Supports dataframes created from a `DataCombined` object via
#' `DataCombined$toDataFrame()`. The dataframe must include columns for
#' "dataType", "xValues", "yValues", and optionally "yErrorValues" if the
#' `residualWeightingMethod` is set to "error".
#' @param residualWeightingMethod A string indicating the method to weight the
#' residuals. Options include "none" (default), "std", "mean", and "error".
#' @param scaleVar A boolean indicating whether to scale residuals by the
#' number of observations. Defaults to `FALSE`.
#' @param ... Additional arguments. Currently not used.
#'
#' @details
#' The function calculates the residuals between the simulated and observed
#' values, applies the specified weighting method, and computes the cost metrics
#' based on the scaled or unscaled residuals.
#'
#' @return
#' A list containing the following elements:
#' - `modelCost`: The total cost calculated from the scaled sum of squared residuals.
#' - `minLogProbability`: The minimum log probability indicating the model fit.
#' - `costDetails`: A dataframe with details on the cost calculations.
#' - `residualDetails`: A dataframe with the calculated residuals and their weights.
#' The list has the class `modelCost` for easy identification and further processing.
#'
#' @examples
#' \dontrun{
#'
#' # Assuming DataCombined is a valid ospsuite DataCombined object
#' df <- DataCombined$toDataFrame()
#'
#' # Calculate cost metrics
#' costMetrics <- calculateCostMetrics(df, residualWeightingMethod = "std", scaleVar = TRUE)
#'
#' # View model cost
#' print(costMetrics$modelCost)
#'
#' @export
calculateCostMetrics <- function(df, residualWeightingMethod = "none", scaleVar = FALSE, ...) {
  # Validate input dataframe structure
  ospsuite.utils::validateIsOfType(df, "tbl_df")
  ospsuite.utils::validateIsIncluded(
    c("dataType", "xDimension", "yDimension", "xValues", "yValues", "xUnit", "yUnit"),
    colnames(df)
  )
  ospsuite.utils::validateIsOfLength(unique(df$xUnit), 1)
  ospsuite.utils::validateIsOfLength(unique(df$yUnit), 1)
  ospsuite.utils::validateIsIncluded(unique(df$xDimension), "Time")

  # Ensure the residual weighting method is recognized
  ospsuite.utils::validateEnumValue(residualWeightingMethod, residualWeightingOptions)
  if (residualWeightingMethod == "error") {
    ospsuite.utils::validateIsIncluded(c("yErrorValues", "yErrorUnit"), colnames(df))
  }

  # Handle infinite values by converting them to NA for accurate calculations
  df$xValues[df$xValues == Inf | df$xValues == -Inf] <- NA
  df$yValues[df$yValues == Inf | df$yValues == -Inf] <- NA
  idx <- is.na(df$xValues) & is.na(df$yValues)
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

  # Extracting values for interpolation or direct matching
  simulatedXVal <- simulatedData[["xValues"]]
  simulatedYVal <- simulatedData[["yValues"]]
  observedXVal <- observedData[["xValues"]]
  observedYVal <- observedData[["yValues"]]

  # Interpolating simulated Y values based on observed X values if applicable
  if (length(unique(simulatedXVal)) > 1) {
    simulatedYValApprox <- approx(simulatedXVal, simulatedYVal, xout = observedXVal)$y
  } else {
    simulatedYValApprox <- simulatedYVal[match(observedXVal, simulatedXVal)]
  }

  # Determining the method for residual weighting
  observedYErr <-
    switch(residualWeightingMethod,
           "none" = 1,
           "error" = { observedData[["yErrorValues"]] |>
               (\(x) replace(x, is.na(x), 1))() },
           "std" = {
             if (length(unique(observedYVal)) == 1) {
               observedYErr <- sqrt(.Machine$double.eps)
             } else {
               observedYErr <- sd(observedYVal)
             }
           },
           "mean" = {
             meanVal <- mean(abs(observedYVal))
             if (meanVal == 0) 1 else meanVal
           }
    )

  # Scaling residuals by the number of observations if requested
  scaleFactor <- if (scaleVar) 1 / length(observedYVal) else 1

  # Calculating and organizing residuals
  rawResiduals <- simulatedYValApprox - observedYVal
  weightedResiduals <- rawResiduals / observedYErr
  normalizedResiduals <- weightedResiduals * scaleFactor

  residualsData <- data.frame(
    x = observedXVal,
    yObserved = observedYVal,
    ySimulated = simulatedYValApprox,
    weight = 1 / observedYErr,
    residuals = rawResiduals,
    weightedResiduals = weightedResiduals,
    normalizedResiduals = normalizedResiduals
  )

  # Compiling cost variables based on residuals
  costVariables <- data.frame(
    scaleFactor = scaleFactor,
    nObservations = length(rawResiduals),
    SSR = sum(rawResiduals^2),
    weightedSSR = sum(weightedResiduals^2),
    normalizedSSR = sum(normalizedResiduals^2)
  )

  # Calculating log probability to evaluate model fit
  logProbability <- -sum(log(pmax(0, dnorm(
    residualsData$ySimulated, residualsData$yObserved, 1 / residualsData$weight))))

  # Organizing output with model evaluation metrics
  modelCost <- list(
    modelCost = costVariables$normalizedSSR,
    minLogProbability = logProbability,
    costVariables = costVariables,
    residualDetails = residualsData
  )

  class(modelCost) <- "modelCost"
  return(modelCost)
}

## -----------------------------------------------------------------------------
## S3 methods of modCost
## -----------------------------------------------------------------------------

plot.modCost <- function(x, legpos = "topleft", ...) {
  nvar <- nrow(x$var)

  dots <- list(...)

  dots$xlab <- if (is.null(dots$xlab)) "x" else dots$xlab
  dots$ylab <- if (is.null(dots$ylab)) "weighted residuals" else dots$ylab
  DotsPch <- if (is.null(dots$pch)) (16:24) else dots$pch
  dots$pch <- if (is.null(dots$pch)) rep(16:24, length.out = nvar)[x$residuals$name] else rep(dots$pch, length.out = nvar)[x$residuals$name] # Tom 02/01/2012: changed (16:24)  to rep(16:24,length.out=nvar); same for dots$pch in else part
  DotsCol <- if (is.null(dots$col)) (1:nvar) else dots$col
  dots$col <- if (is.null(dots$col)) (1:nvar)[x$residuals$name] else dots$col[x$residuals$name]

  do.call("plot", c(alist(x$residuals$x, x$residuals$res), dots))

  #  plot(x$residuals$x, x$residuals$res, xlab="x", ylab="weighted residuals",
  #     pch=c(16:24)[x$residuals$name],col=c(1:nvar)[x$residuals$name],...)

  if (!is.na(legpos)) {
    legend(legpos, legend = x$var$name, col = DotsCol, pch = DotsPch)
  }
}

#' Handle Simulation Failure
#'
#' This function checks for NA in simulation results and returns an infinite cost structure if any are found.
#' @param simulationResults A list of simulation results.
#' @return Returns an infinite cost structure if NA values are found in the simulation results, NULL otherwise.
#' @keywords internal
.handleSimulationFailure <- function(simulationResults) {
  if (any(is.na(simulationResults))) {
    return(.createErrorCostStructure(infinite = TRUE))
  }
  return(NULL)
}

#' Create Infinite Cost Structure
#'
#' Generates a cost structure with infinite values, used in cases of simulation failure.
#' @return Returns a list with infinite values for model, minlogp, var, and residuals components.
#' @keywords internal
.createErrorCostStructure <- function(infinite = FALSE) {
  if (infinite) {
    costValue <- Inf
    nValue <- 1
  } else {
    costValue <- 0
    nValue <- 0
  }

  errorCostStructure <- list(
    model = costValue,
    minlogp = costValue,
    var = data.frame(
      name = "Values", scale = 1, N = nValue,
      SSRUnweighted = costValue, SSRUnscaled = costValue, SSR = costValue
    ),
    residuals = data.frame(
      name = "Values", x = 0, obs = 0, mod = costValue,
      weight = 1, resUnweighted = costValue, res = costValue
    )
  )
  class(errorCostStructure) <- "modCost"
  return(errorCostStructure)
}

#' Apply Log Transformation to Data Frame
#'
#' Transforms the `yValues` and `lloq` columns in the given data frame using a log
#' transformation. Currently, this function only supports `obsVsPredDf` data frames,
#' which must contain `yDimension`, `yUnit`, `yValues`, and `lloq` columns.
#'
#' @param df A `tbl_df` representing the observed vs predicted data frame (`obsVsPredDf`).
#'
#' @return A transformed data frame with log-transformed `yValues` and `lloq`.
#' @keywords internal
#'
#' @examples
#' # Assuming df is a valid obsVsPredDf data frame
#' transformedDf <- applyLogTransformation(df)
.applyLogTransformation <- function(df) {
  ospsuite.utils::validateIsOfType(df, "tbl_df")
  ospsuite.utils::validateIsIncluded(
    c("yDimension", "yUnit", "yValues", "lloq"), colnames(df)
  )

  UNITS_EPSILON <- ospsuite::toUnit(
    quantityOrDimension = df$yDimension[1],
    values = ospsuite::getOSPSuiteSetting("LOG_SAFE_EPSILON"),
    targetUnit = df$yUnit[1],
    molWeight = 1
  )

  df$yValues <- ospsuite.utils::logSafe(
    df$yValues, epsilon = UNITS_EPSILON, base = exp(1)
  )
  df$lloq <- ospsuite.utils::logSafe(
    df$lloq, epsilon = UNITS_EPSILON, base = exp(1)
  )

  return(df)
}

#' Calculate Censored Error
#'
#' @param df Data frame with specific structure and columns.
#' @param censoredError Data frame to append results, or NULL.
#' @param scaling Character, scaling method.
#' @param cvM3 Numeric, coefficient of variation for linear scaling.
#' @param sdForLogCV Numeric, standard deviation for log scaling.
#' @return Data frame with censored error calculations.
#' @keywords internal
#' @examples
#' \dontrun{
#' .calculateCensoredError(df, scaling = "lin", cvM3 = 0.2)
#' }
.calculateCensoredError <- function(df, censoredError = NULL, scaling,
                                    cvM3 = NULL, sdForLogCV = NULL) {
  ospsuite.utils::validateIsOfType(df, "tbl_df")
  ospsuite.utils::validateIsIncluded(
    c("dataType", "lloq", "yValues", "xValues", "xUnit", "xDimension"),
    colnames(df)
  )
  ospsuite.utils::validateEnumValue(scaling, ScalingOptions)
  # Add evaluation logic for lloq values in df
  # LLOQ <- unique(df$lloq[!is.na(df$lloq)])

  simulated <- df[df$dataType == "simulated", ]
  observed <- df[df$dataType == "observed", ]

  # Distinguishing between uncensored and censored observations based on lloq values
  observedUncensored <- observed[is.na(observed$lloq) | (observed$yValues > observed$lloq), ]
  observedCensored <- observed[!is.na(observed$lloq) & (observed$yValues <= observed$lloq), ]
  simulatedUncensored <- merge(observedUncensored[c("xValues", "xUnit", "xDimension")],
                               simulated, by = c("xValues", "xUnit", "xDimension"), all.x = TRUE)
  simulatedCensored <- merge(observedCensored[c("xValues", "xUnit", "xDimension")],
                             simulated, by = c("xValues", "xUnit", "xDimension"), all.x = TRUE)

  # Calculate standard deviation based on the scaling method
  if (scaling == "lin") {
    ospsuite.utils::validateIsNumeric(cvM3)
    sd <- abs(cvM3 * observedCensored$lloq)
  } else if (scaling == "log") {
    ospsuite.utils::validateIsNumeric(sdForLogCV)
    sd <- sdForLogCV
  }

  if (isTRUE(nrow(observedCensored) > 0)) {
    # Calculate the probabilities for censored data points
    censoredProbabilities <- pnorm((observedCensored$lloq - simulatedCensored$yValues) / sd)
    censoredProbabilities[censoredProbabilities == 0] <- .Machine$double.xmin
    # Using logarithmic transformation for censored residuals as per the M3 method
    censoredErrorVector <- -2 * log(censoredProbabilities, base = 10)
    # Transforming residuals to match the expected format for subsequent analysis
    censoredErrorVector <- sqrt(censoredErrorVector)

    censoredError <- rbind(censoredError,
                           data.frame(
                             name = "Values",
                             x = observedCensored$xValues,
                             obs = observedCensored$yValues,
                             mod = simulatedCensored$yValues,
                             weight = 1,
                             resUnweighted = censoredErrorVector,
                             res = censoredErrorVector
                           )
    )
  }

  return(censoredError)
}

#' Summarize Cost Lists
#'
#' This function takes two lists, each being the output of the `calculateCostMetrics` function,
#' and summarizes them. It aggregates model costs and min log probabilities, and combines
#' cost and residual details by row-binding.
#'
#' @param list1 The first list, containing the output of the `calculateCostMetrics` function,
#'   which includes `modelCost`, `minLogProbability`, `costDetails`, and `residualDetails`.
#' @param list2 The second list, containing the output of the `calculateCostMetrics` function,
#'   which includes `modelCost`, `minLogProbability`, `costDetails`, and `residualDetails`.
#'
#' @return Returns a list that includes the sum of `modelCosts`, the sum of `minLogProbabilities`,
#'   a row-bound combination of `costDetails`, and a row-bound combination of `residualDetails`.
#'
#'
#' @keywords internal
.summarizeCostLists <- function(list1, list2) {
  # Sum modelCost
  list1$modelCost <- list1$modelCost + list2$modelCost

  # Sum minLogProbability
  list1$minLogProbability <- list1$minLogProbability + list2$minLogProbability

  # rbind costDetails
  list1$costDetails <- rbind(list1$costDetails, list2$costDetails)

  # rbind residualDetails
  list1$residualDetails <- rbind(list1$residualDetails, list2$residualDetails)

  return(list1)
}
