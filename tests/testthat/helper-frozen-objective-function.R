# A reference objective function, which calculates the cost on the data
# frames of `DataCombined` objects. The tests compare the objective function,
# which calculates the cost on numeric vectors, with it by `identical()`.
#
# lintr skips the code (`nolint start` and `nolint end`). Do not change it: it
# is the reference of the tests. It calls these functions of the package:
# `.newModelCost()`, `.computeErrorWeights()`, `.calculateHuberWeights()`,
# `.calculateBisquareWeights()` and `.calculateCensoredContribution()`.
#
# The LLOQ rule of the reference for "lsq" replaces the simulated values below
# the lowest LLOQ of an output mapping by half that LLOQ, for all its observed
# values, while the objective function compares the values below the LLOQ
# with the LLOQ. The reference passes no transformed value of 0 to
# `.calculateCensoredContribution()`. So it equals the objective function for
# "lsq" when no observed value of an output mapping has an LLOQ, and for "m3"
# when no observed value with an LLOQ has a y offset. The tests compare with
# it only for such data, and for "lsq" with an LLOQ with the reference without
# the LLOQ. For "m3" with several LLOQs, the comparison shows that both pass
# the same data to `.calculateCensoredContribution()`.

# nolint start

# The objective function after the simulations, on data frames: the
# `DataCombined` objects of `.evaluate()`, their data frames in base units, the
# LLOQ rule, the log transformation, the data weights and the cost of every
# output mapping, summed over the output mappings. The observed data are read
# together with the simulated data, as on the first evaluation of a call. The
# simulations are run by `.runSimulations()` of the task, for the output
# mappings of `bootstrapSeed`.
#
# @param task A `ParameterIdentification` object whose batches are
#   initialized.
# @param currVals Vector of parameter values.
# @param bootstrapSeed Optional bootstrap seed (see `.getOutputMappings()`).
# @return The cost summary of all output mappings, a `modelCost` object.
frozenObjectiveFunction <- function(task, currVals, bootstrapSeed = NULL) {
  private <- task$.__enclos_env__$private
  outputMappings <- private$.getOutputMappings(bootstrapSeed)
  simulationResults <- private$.runSimulations(currVals)

  # `.evaluate()` after `runSimulationBatches()`
  obsVsPredList <- vector("list", length(outputMappings))
  for (idx in seq_along(outputMappings)) {
    obsVsPred <- ospsuite::DataCombined$new()
    currOutputMapping <- outputMappings[[idx]]
    # Find the simulation that is the parent of the output quantity
    simId <- .getSimulationContainer(currOutputMapping$quantity)$id
    # Construct group names out of output path and simulation id
    groupName <- currOutputMapping$quantity$path
    # `.runSimulations()` names the results by the simulation IDs
    resultObject <- simulationResults[[simId]][[1]]
    obsVsPred$addSimulationResults(
      resultObject,
      quantitiesOrPaths = currOutputMapping$quantity$path,
      names = groupName,
      groups = groupName
    )

    obsVsPred$addDataSets(
      currOutputMapping$observedDataSets,
      groups = groupName
    )
    # apply data transformations stored in corresponding `outputMapping`
    obsVsPred$setDataTransformations(
      forNames = names(outputMappings[[idx]]$observedDataSets),
      xOffsets = outputMappings[[idx]]$dataTransformations$xOffsets,
      xScaleFactors = outputMappings[[idx]]$dataTransformations$xFactors,
      yOffsets = outputMappings[[idx]]$dataTransformations$yOffsets,
      yScaleFactors = outputMappings[[idx]]$dataTransformations$yFactors
    )
    obsVsPredList[[idx]] <- obsVsPred
  }

  # `.objectiveFunction()` after `.evaluate()`
  # Evaluate cost per output mapping
  costSummaryList <- vector("list", length(outputMappings))
  for (idx in seq_along(outputMappings)) {
    df <- obsVsPredList[[idx]]$toDataFrame()

    # Convert all columns to base units for consistent residual calculation
    obsVsPredDf <- ospsuite:::.unitConverter(
      df,
      xUnit = ospsuite::getBaseUnit("Time"),
      yUnit = ospsuite::getBaseUnit(
        outputMappings[[idx]]$quantity$dimension
      )
    )
    # Apply LLOQ handling for LSQ
    if (
      private$.configuration$objectiveFunctionOptions$objectiveFunctionType ==
        "lsq"
    ) {
      # replace values < LLOQ with LLOQ/2 in simulated data
      if (sum(is.finite(obsVsPredDf$lloq)) > 0) {
        lloq <- min(obsVsPredDf$lloq, na.rm = TRUE)
        obsVsPredDf[
          (obsVsPredDf$dataType == "simulated" &
            obsVsPredDf$yValues < lloq),
          "yValues"
        ] <- lloq / 2
      }
    }

    # Apply log transformation if requested
    if (outputMappings[[idx]]$scaling == "log") {
      obsVsPredDf <- frozenApplyLogTransformation(obsVsPredDf)
    }

    # Assign weights from PIOutputMapping
    obsVsPredDf$weights <- NA_real_
    if (!is.null(outputMappings[[idx]]$dataWeights)) {
      weights <- outputMappings[[idx]]$dataWeights
      for (dataset in names(weights)) {
        obsVsPredDf$weights[obsVsPredDf$name == dataset] <- weights[[
          dataset
        ]]
      }
    }

    # Extract cost function options
    costControl <- private$.configuration$objectiveFunctionOptions
    costControl$scaling <- outputMappings[[idx]]$scaling
    ospsuite.utils::validateIsOption(
      options = costControl,
      validOptions = ObjectiveFunctionSpecs
    )

    # Compute cost for current output mapping
    costSummary <- frozenCalculateCostMetrics(
      df = obsVsPredDf,
      objectiveFunctionType = costControl$objectiveFunctionType,
      residualWeightingMethod = costControl$residualWeightingMethod,
      robustMethod = costControl$robustMethod,
      scaleVar = costControl$scaleVar,
      index = idx,
      linScaleCV = costControl$linScaleCV,
      logScaleSD = costControl$logScaleSD,
      scaling = costControl$scaling
    )

    costSummaryList[[idx]] <- costSummary
  }

  # Aggregate cost across all output mappings
  Reduce(frozenSummarizeCostLists, costSummaryList)
}

# The cost of one output mapping from a data frame of simulated and observed
# data (see `.calculateCostMetrics()` of the package for the arguments).
frozenCalculateCostMetrics <- function(
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
    return(frozenCreateErrorCostStructure(index = index))
  }

  return(modelCost)
}

# An infinite-cost `modelCost` object.
frozenCreateErrorCostStructure <- function(index = NA_real_) {
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

# Transforms the `yValues` and
# `lloq` columns of a data frame of observed and simulated data (a `tbl_df`
# with `yDimension`, `yUnit`, `yValues` and `lloq`) with a log transformation
# of the given base.
frozenApplyLogTransformation <- function(df, base = exp(1)) {
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

# Sums the model costs, minimum log
# probabilities and cost variables of two cost summaries and binds their
# residual details by rows.
frozenSummarizeCostLists <- function(list1, list2) {
  mergedList <- list(
    modelCost = list1$modelCost + list2$modelCost,
    minLogProbability = list1$minLogProbability + list2$minLogProbability,
    costVariables = list1$costVariables + list2$costVariables,
    residualDetails = rbind(list1$residualDetails, list2$residualDetails)
  )
  class(mergedList) <- class(list1)

  return(mergedList)
}

# nolint end
