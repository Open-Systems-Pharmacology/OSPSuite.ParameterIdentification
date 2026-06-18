# .calculateCensoredContribution

obsVsPredDf <- readr::read_csv(
  getTestDataFilePath("Aciclovir_obsVsPredDf.csv"),
  show_col_types = FALSE
)

obsDf <- obsVsPredDf[obsVsPredDf$dataType == "observed", ]
predDf <- obsVsPredDf[obsVsPredDf$dataType == "simulated", ]

test_that(".calculateCensoredContribution correctly calculates result with linear scaling", {
  obsDf$lloq <- 2.5
  result <- .calculateCensoredContribution(
    observed = obsDf,
    simulated = predDf,
    scaling = "lin",
    linScaleCV = 0.25
  )
  expect_equal(result, 0.545702, tolerance = 1e-4)
})

test_that(".calculateCensoredContribution correctly calculates result with logarithmic scaling", {
  obsVsPredDf$lloq <- 2.5
  obsVsPredDfLog <- .applyLogTransformation(obsVsPredDf)
  obsDfLog <- obsVsPredDfLog[obsVsPredDfLog$dataType == "observed", ]
  predDfLog <- obsVsPredDfLog[obsVsPredDfLog$dataType == "simulated", ]
  result <- .calculateCensoredContribution(
    observed = obsDfLog,
    simulated = predDfLog,
    scaling = "log",
    logScaleSD = 0.086
  )
  expect_equal(result, 0.437086, tolerance = 1e-4)
})

test_that(".calculateCensoredContribution can handle a minimal dataset with a single censored observation", {
  obsDf$lloq <- 2.5
  obsDfSingle <- obsDf[1, , drop = FALSE]
  predDfSingle <- predDf[1, , drop = FALSE]
  result <- .calculateCensoredContribution(
    observed = obsDfSingle,
    simulated = predDfSingle,
    scaling = "lin",
    linScaleCV = 0.2
  )
  expect_equal(result, 0, tolerance = 1e-4)
})

test_that(".calculateCensoredContribution throws an error when LLOQ values are missing in the observed data", {
  obsDf$lloq <- NA
  expect_error(
    result <- .calculateCensoredContribution(
      observed = obsDf,
      simulated = predDf,
      scaling = "lin",
      linScaleCV = 0.2
    )
  )
  obsDf$lloq <- NULL
  expect_error(
    result <- .calculateCensoredContribution(
      observed = obsDf,
      simulated = predDf,
      scaling = "lin",
      linScaleCV = 0.2
    )
  )
})

test_that(".calculateCensoredContribution throws errors on invalid options", {
  obsDf$lloq <- 2.5
  expect_error(
    result <- .calculateCensoredContribution(
      observed = obsDf,
      simulated = predDf,
      scaling = "invalidOption",
      linScaleCV = 0.2
    )
  )
  expect_error(
    result <- .calculateCensoredContribution(
      observed = obsDf,
      simulated = predDf,
      scaling = "lin",
      logScaleSD = 0.086
    )
  )
  expect_error(
    result <- .calculateCensoredContribution(
      observed = obsDf,
      simulated = predDf,
      scaling = "log",
      linScaleCV = 0.2
    )
  )
})

# .newModelCost

test_that(".newModelCost builds the canonical schema with index owned by the constructor", {
  result <- .newModelCost(
    modelCost = 12.5,
    minLogProbability = 7.25,
    nObservations = 3,
    weightedSSR = 12.5,
    rawSSR = 14.0,
    M3Contribution = 1.5,
    x = c(1, 2, 3),
    yObserved = c(10, 8, 6),
    ySimulated = c(11, 7, 5),
    scaleFactor = c(1, 1, 1),
    errorWeights = c(1, 1, 1),
    robustWeights = c(1, 1, 1),
    userWeights = c(1, 1, 1),
    totalWeights = c(1, 1, 1),
    rawResiduals = c(1, -1, -1),
    weightedResiduals = c(1, -1, -1),
    index = 2
  )

  expect_s3_class(result, "modelCost")
  expect_equal(
    names(result),
    c("modelCost", "minLogProbability", "costVariables", "residualDetails")
  )
  expect_equal(
    names(result$costVariables),
    c("nObservations", "M3Contribution", "rawSSR", "weightedSSR")
  )
  expect_equal(
    names(result$residualDetails),
    c(
      "index",
      "x",
      "yObserved",
      "ySimulated",
      "scaleFactor",
      "errorWeights",
      "robustWeights",
      "userWeights",
      "totalWeights",
      "rawResiduals",
      "weightedResiduals"
    )
  )
  expect_equal(result$residualDetails$index, c(2, 2, 2))
})

test_that(".newModelCost fills a single NA residual row when per-observation vectors are omitted", {
  result <- .newModelCost(
    modelCost = Inf,
    minLogProbability = Inf,
    nObservations = 1,
    weightedSSR = Inf
  )

  expect_equal(nrow(result$residualDetails), 1)
  expect_true(is.na(result$residualDetails$x))
  expect_true(is.na(result$residualDetails$index))
})

# .createErrorCostStructure

test_that(".createErrorCostStructure shares the canonical schema with kernel output", {
  kernelOut <- .calculateCostMetrics(obsVsPredDf)
  errorOut <- .createErrorCostStructure()

  expect_equal(
    names(errorOut$costVariables),
    names(kernelOut$costVariables)
  )
  expect_equal(
    names(errorOut$residualDetails),
    names(kernelOut$residualDetails)
  )
  expect_equal(errorOut$modelCost, Inf)
})

test_that(".summarizeCostLists aggregates a kernel output and an error structure without error", {
  kernelOut <- .calculateCostMetrics(obsVsPredDf)
  errorOut <- .createErrorCostStructure()

  merged <- .summarizeCostLists(kernelOut, errorOut)

  expect_equal(merged$modelCost, kernelOut$modelCost + Inf)
  expect_equal(
    nrow(merged$residualDetails),
    nrow(kernelOut$residualDetails) + nrow(errorOut$residualDetails)
  )
  expect_equal(
    merged$costVariables$nObservations,
    kernelOut$costVariables$nObservations + errorOut$costVariables$nObservations
  )
})

# .applyLogTransformation

test_that(".applyLogTransformation correctly log-transforms `yValues` and `lloq`", {
  obsVsPredDfLog <- .applyLogTransformation(obsVsPredDf)
  expect_snapshot_value(
    obsVsPredDfLog$yValues,
    style = "deparse",
    tolerance = 1e-5
  )
  expect_snapshot_value(
    obsVsPredDfLog$lloq,
    style = "deparse",
    tolerance = 1e-5
  )
})

# .calculateHuberWeights

test_that(".calculateHuberWeights calculates correct weights for standard residuals", {
  residuals <- c(-2, -1, 0, 1, 2)
  expected_weights <- c(1, 1, 1, 1, 1)
  expect_equal(
    .calculateHuberWeights(residuals),
    expected_weights,
    tolerance = 0.01
  )
})

test_that(".calculateHuberWeights returns empty vector for empty residuals", {
  residuals <- numeric(0)
  expect_equal(.calculateHuberWeights(residuals), logical(0))
})

# .calculateBisquareWeights

test_that(".calculateBisquareWeights calculates correct weights for standard residuals", {
  residuals <- c(-2, -1, 0, 1, 2)
  expected_weights <- c(0.841, 0.959, 1, 0.959, 0.841)
  expect_equal(
    .calculateBisquareWeights(residuals),
    expected_weights,
    tolerance = 0.01
  )
})

test_that(".calculateBisquareWeights returns empty vector for empty residuals", {
  residuals <- numeric(0)
  expect_equal(.calculateHuberWeights(residuals), logical(0))
})

# calculateCostMetrics

test_that("calculateCostMetrics returns expected cost metrics for valid input data and default parameters", {
  result <- .calculateCostMetrics(obsVsPredDf)
  expect_s3_class(result, "modelCost")
  expect_true(all(
    c("modelCost", "minLogProbability", "costVariables", "residualDetails") %in%
      names(result)
  ))
})

test_that("calculateCostMetrics returns correct cost metric values for default parameters", {
  result <- .calculateCostMetrics(obsVsPredDf)
  expect_snapshot_value(result, style = "deparse")
})

test_that("calculateCostMetrics with residualWeightingMethod `none` returns expected results", {
  result <- .calculateCostMetrics(obsVsPredDf, residualWeightingMethod = "none")
  expect_s3_class(result, "modelCost")
  expect_snapshot_value(result$modelCost, tolerance = 1e-3)
})

test_that("calculateCostMetrics rejects removed weighting methods std and mean", {
  expect_error(
    .calculateCostMetrics(obsVsPredDf, residualWeightingMethod = "std"),
    regexp = "std"
  )
  expect_error(
    .calculateCostMetrics(obsVsPredDf, residualWeightingMethod = "mean"),
    regexp = "mean"
  )
})

test_that("calculateCostMetrics with residualWeightingMethod `error` returns expected results", {
  # ArithmeticStdDev
  resultArith <- .calculateCostMetrics(
    obsVsPredDf,
    residualWeightingMethod = "error"
  )
  expect_snapshot_value(resultArith$modelCost, tolerance = 1e-3)

  # GeometricStdDev: convert ArithSD to exact lognormal-equivalent GSD values
  # GSD = exp(sqrt(log(1 + CV^2))) where CV = arithSD / mean
  obsVsPredDfGeom <- obsVsPredDf
  obs <- obsVsPredDfGeom[obsVsPredDfGeom$dataType == "observed", ]
  validIdx <- !is.na(obs$yErrorValues) & obs$yErrorValues > 0
  obs$yErrorValues[validIdx] <- exp(
    sqrt(log(1 + (obs$yErrorValues[validIdx] / obs$yValues[validIdx])^2))
  )
  obsVsPredDfGeom[obsVsPredDfGeom$dataType == "observed", ] <- obs
  obsVsPredDfGeom$yErrorType <- "GeometricStdDev"
  resultGeom <- .calculateCostMetrics(
    obsVsPredDfGeom,
    residualWeightingMethod = "error"
  )
  expect_equal(resultArith$modelCost, resultGeom$modelCost, tolerance = 1e-3)
})

test_that("robust methods (huber, bisquare) modify the residuals appropriately", {
  resultHuber <- .calculateCostMetrics(obsVsPredDf, robustMethod = "huber")
  resultBisquare <- .calculateCostMetrics(
    obsVsPredDf,
    robustMethod = "bisquare"
  )
  expect_equal(resultHuber$modelCost, 8.94396, tolerance = 1e-4)
  expect_equal(resultBisquare$modelCost, 4.929464, tolerance = 1e-4)
})

test_that("least squares and M3 methods produce different model costs", {
  obsVsPredDf$lloq <- 2.5
  result_lsq <- .calculateCostMetrics(
    obsVsPredDf,
    objectiveFunctionType = "lsq"
  )
  result_m3 <- .calculateCostMetrics(
    obsVsPredDf,
    objectiveFunctionType = "m3",
    scaling = "lin",
    linScaleCV = 0.2
  )
  expect_true(result_lsq$modelCost != result_m3$modelCost)
})

test_that("calculateCostMetrics correctly scales residuals when scaleVar is TRUE", {
  result_scaled <- .calculateCostMetrics(obsVsPredDf, scaleVar = TRUE)
  result_unscaled <- .calculateCostMetrics(obsVsPredDf, scaleVar = FALSE)
  expect_true(result_scaled$modelCost != result_unscaled$modelCost)
})

test_that("calculateCostMetrics handles infinite values in xValues and yValues correctly", {
  obsVsPredDfInf <- obsVsPredDf
  obsVsPredDfInf$xValues[1] <- Inf
  obsVsPredDfInf$yValues[1] <- -Inf
  expect_silent(.calculateCostMetrics(obsVsPredDfInf))
})

# observed-data caching

currStartValues <- function(task) {
  vapply(task$parameters, function(p) p$startValue, numeric(1))
}

test_that(".evaluate omits observed data when includeObserved = FALSE", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()

  dcList <- priv$.evaluate(currStartValues(task), includeObserved = FALSE)
  df <- dcList[[1]]$toDataFrame()

  expect_true("simulated" %in% df$dataType)
  expect_false("observed" %in% df$dataType)
})

test_that(".evaluate includes observed data by default", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()

  dcList <- priv$.evaluate(currStartValues(task))
  df <- dcList[[1]]$toDataFrame()

  expect_true(all(c("simulated", "observed") %in% df$dataType))
})

test_that("objective function builds an observed-data cache and reuses it", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  currVals <- currStartValues(task)

  expect_null(priv$.obsVsPredDfCache)

  cost1 <- priv$.objectiveFunction(currVals)
  expect_false(is.null(priv$.obsVsPredDfCache))

  cost2 <- priv$.objectiveFunction(currVals)
  expect_equal(cost2$modelCost, cost1$modelCost)
})

test_that("cached observed rows match the observed rows of a full evaluation", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  currVals <- currStartValues(task)

  full <- priv$.evaluate(currVals, includeObserved = TRUE)[[1]]$toDataFrame()
  fullObs <- full[full$dataType == "observed", , drop = FALSE]

  priv$.objectiveFunction(currVals)
  cached <- priv$.obsVsPredDfCache[[1]]

  expect_equal(
    as.data.frame(cached, stringsAsFactors = FALSE),
    as.data.frame(fullObs, stringsAsFactors = FALSE)
  )
})

test_that("observed-data cache is invalidated when the bootstrap seed changes", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  currVals <- currStartValues(task)

  priv$.gprModels <- .prepareGPRModels(priv$.outputMappings)
  priv$.objectiveFunction(currVals, bootstrapSeed = 1L)
  expect_false(is.null(priv$.obsVsPredDfCache))

  priv$.getOutputMappings(bootstrapSeed = 2L)
  expect_null(priv$.obsVsPredDfCache)
})

# .computeErrorWeights

test_that(".computeErrorWeights uses exact lognormal SD formula for GeometricStdDev", {
  yValues <- c(10, 20)
  yErrorValues <- c(2.0, 3.0)
  yErrorType <- c("GeometricStdDev", "GeometricStdDev")

  result <- .computeErrorWeights(yValues, yErrorValues, yErrorType)

  # GSD=2, mean=10: sigma=ln(2)=0.6931, exp(sigma^2)-1=0.617, SD=10*sqrt(0.617)=7.854
  # GSD=3, mean=20: sigma=ln(3)=1.0986, exp(sigma^2)-1=2.343, SD=20*sqrt(2.343)=30.62
  expect_equal(result, c(1 / 7.854, 1 / 30.62), tolerance = 1e-3)
})

test_that(".computeErrorWeights warns when error weighting falls back to unit weights", {
  yValues <- c(5, 10)
  yErrorValues <- c(0, 0)
  yErrorType <- c("ArithmeticStdDev", "ArithmeticStdDev")

  expect_warning(
    .computeErrorWeights(yValues, yErrorValues, yErrorType),
    regexp = "unit weights"
  )
})

test_that(".computeErrorWeights warns when some GSD error values are invalid", {
  yValues <- c(10, 20, 15)
  yErrorValues <- c(2.0, 0.5, 3.0)
  yErrorType <- c("GeometricStdDev", "GeometricStdDev", "GeometricStdDev")

  expect_warning(
    .computeErrorWeights(yValues, yErrorValues, yErrorType),
    regexp = "unit weights"
  )
})
