# .calculateCensoredContribution

# The coefficient of variation whose log-scale SD sqrt(log(1 + cv^2)) is the
# value 0.086 the recorded log-scale results were calculated with
cvOfSd0086 <- sqrt(exp(0.086^2) - 1)

obsVsPredDf <- readr::read_csv(
  getTestDataFilePath("Aciclovir_obsVsPredDf.csv"),
  show_col_types = FALSE
)

test_that(".calculateCensoredContribution sums -2 ln P with the SD of each LLOQ on the linear scale", {
  lloq <- c(1, 1, 2, 2)
  simulated <- c(0.1, 0.9, 1.9, 0.2)
  expect_equal(
    .calculateCensoredContribution(lloq, simulated, "lin", cv = 0.2),
    sum(-2 * log(stats::pnorm((lloq - simulated) / (0.2 * lloq))))
  )
})

test_that(".calculateCensoredContribution uses the log-normal SD on the log scale", {
  lloq <- log(c(1, 2))
  simulated <- log(c(0.5, 3))
  expect_equal(
    .calculateCensoredContribution(lloq, simulated, "log", cv = 0.2),
    sum(-2 * log(stats::pnorm((lloq - simulated) / sqrt(log(1 + 0.2^2)))))
  )
})

test_that(".calculateCensoredContribution is 0 without censored values", {
  expect_identical(
    .calculateCensoredContribution(numeric(0), numeric(0), "lin", cv = 0.2),
    0
  )
})

test_that(".calculateCensoredContribution caps a probability of 0", {
  expect_equal(
    .calculateCensoredContribution(c(1, 1), c(1e6, 1e6), "lin", cv = 0.2),
    -2 * 2 * log(.Machine$double.xmin)
  )
})

test_that(".calculateCensoredContribution keeps a positive log-scale SD for a tiny cv", {
  # 1 + cv^2 rounds to 1 for cv = 1e-9, so the SD must not be computed from it.
  # A prediction at the LLOQ then has probability 0.5 of lying below it.
  expect_equal(
    .calculateCensoredContribution(log(1), log(1), "log", cv = 1e-9),
    -2 * log(0.5)
  )
})

# BLQ scoring in the kernel

# Simulated values at 0 to 3 h and observed values at 1, 2 and 3 h with an
# LLOQ of 1 mg/l. The simulated values at the observed times are 5, 1.5 and
# 3.5, and the observed value at 2 h is below the LLOQ.
blqDf <- function(observedX = c(1, 2, 3), observedY = c(5, 0.2, 4)) {
  tibble::tibble(
    dataType = rep(c("simulated", "observed"), c(4, 3)),
    xValues = c(0, 1, 2, 3, observedX),
    yValues = c(6, 5, 1.5, 3.5, observedY),
    xDimension = "Time",
    yDimension = "Concentration (mass)",
    xUnit = "h",
    yUnit = "mg/l",
    lloq = rep(c(NA, 1), c(4, 3)),
    weights = NA_real_
  )
}

test_that("blqMethod substitutes only the observed BLQ values", {
  for (case in list(
    list(blqMethod = "none", substitute = 0.2),
    list(blqMethod = "lloq", substitute = 1),
    list(blqMethod = "lloqHalf", substitute = 0.5)
  )) {
    cost <- .calculateCostMetrics(blqDf(), blqMethod = case$blqMethod)
    expect_identical(cost$residualDetails$yObserved, c(5, case$substitute, 4))
    expect_identical(cost$residualDetails$ySimulated, c(5, 1.5, 3.5))
    expect_equal(cost$modelCost, (1.5 - case$substitute)^2 + (3.5 - 4)^2)
  }
})

test_that("m3 keeps the weights and error values of the kept observations", {
  terms <- .costKernel(
    simulatedX = c(0, 1, 2, 3),
    simulatedY = c(6, 5, 1.5, 3.5),
    observedX = c(1, 2, 3),
    observedY = c(5, 0.2, 4),
    userWeights = c(2, 3, 4),
    yErrorValues = c(0.5, 0.1, 0.25),
    yErrorType = rep("ArithmeticStdDev", 3),
    residualWeightingMethod = "error",
    robustMethod = "none",
    scaleVar = FALSE,
    index = 1L,
    lloq = rep(1, 3),
    blqMethod = "m3",
    cv = 0.2,
    scaling = "lin",
    observedYLinear = c(5, 0.2, 4),
    lloqLinear = rep(1, 3)
  )
  expect_identical(terms$userWeights, c(2, 4))
  expect_identical(terms$errorWeights, c(2, 4))
  expect_identical(terms$yObserved, c(5, 4))
})

test_that("m3 on a mapping with only censored observations gives the censored cost", {
  allCensored <- blqDf(observedY = c(0.5, 0.2, 0.1))
  cost <- .calculateCostMetrics(allCensored, blqMethod = "m3")
  expect_equal(cost$costVariables$nObservations, 0)
  expect_equal(nrow(cost$residualDetails), 0)
  simulated <- c(5, 1.5, 3.5)
  expect_equal(
    cost$modelCost,
    sum(-2 * log(stats::pnorm((1 - simulated) / 0.2)))
  )
})

test_that("calculateCostMetrics uses the default cv of blqOptions", {
  expect_identical(
    .calculateCostMetrics(blqDf(), blqMethod = "m3"),
    .calculateCostMetrics(blqDf(), blqMethod = "m3", cv = BLQOptions$cv)
  )
})

test_that("BLQ substitution under log scaling gives error weights of the linear substituted value", {
  dataSet <- ospsuite::DataSet$new(name = "logError")
  dataSet$xUnit <- ospsuite::ospUnits$Time$min
  dataSet$yDimension <- ospsuite::ospDimensions$`Concentration (molar)`
  dataSet$yUnit <- "µmol/l"
  dataSet$setValues(
    xValues = c(60, 120, 180),
    yValues = c(20, 15, 1.5),
    yErrorValues = c(2, 2, 2)
  )
  dataSet$yErrorType <- ospsuite::DataErrorType$ArithmeticStdDev
  dataSet$yErrorUnit <- "µmol/l"
  dataSet$LLOQ <- 10
  mapping <- PIOutputMapping$new(quantity = testQuantity())
  mapping$addObservedDataSets(dataSet)
  costControl <- PIConfiguration$new()$objectiveFunctionOptions
  costControl$scaling <- "log"
  costControl$residualWeightingMethod <- "error"
  costControl$cv <- 0.2
  for (case in list(
    list(blqMethod = "lloqHalf", substitute = 5),
    list(blqMethod = "lloq", substitute = 10)
  )) {
    costControl$blqMethod <- case$blqMethod
    terms <- .mappingCostTerms(
      simulated = list(xValues = c(60, 120, 180), yValues = c(18, 14, 4)),
      observed = .prepareObservedData(mapping),
      dataWeights = NULL,
      costControl = costControl,
      index = 1L
    )
    expect_equal(terms$yObserved[[3]], log(case$substitute))
    expect_equal(
      terms$errorWeights[[3]],
      1 / sqrt(log(1 + (2 / case$substitute)^2))
    )
  }
})

test_that("m3 scores censored values once, outside the residuals", {
  cost <- .calculateCostMetrics(blqDf(), blqMethod = "m3", cv = 0.2)
  expect_identical(cost$residualDetails$x, c(1, 3))
  expect_equal(cost$costVariables$nObservations, 2)
  censored <- -2 * log(stats::pnorm((1 - 1.5) / 0.2))
  expect_equal(cost$costVariables$M3Contribution, censored)
  expect_equal(cost$modelCost, (5 - 5)^2 + (3.5 - 4)^2 + censored)
})

test_that("m3 scores a censored value at a time that is not a simulated time", {
  cost <- .calculateCostMetrics(
    blqDf(observedX = c(1, 2.123456789, 3)),
    blqMethod = "m3",
    cv = 0.2
  )
  expect_true(is.finite(cost$modelCost))
  expect_gt(cost$costVariables$M3Contribution, 0)
})

test_that("m3 under log scaling ignores the stored value of a censored observation", {
  dataSet <- ospsuite::DataSet$new(name = "storedZero")
  dataSet$xUnit <- ospsuite::ospUnits$Time$min
  dataSet$yDimension <- ospsuite::ospDimensions$`Concentration (molar)`
  dataSet$yUnit <- "µmol/l"
  dataSet$setValues(xValues = c(60, 120, 180), yValues = c(10, 5, 0))
  dataSet$LLOQ <- 2
  mapping <- PIOutputMapping$new(quantity = testQuantity())
  mapping$addObservedDataSets(dataSet)
  costControl <- PIConfiguration$new()$objectiveFunctionOptions
  costControl$scaling <- "log"
  costControl$blqMethod <- "m3"
  costControl$cv <- 0.2
  terms <- .mappingCostTerms(
    simulated = list(xValues = c(60, 120, 180), yValues = c(9, 4, 1)),
    observed = .prepareObservedData(mapping),
    dataWeights = NULL,
    costControl = costControl,
    index = 1L
  )
  expect_identical(terms$x, c(60, 120))
  expect_equal(
    terms$modelCost,
    log(9 / 10)^2 +
      log(4 / 5)^2 +
      -2 * log(stats::pnorm((log(2) - log(1)) / sqrt(log(1 + 0.2^2))))
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
  expect_equal(result$residualDetails$x, NA_real_)
  expect_equal(result$residualDetails$index, NA_real_)
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

test_that(".createErrorCostStructure stamps a non-NA index onto the residual row", {
  errorOut <- .createErrorCostStructure(index = 2)
  expect_equal(errorOut$residualDetails$index, 2)
})

test_that(".createErrorCostStructure counts no observations", {
  expect_equal(.createErrorCostStructure()$costVariables$nObservations, 0)
})

test_that("a failed output mapping adds no observations to the combined cost", {
  threeObservations <- .costKernel(
    simulatedX = c(0, 1, 2, 3),
    simulatedY = c(1, 2, 3, 2),
    observedX = c(0.5, 1.5, 2.5),
    observedY = c(1.4, 2.7, 2.2),
    userWeights = c(1, 1, 1),
    yErrorValues = NULL,
    yErrorType = NULL,
    residualWeightingMethod = "none",
    robustMethod = "none",
    scaleVar = FALSE,
    lloq = rep(NA_real_, 3),
    blqMethod = "none",
    cv = NULL,
    scaling = "lin",
    observedYLinear = c(1.4, 2.7, 2.2),
    lloqLinear = rep(NA_real_, 3),
    index = 1L
  )
  combined <- .combineCostTerms(
    list(threeObservations, .errorCostTerms(index = 2L))
  )
  expect_equal(combined$costVariables$nObservations, 3)
})

# .validateModelCost

test_that(".validateModelCost accepts the cost of the kernel and of a failure", {
  expect_no_error(.validateModelCost(.calculateCostMetrics(obsVsPredDf)))
  expect_no_error(.validateModelCost(.createErrorCostStructure()))
})

test_that(".validateModelCost stops on an object that is not a modelCost", {
  expect_snapshot(.validateModelCost(list(modelCost = 1)), error = TRUE)
})

test_that(".validateModelCost names the missing fields", {
  cost <- .createErrorCostStructure()
  cost$minLogProbability <- NULL
  cost$costVariables$weightedSSR <- NULL
  cost$residualDetails$totalWeights <- NULL
  expect_snapshot(.validateModelCost(cost), error = TRUE)
})

test_that(".validateModelCost names a missing table once, without its columns", {
  cost <- .createErrorCostStructure()
  cost$residualDetails <- NULL
  expect_snapshot(.validateModelCost(cost), error = TRUE)
})

# plot.modelCost

test_that("plot.modelCost shows only the raw series when weighting leaves residuals unchanged", {
  result <- .calculateCostMetrics(obsVsPredDf, index = 1)
  # Default weighting is inert, so weighted == raw and the overlay is skipped.
  expect_true(all(
    result$residualDetails$rawResiduals ==
      result$residualDetails$weightedResiduals
  ))
  expect_silent(plot.modelCost(result, legpos = NA))
})

test_that("plot.modelCost overlays the weighted series when weighting changes the residuals", {
  result <- .calculateCostMetrics(obsVsPredDf, robustMethod = "huber")
  # Mirror the predicate that gates the overlay: the weighted series is drawn
  # only when it differs from the raw series.
  expect_false(all(
    result$residualDetails$rawResiduals ==
      result$residualDetails$weightedResiduals
  ))
  vdiffr::expect_doppelganger(
    "model-cost-raw-and-weighted-residuals",
    function() plot.modelCost(result)
  )
})

test_that("plot.modelCost errors on a failed-evaluation cost object with no finite residuals", {
  errorCost <- .createErrorCostStructure()
  expect_true(all(is.na(errorCost$residualDetails$rawResiduals)))
  expect_error(
    plot.modelCost(errorCost),
    regexp = messages$errorNoResidualsToPlot(),
    fixed = TRUE
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

test_that("calculateCostMetrics stamps the supplied index onto every residual row", {
  result <- .calculateCostMetrics(obsVsPredDf, index = 7)
  expect_equal(
    result$residualDetails$index,
    rep(7, result$costVariables$nObservations)
  )
})

test_that("calculateCostMetrics returns the indexed error structure when the cost is NA", {
  # Push one observed time beyond the simulated range so interpolation yields
  # NA, which propagates to an NA model cost and triggers the fallback.
  obsVsPredDfNA <- obsVsPredDf
  firstObs <- which(obsVsPredDfNA$dataType == "observed")[1]
  obsVsPredDfNA$xValues[firstObs] <- max(obsVsPredDfNA$xValues) * 10

  expect_warning(
    result <- .calculateCostMetrics(obsVsPredDfNA, index = 5),
    regexp = "Invalid model cost"
  )
  expect_s3_class(result, "modelCost")
  expect_equal(result$modelCost, Inf)
  expect_equal(result$residualDetails$index, 5)
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

  # GeometricStdDev: the GSD whose SD is the arithmetic SD when y is the
  # geometric mean. With u = exp(sigma^2), CV^2 = u^2 - u, so
  # GSD = exp(sqrt(log((1 + sqrt(1 + 4 * CV^2)) / 2))) where CV = arithSD / y
  obsVsPredDfGeom <- obsVsPredDf
  obs <- obsVsPredDfGeom[obsVsPredDfGeom$dataType == "observed", ]
  validIdx <- !is.na(obs$yErrorValues) & obs$yErrorValues > 0
  cv <- obs$yErrorValues[validIdx] / obs$yValues[validIdx]
  obs$yErrorValues[validIdx] <- exp(sqrt(log((1 + sqrt(1 + 4 * cv^2)) / 2)))
  obsVsPredDfGeom[obsVsPredDfGeom$dataType == "observed", ] <- obs
  obsVsPredDfGeom$yErrorType <- "GeometricStdDev"
  resultGeom <- .calculateCostMetrics(
    obsVsPredDfGeom,
    residualWeightingMethod = "error"
  )
  expect_equal(resultArith$modelCost, resultGeom$modelCost)
})

test_that("the weights in residualDetails reproduce the weighted residuals", {
  # A valid SD for every observation, so that every error weight differs
  # from 1
  withErrors <- obsVsPredDf
  observed <- withErrors$dataType == "observed"
  withErrors$yErrorType <- "ArithmeticStdDev"
  withErrors$yErrorValues[observed] <- 0.1 * withErrors$yValues[observed] + 0.05
  result <- .calculateCostMetrics(
    withErrors,
    residualWeightingMethod = "error",
    robustMethod = "huber"
  )
  details <- result$residualDetails
  expect_equal(
    details$errorWeights * details$robustWeights * details$userWeights,
    details$totalWeights
  )
  expect_equal(
    details$rawResiduals * details$scaleFactor * details$totalWeights,
    details$weightedResiduals
  )
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
    blqMethod = "lloqHalf"
  )
  result_m3 <- .calculateCostMetrics(
    obsVsPredDf,
    blqMethod = "m3",
    scaling = "lin",
    cv = 0.2
  )
  expect_true(result_lsq$modelCost != result_m3$modelCost)
})

test_that("kernel costs with an LLOQ equal the recorded values", {
  obsVsPredDf$lloq <- 2.5
  expect_equal(
    .calculateCostMetrics(obsVsPredDf, blqMethod = "lloqHalf")$modelCost,
    679.97101306389038
  )
  expect_equal(
    .calculateCostMetrics(
      obsVsPredDf,
      blqMethod = "m3",
      scaling = "lin",
      cv = 0.2
    )$modelCost,
    678.32196853928076
  )
})

test_that("calculateCostMetrics correctly scales residuals when scaleVar is TRUE", {
  result_scaled <- .calculateCostMetrics(obsVsPredDf, scaleVar = TRUE)
  result_unscaled <- .calculateCostMetrics(obsVsPredDf, scaleVar = FALSE)
  expect_true(result_scaled$modelCost != result_unscaled$modelCost)
})

test_that("the cost stops without simulated or observed data", {
  for (dataType in c("simulated", "observed")) {
    expect_error(
      .calculateCostMetrics(obsVsPredDf[obsVsPredDf$dataType != dataType, ]),
      messages$errorNoDataForCost(dataType),
      fixed = TRUE
    )
  }

  costControl <- PIConfiguration$new()$objectiveFunctionOptions
  costControl$scaling <- "lin"
  costControl$blqMethod <- "lloqHalf"
  costControl$cv <- 0.2
  observed <- .prepareObservedData(testOutputMapping()[[1]])
  expect_error(
    .mappingCostTerms(
      simulated = list(xValues = c(0, 60), yValues = c(NA, Inf)),
      observed = observed,
      dataWeights = NULL,
      costControl = costControl,
      index = 2L,
      quantityPath = "Organism|A"
    ),
    paste0(
      "No simulated values of output mapping 2 ('Organism|A') enter the ",
      "cost: every value has a time below 0, or a missing or infinite time ",
      "or value."
    ),
    fixed = TRUE
  )
  observed$yValues[] <- NA
  expect_error(
    .mappingCostTerms(
      simulated = list(xValues = c(0, 60), yValues = c(1, 2)),
      observed = observed,
      dataWeights = NULL,
      costControl = costControl,
      index = 2L,
      quantityPath = "Organism|A"
    ),
    paste0(
      "No observed values of output mapping 2 ('Organism|A') enter the ",
      "cost: every value has a time below 0, or a missing or infinite time ",
      "or value. Check the data transformations of the output mapping, for ",
      "example xOffsets."
    ),
    fixed = TRUE
  )
})

test_that("observed times all below 0 stop the call with the output mapping", {
  task <- testPiTask()
  mapping <- task$outputMappings[[1]]
  # Shifts every observed time below 0. The data sets have the same x unit.
  lastTime <- max(unlist(lapply(mapping$observedDataSets, `[[`, "xValues")))
  mapping$setDataTransformations(xOffsets = -(lastTime + 1))
  expect_error(
    task$gridSearch(lower = -0.5, upper = 0.5, totalEvaluations = 2),
    messages$errorNoDataForCost("observed", 1, mapping$quantity$path),
    fixed = TRUE
  )
})

test_that("calculateCostMetrics handles infinite values in xValues and yValues correctly", {
  obsVsPredDfInf <- obsVsPredDf
  obsVsPredDfInf$xValues[1] <- Inf
  obsVsPredDfInf$yValues[1] <- -Inf
  expect_silent(.calculateCostMetrics(obsVsPredDfInf))
})

# observed-data caching

# state-variable parameter routing

test_that("fixture state-variable path is classified as a state variable", {
  sim <- loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  )
  expect_true(
    getParameter(stateVariableParameterPath, container = sim)$isStateVariable
  )
  expect_false(
    getParameter("Aciclovir|Lipophilicity", container = sim)$isStateVariable
  )
})

test_that("objective function runs with only a state-variable parameter", {
  task <- testStateVariableOnlyTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()

  # The parameters bucket is empty for this simulation
  simId <- names(priv$.simulations)[[1]]
  expect_length(priv$.model$variableParameters[[simId]], 0L)

  cost <- taskObjective(task)$evaluate(currStartValues(task))
  expect_true(is.finite(cost$modelCost))
})

test_that("simulatedOutputs() returns the simulated rows of a full evaluation", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  currVals <- currStartValues(task)

  full <- .observedVsPredicted(
    priv$.model$simulate(currVals),
    task$outputMappings
  )[[1]]$toDataFrame()
  fullSimulated <- full[full$dataType == "simulated", , drop = FALSE]
  simulated <- priv$.model$simulatedOutputs(currVals, task$outputMappings)

  expect_length(simulated, 1)
  expect_identical(simulated[[1]]$xValues, fullSimulated$xValues)
  expect_identical(simulated[[1]]$yValues, fullSimulated$yValues)
})

test_that("simulatedOutputs() reads several outputs of one simulation", {
  sim <- loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  )
  piParameter <- PIParameters$new(
    parameters = list(getParameter("Aciclovir|Lipophilicity", container = sim))
  )
  mappings <- lapply(
    c(
      paste0(
        "Organism|PeripheralVenousBlood|Aciclovir|",
        "Plasma (Peripheral Venous Blood)"
      ),
      "Organism|VenousBlood|Plasma|Aciclovir|Concentration"
    ),
    function(path) {
      mapping <- PIOutputMapping$new(quantity = getQuantity(path, sim))
      mapping$addObservedDataSets(
        testObservedData()$`AciclovirLaskinData.Laskin 1982.Group A`
      )
      mapping
    }
  )
  task <- ParameterIdentification$new(
    simulations = sim,
    parameters = piParameter,
    outputMappings = mappings
  )
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  currVals <- currStartValues(task)

  full <- .observedVsPredicted(
    priv$.model$simulate(currVals),
    task$outputMappings
  )
  simulated <- priv$.model$simulatedOutputs(currVals, task$outputMappings)

  expect_length(simulated, 2)
  for (idx in 1:2) {
    df <- full[[idx]]$toDataFrame()
    df <- df[df$dataType == "simulated", , drop = FALSE]
    expect_identical(simulated[[idx]]$xValues, df$xValues)
    expect_identical(simulated[[idx]]$yValues, df$yValues)
  }
  expect_false(identical(simulated[[1]]$yValues, simulated[[2]]$yValues))
})

test_that(".simulatedValues orders a population result as DataCombined", {
  sim <- freshSimulation()
  # Three individuals with different lipophilicity, so that their values
  # differ at every time
  populationFile <- tempfile(fileext = ".csv")
  on.exit(unlink(populationFile), add = TRUE)
  writeLines(
    c('"IndividualId","Aciclovir|Lipophilicity"', "0,-0.5", "1,0.3", "2,1"),
    populationFile
  )
  results <- runSimulations(
    sim,
    population = loadPopulation(populationFile)
  )[[1]]
  path <- testQuantity(sim)$path

  simulated <- .simulatedValues(results, path, .simulatedTimes(results))
  dataCombined <- DataCombined$new()
  dataCombined$addSimulationResults(results, quantitiesOrPaths = path)
  df <- dataCombined$toDataFrame()

  expect_identical(sort(unique(df$IndividualId)), 0:2)
  expect_identical(simulated$xValues, df$xValues)
  expect_identical(simulated$yValues, df$yValues)
})

# several simulations

test_that(".resolveParameterTargets resolves groups over two simulations", {
  clPath <- paste0(
    "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|",
    "Renal Clearances-TS-Aciclovir|TSspec"
  )
  lipophilicityPath <- "Aciclovir|Lipophilicity"
  volumePath <- "Organism|Liver|Volume"
  simulations <- replicate(
    2,
    freshSimulation(),
    simplify = FALSE
  )
  ids <- vapply(simulations, function(sim) sim$root$id, character(1))
  group <- function(path, positions) {
    PIParameters$new(
      parameters = lapply(simulations[positions], function(sim) {
        getParameter(path, container = sim)
      })
    )
  }
  groups <- list(
    # Both simulations, the second one first
    group(clPath, 2:1),
    # Both simulations
    group(lipophilicityPath, 1:2),
    # A path of the second simulation that the second group has, too: the
    # last group wins
    group(lipophilicityPath, 2),
    # A state variable in both simulations
    group(stateVariableParameterPath, 1:2),
    # The same path twice in one group
    group(volumePath, c(1, 1))
  )

  targets <- .resolveParameterTargets(groups)
  expect_identical(names(targets), rev(ids))
  expect_identical(
    targets[[ids[[1]]]],
    list(
      parameterPaths = c(clPath, lipophilicityPath, volumePath),
      parameterGroups = c(1L, 2L, 5L),
      moleculePaths = stateVariableParameterPath,
      moleculeGroups = 4L
    )
  )
  expect_identical(
    targets[[ids[[2]]]],
    list(
      parameterPaths = c(clPath, lipophilicityPath),
      parameterGroups = c(1L, 3L),
      moleculePaths = stateVariableParameterPath,
      moleculeGroups = 4L
    )
  )
})

# The intravenous and the oral Clarithromycin task, and one with both, built
# once for the tests below
clarithromycinTasks <- local({
  tasks <- NULL
  function() {
    if (is.null(tasks)) {
      tasks <<- list(
        both = testClarithromycinTask(),
        IV250 = testClarithromycinTask("IV250"),
        PO250 = testClarithromycinTask("PO250")
      )
      for (task in tasks) {
        task$.__enclos_env__$private$.batchInitialization()
      }
    }
    tasks
  }
})

test_that("the cost of two simulations is the sum of their costs", {
  tasks <- clarithromycinTasks()
  objective <- function(name, values) {
    taskObjective(tasks[[name]])$evaluate(values)
  }
  startValues <- currStartValues(tasks$both)

  for (values in list(startValues, startValues * c(2, 0.5, 1.1))) {
    both <- objective("both", values)
    # The intravenous simulation has only the first two groups
    iv <- objective("IV250", values[1:2])
    po <- objective("PO250", values)

    expect_identical(both$modelCost, po$modelCost + iv$modelCost)
    expect_identical(
      both$minLogProbability,
      po$minLogProbability + iv$minLogProbability
    )
    expect_identical(both$costVariables, po$costVariables + iv$costVariables)
    # The first output mapping is that of the oral simulation, the second
    # that of the intravenous one
    ivRows <- iv$residualDetails
    ivRows$index <- 2L
    expect_identical(both$residualDetails, rbind(po$residualDetails, ivRows))
    expect_identical(unique(both$residualDetails$index), 1:2)
  }
})

test_that("simulatedOutputs() reads the outputs of two simulations", {
  task <- clarithromycinTasks()$both
  priv <- task$.__enclos_env__$private
  currVals <- currStartValues(task)

  full <- .observedVsPredicted(
    priv$.model$simulate(currVals),
    task$outputMappings
  )
  simulated <- priv$.model$simulatedOutputs(currVals, task$outputMappings)

  expect_length(simulated, 2)
  for (idx in 1:2) {
    df <- full[[idx]]$toDataFrame()
    df <- df[df$dataType == "simulated", , drop = FALSE]
    expect_identical(simulated[[idx]]$xValues, df$xValues)
    expect_identical(simulated[[idx]]$yValues, df$yValues)
  }
  # Each simulation has its own output times, those of its observed data
  expect_false(identical(simulated[[1]]$xValues, simulated[[2]]$xValues))
  expect_false(identical(simulated[[1]]$yValues, simulated[[2]]$yValues))
})

test_that("objective function reads the observed data once and reuses them", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  currVals <- currStartValues(task)
  observedDataReads <- localObservedDataReads()

  expect_null(priv$.data)

  objective <- objectiveClosure(task)
  cost1 <- objective(currVals)
  observedData <- priv$.data$observedData
  expect_false(is.null(observedData))
  expect_equal(observedDataReads$reads, 1)

  cost2 <- objective(currVals)
  objectiveClosure(task)(currVals * 2)
  expect_equal(observedDataReads$reads, 1)
  expect_identical(priv$.data$observedData, observedData)
  expect_identical(cost2, cost1)
})

test_that("observed data equal the observed rows of a full evaluation", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  currVals <- currStartValues(task)

  full <- .observedVsPredicted(
    priv$.model$simulate(currVals),
    task$outputMappings
  )[[1]]$toDataFrame()
  converted <- ospsuite:::.unitConverter(
    full,
    xUnit = ospsuite::getBaseUnit("Time"),
    yUnit = ospsuite::getBaseUnit(task$outputMappings[[1]]$quantity$dimension)
  )
  # The log transformation of 2.2.0.9009 takes its epsilon from the first,
  # simulated row
  logged <- frozenApplyLogTransformation(converted)
  isObserved <- converted$dataType == "observed"
  expected <- converted[isObserved, , drop = FALSE]

  observed <- taskObjective(task)$observedData[[1]]

  for (column in c(
    "xValues",
    "yValues",
    "yErrorValues",
    "yErrorType",
    "lloq"
  )) {
    expect_identical(observed[[column]], expected[[column]])
  }
  expect_identical(observed$name, as.character(expected$name))
  expect_identical(observed$logYValues, logged$yValues[isObserved])
  expect_identical(observed$logLloq, logged$lloq[isObserved])
})

# The objective function against that of 2.2.0.9009, which calculated the
# cost on data frames (`frozenObjectiveFunction()`, see
# helper-frozen-objective-function.R). Each test builds one task and applies
# several settings to it. A setting sets the scaling of every output mapping
# and every objective function option, so it does not depend on the settings
# applied before it.

# The value of an expression and the messages of its warnings
withWarningMessages <- function(expr) {
  warningMessages <- character()
  value <- withCallingHandlers(expr, warning = function(w) {
    warningMessages <<- c(warningMessages, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = warningMessages)
}

# Sets the scaling of every output mapping (`scaling` is recycled) and every
# objective function option, the given ones and the defaults for the others,
# and initializes the batches, as a public method does. The observed data are
# then read again.
applyCostSetting <- function(
  task,
  scaling = "lin",
  options = list(),
  blq = list()
) {
  mappings <- task$outputMappings
  scaling <- rep_len(scaling, length(mappings))
  for (idx in seq_along(mappings)) {
    mappings[[idx]]$scaling <- scaling[[idx]]
  }
  task$configuration$objectiveFunctionOptions <- utils::modifyList(
    PIConfiguration$new()$objectiveFunctionOptions,
    options
  )
  task$configuration$blqMethod <- blq$blqMethod %||% "lloqHalf"
  task$configuration$blqOptions <- utils::modifyList(
    PIConfiguration$new()$blqOptions,
    blq$blqOptions %||% list()
  )
  task$.__enclos_env__$private$.batchInitialization()
}

# Expects identical results and warnings from the objective function and from
# the objective function of 2.2.0.9009, for each parameter value, in this
# order, the objective function in the convention of 2.2.0.9009
# (`asFrozenCost()`). The first evaluation reads the observed data, the later
# ones reuse them.
expectFrozenObjective <- function(task, values, bootstrapSeed = NULL) {
  objective <- taskObjective(task, bootstrapSeed)
  for (value in values) {
    testthat::expect_identical(
      withWarningMessages(asFrozenCost(objective$evaluate(value))),
      withWarningMessages(frozenObjectiveFunction(task, value, bootstrapSeed))
    )
  }
}

# Applies each setting to the task and compares the objective functions
expectFrozenForSettings <- function(task, settings, values) {
  for (setting in settings) {
    do.call(applyCostSetting, c(list(task), setting))
    expectFrozenObjective(task, values)
  }
}

# The model costs for each setting and parameter value, in this order. The
# objective function of 2.2.0.9009 handled BLQ data differently, so data with
# an LLOQ are compared with costs recorded when the BLQ handling moved into
# the kernel.
recordedCosts <- function(task, settings, values) {
  costs <- numeric(0)
  for (setting in settings) {
    do.call(applyCostSetting, c(list(task), setting))
    objective <- taskObjective(task)
    for (value in values) {
      # The warnings are compared with 2.2.0.9009 on the data without LLOQs
      cost <- suppressWarnings(objective$evaluate(value))
      costs <- c(costs, cost$modelCost)
    }
  }
  costs
}

aciclovirPlasmaPaths <- c(
  paste0(
    "Organism|PeripheralVenousBlood|Aciclovir|",
    "Plasma (Peripheral Venous Blood)"
  ),
  "Organism|VenousBlood|Plasma|Aciclovir|Concentration"
)

# A task with the lipophilicity of one Aciclovir simulation as the parameter
# and a list of observed data sets for each output path
aciclovirTask <- function(dataSetsByPath) {
  sim <- freshSimulation()
  ParameterIdentification$new(
    simulations = sim,
    parameters = PIParameters$new(
      parameters = list(ospsuite::getParameter("Aciclovir|Lipophilicity", sim))
    ),
    outputMappings = lapply(names(dataSetsByPath), function(path) {
      mapping <- PIOutputMapping$new(
        quantity = ospsuite::getQuantity(path, sim)
      )
      mapping$addObservedDataSets(dataSetsByPath[[path]])
      mapping
    })
  )
}

# Two outputs of one simulation with the same observed data, optionally with
# an LLOQ, and with data weights for the first output
twoOutputsTask <- function(lloq = NULL, weights = FALSE) {
  data <- testObservedData()$`AciclovirLaskinData.Laskin 1982.Group A`
  if (!is.null(lloq)) {
    data$LLOQ <- lloq
  }
  task <- aciclovirTask(
    stats::setNames(list(data, data), aciclovirPlasmaPaths)
  )
  if (weights) {
    task$outputMappings[[1]]$setDataWeights(
      stats::setNames(list(seq(0.5, 2, length.out = 11)), data$name)
    )
  }
  task
}

lipophilicityValues <- c(-0.097, 0.3)

test_that("objective function equals 2.2.0.9009 for scaling and options", {
  expectFrozenForSettings(
    twoOutputsTask(),
    list(
      list(),
      list(scaling = "log"),
      list(options = list(residualWeightingMethod = "error")),
      list(options = list(robustMethod = "huber")),
      list(scaling = "log", options = list(robustMethod = "bisquare")),
      list(options = list(scaleVar = TRUE))
    ),
    lipophilicityValues
  )
})

test_that("objective function equals 2.2.0.9009 with data weights", {
  expectFrozenForSettings(
    twoOutputsTask(weights = TRUE),
    list(
      list(scaling = c("lin", "log")),
      list(scaling = c("log", "lin"))
    ),
    lipophilicityValues
  )
})

test_that("objective function with an LLOQ gives the recorded costs", {
  costs <- recordedCosts(
    twoOutputsTask(lloq = 0.5),
    list(
      list(),
      list(scaling = "log"),
      list(blq = list(blqMethod = "m3")),
      list(
        scaling = "log",
        blq = list(blqMethod = "m3", blqOptions = list(cv = cvOfSd0086))
      )
    ),
    lipophilicityValues
  )
  expect_equal(
    costs,
    c(
      1220.2393362842672104,
      657.4726692176054712,
      16.3816363783996124,
      10.3372856741677115,
      1218.4772715246112966,
      657.8799182603720510,
      7.2808531862041646,
      16.0917878128034637
    )
  )
})

test_that("objective function with two LLOQs in an output mapping gives the recorded costs", {
  dataSets <- testObservedDataMultiple()
  dataSets$dataSet1$LLOQ <- 0.5
  dataSets$dataSet2$LLOQ <- 2
  costs <- recordedCosts(
    aciclovirTask(stats::setNames(list(dataSets), aciclovirPlasmaPaths[[1]])),
    list(
      list(),
      list(scaling = "log"),
      list(blq = list(blqMethod = "m3")),
      list(
        scaling = "log",
        blq = list(blqMethod = "m3", blqOptions = list(cv = cvOfSd0086))
      )
    ),
    lipophilicityValues
  )
  expect_equal(
    costs,
    c(
      1412.6285263451634364,
      868.3906019359211541,
      12.9622061142714955,
      9.0495445371042802,
      1387.4807406750228438,
      849.9689432511274845,
      6.3258207804017719,
      11.6224753838891903
    )
  )
})

test_that("censored values contribute alike in one or in two output mappings", {
  dataSets <- testObservedDataMultiple()
  dataSets$dataSet1$LLOQ <- 0.5
  dataSets$dataSet2$LLOQ <- 2
  # The M3 contribution with the data sets in the given output mappings. The
  # frozen objective function uses `.calculateCensoredContribution()` of the
  # package, so this compares with output mappings of one LLOQ each instead.
  m3Contribution <- function(dataSetsByMapping, scaling, blq) {
    sim <- freshSimulation()
    task <- ParameterIdentification$new(
      simulations = sim,
      parameters = PIParameters$new(
        parameters = list(
          ospsuite::getParameter("Aciclovir|Lipophilicity", sim)
        )
      ),
      outputMappings = lapply(dataSetsByMapping, function(mappingDataSets) {
        mapping <- PIOutputMapping$new(
          quantity = ospsuite::getQuantity(aciclovirPlasmaPaths[[1]], sim)
        )
        mapping$addObservedDataSets(mappingDataSets)
        mapping
      })
    )
    applyCostSetting(task, scaling = scaling, blq = blq)
    taskObjective(task)$evaluate(
      lipophilicityValues[[1]]
    )$costVariables$M3Contribution
  }

  for (setting in list(
    list(
      scaling = "lin",
      blq = list(blqMethod = "m3")
    ),
    list(
      scaling = "log",
      blq = list(blqMethod = "m3", blqOptions = list(cv = cvOfSd0086))
    )
  )) {
    together <- m3Contribution(
      list(unname(dataSets)),
      setting$scaling,
      setting$blq
    )
    separate <- m3Contribution(
      list(dataSets$dataSet1, dataSets$dataSet2),
      setting$scaling,
      setting$blq
    )
    expect_gt(together, 0)
    expect_equal(together, separate)
  }
})

test_that("objective function equals 2.2.0.9009 with observed values of zero", {
  # A zero and a value below the epsilon of the log transformation
  dataSet <- ospsuite::DataSet$new(name = "withZeros")
  dataSet$xUnit <- ospsuite::ospUnits$Time$min
  dataSet$yDimension <- ospsuite::ospDimensions$`Concentration (molar)`
  dataSet$yUnit <- "nmol/l"
  dataSet$setValues(
    xValues = c(30, 60, 120, 240, 480, 720),
    yValues = c(12000, 9000, 0, 3000, 1e-20, 500)
  )
  task <- aciclovirTask(
    stats::setNames(list(dataSet), aciclovirPlasmaPaths[[1]])
  )
  expectFrozenForSettings(
    task,
    list(
      list(),
      list(scaling = "log"),
      list(scaling = "log", options = list(robustMethod = "bisquare"))
    ),
    lipophilicityValues
  )
  # Both values are replaced by the epsilon, in the base unit of the data
  logYValues <- taskObjective(task)$observedData[[1]]$logYValues
  expect_equal(
    logYValues[c(3, 5)],
    rep(log(ospsuite::getOSPSuiteSetting("LOG_SAFE_EPSILON")), 2)
  )
  expect_true(all(is.finite(logYValues)))
})

# Molar data in min with geometric SD
molarDataSet <- function(name, lloq = NULL) {
  dataSet <- ospsuite::DataSet$new(name = name)
  dataSet$xUnit <- ospsuite::ospUnits$Time$min
  dataSet$yDimension <- ospsuite::ospDimensions$`Concentration (molar)`
  dataSet$yUnit <- "nmol/l"
  dataSet$setValues(
    xValues = c(30, 60, 120, 240, 480, 720),
    yValues = c(12000, 9000, 6000, 3000, 1200, 500),
    yErrorValues = c(1.2, 1.3, 1.5, 1.4, 1.8, 2)
  )
  dataSet$yErrorType <- ospsuite::DataErrorType$GeometricStdDev
  if (!is.null(lloq)) {
    dataSet$LLOQ <- lloq
  }
  dataSet
}

# One simulation with two outputs. The first output has three data sets, with
# y transformations: the data of Laskin 1982 (mg/l, arithmetic SD) with a data
# weight, the same data times 1.5 without the last point, with an LLOQ and a
# data weight per point, and molar data. The second output has molar data
# with an LLOQ, with x and y transformations. Without `lloq`, no data set has
# an LLOQ.
severalDataSetsTask <- function(lloq = TRUE) {
  dataSets <- testObservedDataMultiple()
  if (lloq) {
    dataSets$dataSet2$LLOQ <- 1
  }
  task <- aciclovirTask(stats::setNames(
    list(
      c(dataSets, dataSet3 = molarDataSet("dataSet3")),
      molarDataSet("dataSet4", lloq = if (lloq) 1000)
    ),
    aciclovirPlasmaPaths
  ))
  task$outputMappings[[1]]$setDataTransformations(
    yOffsets = 0.05,
    yFactors = 0.9
  )
  task$outputMappings[[1]]$setDataWeights(
    list(dataSet1 = 2, dataSet2 = seq(1, 0.1, length.out = 10))
  )
  task$outputMappings[[2]]$setDataTransformations(
    xOffsets = 2,
    xFactors = 1.5,
    yFactors = 1.2
  )
  task
}

# Without log scaling and error weights: 2.2.0.9009 calculated the error weights
# on the log scale from the log values (#325), see the tests after those of
# `.computeErrorWeights()`
test_that("objective function equals 2.2.0.9009 for several data sets", {
  expectFrozenForSettings(
    severalDataSetsTask(lloq = FALSE),
    list(
      list(),
      list(scaling = "log"),
      list(options = list(residualWeightingMethod = "error")),
      list(
        scaling = c("lin", "log"),
        options = list(robustMethod = "huber", scaleVar = TRUE)
      )
    ),
    lipophilicityValues
  )
})

test_that("objective function for several data sets with LLOQs gives the recorded costs", {
  costs <- recordedCosts(
    severalDataSetsTask(),
    list(
      list(),
      list(scaling = "log"),
      list(options = list(residualWeightingMethod = "error")),
      list(blq = list(blqMethod = "m3")),
      list(
        scaling = "log",
        blq = list(blqMethod = "m3", blqOptions = list(cv = cvOfSd0086))
      ),
      list(
        scaling = c("lin", "log"),
        options = list(robustMethod = "huber", scaleVar = TRUE)
      )
    ),
    lipophilicityValues
  )
  expect_equal(
    costs,
    c(
      4077.93639166568664223,
      2220.13733736031144872,
      20.50730380080133486,
      12.23819518709304610,
      3710.13862599347294235,
      2040.35768618197107571,
      4077.71183943283313056,
      2220.03992715807862623,
      17.76494123913684930,
      11.16899597451127946,
      0.20227508624352725,
      0.32456224250170529
    )
  )
})

test_that("objective function equals 2.2.0.9009 for two simulations with grouped parameters", {
  # Mass data, parameter groups over both simulations and over one
  task <- testClarithromycinTask()
  startValues <- currStartValues(task)
  expectFrozenForSettings(
    task,
    list(
      list(),
      list(scaling = "log"),
      list(scaling = c("lin", "log"), options = list(robustMethod = "huber"))
    ),
    list(startValues, startValues * c(2, 0.5, 1.1))
  )
})

test_that("objective function equals 2.2.0.9009 for the Midazolam model", {
  sim <- ospsuite::loadSimulation(
    getTestDataFilePath("Midazolam_Smith_1981_iv_5mg.pkml"),
    loadFromCache = FALSE,
    addToCache = FALSE
  )
  filePath <- getTestDataFilePath("Midazolam_Smith_1981.xlsx")
  dataConfig <- ospsuite::createImporterConfigurationForFile(filePath)
  dataConfig$sheets <- "Smith1981"
  dataConfig$namingPattern <- "{Source}.{Sheet}"
  mapping <- PIOutputMapping$new(
    quantity = ospsuite::getQuantity(
      paste0(
        "Organism|PeripheralVenousBlood|Midazolam|",
        "Plasma (Peripheral Venous Blood)"
      ),
      sim
    )
  )
  mapping$addObservedDataSets(
    ospsuite::loadDataSetsFromExcel(filePath, dataConfig)
  )
  task <- ParameterIdentification$new(
    simulations = sim,
    parameters = lapply(
      c(
        "Midazolam|Lipophilicity",
        "Midazolam-CYP3A4-Patki et al. 2003 rCYP3A4|kcat"
      ),
      function(path) {
        PIParameters$new(parameters = list(ospsuite::getParameter(path, sim)))
      }
    ),
    outputMappings = mapping
  )
  expectFrozenForSettings(
    task,
    list(list(), list(scaling = "log")),
    list(c(3.9, 320), c(3, 600))
  )
})

test_that("objective function equals 2.2.0.9009 with bootstrap weights", {
  # Five individual data sets, the first with a data weight: the bootstrap
  # resamples the weights of the data sets, not their values
  dataSets <- syntheticObservedData()
  task <- aciclovirTask(stats::setNames(
    list(dataSets),
    aciclovirPlasmaPaths[1]
  ))
  mapping <- task$outputMappings[[1]]
  mapping$setDataWeights(stats::setNames(list(2), dataSets[[1]]$name))
  applyCostSetting(task)

  for (seed in 1:2) {
    expectFrozenObjective(task, lipophilicityValues, bootstrapSeed = seed)
    resampled <- PIObservations$new(task$outputMappings)$resample(seed)
    expect_false(identical(
      resampled$outputMappings[[1]]$dataWeights,
      mapping$dataWeights
    ))
  }
  applyCostSetting(task, scaling = "log")
  expectFrozenObjective(task, lipophilicityValues[[2]], bootstrapSeed = 3L)

  # The data of the task are unchanged
  expect_identical(
    mapping$dataWeights,
    stats::setNames(
      list(rep(2, length(dataSets[[1]]$yValues))),
      dataSets[[1]]$name
    )
  )
  expectFrozenObjective(task, lipophilicityValues[[2]])
})

test_that(".combineCostTerms equals the sum of the costs of 2.2.0.9009", {
  kernelTerms <- function(index) {
    .costKernel(
      simulatedX = c(0, 1, 2, 3),
      simulatedY = c(1, 2, 3, 2) * index,
      observedX = c(0.5, 1.5, 2.5),
      observedY = c(1.4, 2.7, 2.2),
      userWeights = c(NA, 2, 0.5),
      yErrorValues = NULL,
      yErrorType = NULL,
      residualWeightingMethod = "none",
      robustMethod = "huber",
      scaleVar = FALSE,
      lloq = rep(NA_real_, 3),
      blqMethod = "none",
      cv = NULL,
      scaling = "lin",
      observedYLinear = c(1.4, 2.7, 2.2),
      lloqLinear = rep(NA_real_, 3),
      index = index
    )
  }
  terms <- list(kernelTerms(1L), .errorCostTerms(index = 2L), kernelTerms(3L))
  costs <- lapply(terms, function(t) do.call(.newModelCost, t))

  expect_identical(
    .combineCostTerms(terms),
    Reduce(frozenSummarizeCostLists, costs)
  )
  expect_identical(.combineCostTerms(terms[1]), costs[[1]])
  expect_identical(.createErrorCostStructure(index = 2L), costs[[2]])
  expect_identical(
    asFrozenCost(.createErrorCostStructure(index = 2L)),
    frozenCreateErrorCostStructure(index = 2L)
  )
})

test_that("calculateCostMetrics equals 2.2.0.9009 in the settings above", {
  # The data frames of the tests of `.calculateCostMetrics()` above
  naDf <- obsVsPredDf
  firstObserved <- which(naDf$dataType == "observed")[1]
  naDf$xValues[firstObserved] <- max(naDf$xValues) * 10
  geometricDf <- obsVsPredDf
  validErrors <- geometricDf$dataType == "observed" &
    !is.na(geometricDf$yErrorValues) &
    geometricDf$yErrorValues > 0
  cv <- geometricDf$yErrorValues[validErrors] /
    geometricDf$yValues[validErrors]
  geometricDf$yErrorValues[validErrors] <- exp(sqrt(log(1 + cv^2)))
  geometricDf$yErrorType <- "GeometricStdDev"
  infiniteDf <- obsVsPredDf
  infiniteDf$xValues[1] <- Inf
  infiniteDf$yValues[1] <- -Inf

  settings <- list(
    list(df = obsVsPredDf),
    list(df = obsVsPredDf, index = 7),
    list(df = naDf, index = 5),
    list(df = obsVsPredDf, residualWeightingMethod = "none"),
    list(df = obsVsPredDf, residualWeightingMethod = "error"),
    list(df = geometricDf, residualWeightingMethod = "error"),
    list(df = obsVsPredDf, robustMethod = "huber"),
    list(df = obsVsPredDf, robustMethod = "bisquare"),
    list(df = obsVsPredDf, scaleVar = TRUE),
    list(df = infiniteDf)
  )
  for (setting in settings) {
    expect_identical(
      withWarningMessages(asFrozenCost(do.call(
        .calculateCostMetrics,
        setting
      ))),
      withWarningMessages(
        do.call(frozenCalculateCostMetrics, frozenCostArguments(setting))
      )
    )
  }
})

test_that("observed data are read again at the start of every public call", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  currVals <- currStartValues(task)
  costBefore <- objectiveClosure(task)(currVals)$modelCost

  # Public methods start with the batch initialization, so a change of the
  # data transformations between two calls takes effect
  task$outputMappings[[1]]$setDataTransformations(yFactors = 0.5)
  priv$.batchInitialization()
  expect_null(priv$.objective)
  costAfter <- objectiveClosure(task)(currVals)$modelCost

  freshTask <- testPiTask()
  freshTask$outputMappings[[1]]$setDataTransformations(yFactors = 0.5)
  freshPriv <- freshTask$.__enclos_env__$private
  freshPriv$.batchInitialization()

  expect_false(costAfter == costBefore)
  expect_identical(
    costAfter,
    objectiveClosure(freshTask)(currStartValues(freshTask))$modelCost
  )
})

test_that("LLOQ, scaling and weights changed between calls take effect", {
  change <- function(task) {
    mapping <- task$outputMappings[[1]]
    dataSet <- mapping$observedDataSets[[1]]
    dataSet$LLOQ <- stats::median(dataSet$yValues)
    mapping$scaling <- "log"
    mapping$setDataWeights(
      stats::setNames(list(2), names(mapping$observedDataSets))
    )
  }
  gridSearch <- function(task) {
    task$gridSearch(lower = -0.5, upper = 0.5, totalEvaluations = 3)
  }

  task <- testPiTask()
  before <- gridSearch(task)
  change(task)
  after <- gridSearch(task)

  freshTask <- testPiTask()
  change(freshTask)
  expect_false(identical(after$ofv, before$ofv))
  expect_identical(after, gridSearch(freshTask))
})

test_that("a missing simulated value is left out with an LLOQ", {
  costControl <- PIConfiguration$new()$objectiveFunctionOptions
  costControl$scaling <- "lin"
  costControl$blqMethod <- "lloqHalf"
  costControl$cv <- 0.2
  dataSet <- testObservedData()$`AciclovirLaskinData.Laskin 1982.Group A`
  dataSet$LLOQ <- 0.5
  mapping <- PIOutputMapping$new(quantity = testQuantity())
  mapping$addObservedDataSets(dataSet)
  terms <- .mappingCostTerms(
    simulated = list(xValues = c(0, 60, 1e4), yValues = c(0, NA, 1)),
    observed = .prepareObservedData(mapping),
    dataWeights = mapping$dataWeights,
    costControl = costControl,
    index = 1L
  )
  expect_true(is.finite(terms$modelCost))
})

# .computeErrorWeights

test_that(".computeErrorWeights uses exact lognormal SD formula for GeometricStdDev", {
  yValues <- c(10, 20)
  yErrorValues <- c(2.0, 3.0)
  yErrorType <- c("GeometricStdDev", "GeometricStdDev")

  result <- .computeErrorWeights(yValues, yErrorValues, yErrorType)

  # y is the geometric mean, SD = y * exp(sigma^2 / 2) * sqrt(exp(sigma^2) - 1)
  # GSD=2, y=10: sigma^2=0.4805, SD=10*1.2716*0.7854=9.986
  # GSD=3, y=20: sigma^2=1.2069, SD=20*1.8285*1.5308=55.98
  expect_equal(result, c(1 / 9.986, 1 / 55.98), tolerance = 1e-3)
})

test_that(".computeErrorWeights treats y as the geometric mean of a GSD (#332)", {
  # The SD of log-normal samples with median 1 and GSD 2.5 is 1.745
  expect_equal(
    .computeErrorWeights(1, 2.5, "GeometricStdDev"),
    1 / 1.7452,
    tolerance = 1e-4
  )
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

test_that(".computeErrorWeights uses log(GSD) as the SD on the log scale", {
  # The SD of the log of a log-normal value is log(GSD), whatever the value,
  # also for values below 1: 1 / log(1.5) = 2.466303, 1 / log(2) = 1.442695
  result <- .computeErrorWeights(
    yValues = c(0.5, 2, 10),
    yErrorValues = c(1.5, 2, 1.5),
    yErrorType = rep("GeometricStdDev", 3),
    scaling = "log"
  )

  expect_equal(result, c(2.466303, 1.442695, 2.466303), tolerance = 1e-6)
})

test_that(".computeErrorWeights converts arithmetic SDs to the log scale", {
  # CV = 0.2 for both values: SD of the log = sqrt(log(1 + 0.2^2)) = 0.198042
  result <- .computeErrorWeights(
    yValues = c(10, 0.5),
    yErrorValues = c(2, 0.1),
    yErrorType = rep("ArithmeticStdDev", 2),
    scaling = "log"
  )

  expect_equal(result, c(5.049429, 5.049429), tolerance = 1e-6)
})

test_that(".computeErrorWeights weights an SD like its GSD on the log scale", {
  # CV = 0.1 and 0.4, and the GSD exp(sqrt(log(1 + CV^2))) of a log-normal
  # value with the same CV: 1 / sqrt(log(1 + CV^2)) = 10.024927 and 2.595696
  yValues <- c(10, 0.5)
  arithmeticSd <- c(1, 0.2)
  gsd <- exp(sqrt(log(1 + (arithmeticSd / yValues)^2)))

  weights <- list(
    arithmetic = .computeErrorWeights(
      yValues = yValues,
      yErrorValues = arithmeticSd,
      yErrorType = rep("ArithmeticStdDev", 2),
      scaling = "log"
    ),
    geometric = .computeErrorWeights(
      yValues = yValues,
      yErrorValues = gsd,
      yErrorType = rep("GeometricStdDev", 2),
      scaling = "log"
    )
  )

  expected <- c(10.024927, 2.595696)
  expect_equal(
    weights,
    list(arithmetic = expected, geometric = expected),
    tolerance = 1e-6
  )
})

test_that(".computeErrorWeights stops when its inputs differ in length", {
  # Two error values, then two error types, for three observations: each case
  # covers one of the two length checks. Without them, R recycles the shorter
  # input, and the error values 1 and 2 give the weights 1, 0.5 and NA (#335)
  yValues <- c(10, 20, 30)
  expect_snapshot(
    .computeErrorWeights(
      yValues = yValues,
      yErrorValues = c(1, 2),
      yErrorType = rep("ArithmeticStdDev", 3)
    ),
    error = TRUE
  )
  expect_snapshot(
    .computeErrorWeights(
      yValues = yValues,
      yErrorValues = c(1, 2, 3),
      yErrorType = rep("ArithmeticStdDev", 2)
    ),
    error = TRUE
  )
})

# Cost terms of an output mapping with log scaling and error weights, for
# observations at 1, 2 and 3 min
logScaleErrorTerms <- function(
  yValues,
  yErrorValues,
  yErrorType = "GeometricStdDev"
) {
  observed <- list(
    name = rep("dataSet", 3),
    xValues = c(1, 2, 3),
    xDimension = rep(ospsuite::ospDimensions$Time, 3),
    yValues = yValues,
    yErrorValues = yErrorValues,
    yErrorType = rep(yErrorType, 3),
    lloq = rep(NA_real_, 3),
    hasLloq = FALSE,
    lloqMin = NA_real_,
    logYValues = log(yValues),
    logLloq = rep(NA_real_, 3),
    logEpsilon = 1e-20,
    xUnit = ospsuite::ospUnits$Time$min
  )
  simulated <- list(xValues = 0:4, yValues = c(1, 0.6, 2.2, 11, 5))
  costControl <- list(
    objectiveFunctionType = "lsq",
    blqMethod = "lloqHalf",
    cv = 0.2,
    residualWeightingMethod = "error",
    robustMethod = "none",
    scaleVar = FALSE,
    scaling = "log"
  )
  .mappingCostTerms(simulated, observed, list(), costControl, 1)
}

test_that("GSD weights of a mapping on the log scale are 1 / log(GSD)", {
  # The same GSD for all observations, one of them below 1 (#325)
  terms <- logScaleErrorTerms(c(0.5, 2, 10), rep(1.5, 3))

  # 1 / log(1.5) for every observation
  expect_equal(terms$errorWeights, rep(1 / log(1.5), 3))
})

test_that("arithmetic SD weights of a mapping on the log scale use the CV", {
  # CV = 0.2 for all observations, one of them below 1 (#325):
  # 1 / sqrt(log(1 + 0.2^2))
  terms <- logScaleErrorTerms(
    c(0.5, 2, 10),
    c(0.1, 0.4, 2),
    yErrorType = "ArithmeticStdDev"
  )

  expect_equal(terms$errorWeights, rep(1 / sqrt(log(1 + 0.2^2)), 3))
})

test_that("an invalid error of a value below 1 on the log scale is reported", {
  # The GSD of 1 of the value 0.5 is invalid. 2.2.0.9009 did not report it,
  # because it checked the log value, which is below 0 (#325)
  expect_warning(
    terms <- logScaleErrorTerms(c(0.5, 2, 10), c(1, 1.5, 1.5)),
    regexp = "unit weights"
  )
  expect_equal(terms$errorWeights, c(1, 1 / log(1.5), 1 / log(1.5)))
})

test_that("objective function weights GSDs by 1 / log(GSD) on the log scale", {
  # Molar data with the GSDs 1.2, 1.3, 1.5, 1.4, 1.8 and 2 and values from
  # 0.5 to 12 µmol/l in the base unit, so some log values are below 0
  task <- aciclovirTask(stats::setNames(
    list(list(molarDataSet("dataSet"))),
    aciclovirPlasmaPaths[2]
  ))
  applyCostSetting(
    task,
    scaling = "log",
    options = list(residualWeightingMethod = "error")
  )

  details <- taskObjective(task)$evaluate(lipophilicityValues[[
    1
  ]])$residualDetails

  # 1 / log(GSD), to the single precision in which a DataSet stores values
  expect_equal(
    details$errorWeights,
    1 / log(c(1.2, 1.3, 1.5, 1.4, 1.8, 2)),
    tolerance = 1e-6
  )
})

test_that("objective function weights arithmetic SDs by CV on the log scale", {
  # Molar data in nmol/l with arithmetic SDs in mmol/l. In the base unit
  # µmol/l, the CVs are 0.1 to 0.6, a different one for each observation
  dataSet <- molarDataSet("dataSet")
  dataSet$yErrorType <- ospsuite::DataErrorType$ArithmeticStdDev
  dataSet$yErrorUnit <- "mmol/l"
  dataSet$setValues(
    xValues = dataSet$xValues,
    yValues = dataSet$yValues,
    yErrorValues = c(1.2, 1.8, 1.8, 1.2, 0.6, 0.3) / 1000
  )
  task <- aciclovirTask(stats::setNames(
    list(list(dataSet)),
    aciclovirPlasmaPaths[2]
  ))
  # The first observation, at 30 min, moves to -10 min and is not used
  task$outputMappings[[1]]$setDataTransformations(xOffsets = -40)
  applyCostSetting(
    task,
    scaling = "log",
    options = list(residualWeightingMethod = "error")
  )

  details <- taskObjective(task)$evaluate(lipophilicityValues[[
    1
  ]])$residualDetails

  # 1 / sqrt(log(1 + CV^2)) for the CVs 0.2 to 0.6, to the single precision
  # in which a DataSet stores values
  expect_equal(
    details$errorWeights,
    1 / sqrt(log(1 + c(0.2, 0.3, 0.4, 0.5, 0.6)^2)),
    tolerance = 1e-6
  )
})
