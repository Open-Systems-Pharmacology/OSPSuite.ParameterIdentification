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
  obsVsPredDfLog <- frozenApplyLogTransformation(obsVsPredDf)
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

test_that(".calculateCensoredContribution uses the LLOQ of each censored value", {
  # Two data sets with different LLOQs, all values censored, in the order of
  # the data sets and not of the times
  observed <- data.frame(
    xValues = c(60, 180, 120, 240),
    xUnit = "min",
    xDimension = "Time",
    yValues = c(0.5, 0.5, 1, 1),
    lloq = c(1, 1, 2, 2)
  )
  simulated <- data.frame(
    xValues = c(60, 120, 180, 240),
    xUnit = "min",
    xDimension = "Time",
    yValues = c(0.1, 1.9, 0.9, 0.2)
  )
  # The simulated values at the observed times
  simulatedY <- c(0.1, 0.9, 1.9, 0.2)
  lloq <- observed$lloq

  expect_equal(
    .calculateCensoredContribution(
      observed = observed,
      simulated = simulated,
      scaling = "lin",
      linScaleCV = 0.2
    ),
    sum(-2 * log10(stats::pnorm((lloq - simulatedY) / (0.2 * lloq))))
  )
  expect_equal(
    .calculateCensoredContribution(
      observed = transform(observed, yValues = log(yValues), lloq = log(lloq)),
      simulated = transform(simulated, yValues = log(yValues)),
      scaling = "log",
      logScaleSD = 0.086
    ),
    sum(-2 * log10(stats::pnorm((log(lloq) - log(simulatedY)) / 0.086)))
  )
  # One LLOQ for all values
  observed$lloq <- 2
  expect_equal(
    .calculateCensoredContribution(
      observed = observed,
      simulated = simulated,
      scaling = "lin",
      linScaleCV = 0.2
    ),
    sum(-2 * log10(stats::pnorm((2 - simulatedY) / (0.2 * 2))))
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

currStartValues <- function(task) {
  vapply(task$parameters, function(p) p$startValue, numeric(1))
}

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

test_that(".batchInitialization routes state-variable parameters to molecules", {
  task <- testStateVariableMixedTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()

  simId <- names(priv$.simulations)[[1]]

  expect_true(
    stateVariableParameterPath %in% names(priv$.variableMolecules[[simId]])
  )
  expect_false(
    stateVariableParameterPath %in% names(priv$.variableParameters[[simId]])
  )

  expect_true(
    "Aciclovir|Lipophilicity" %in% names(priv$.variableParameters[[simId]])
  )
  expect_false(
    "Aciclovir|Lipophilicity" %in% names(priv$.variableMolecules[[simId]])
  )
})

test_that("objective function delivers the state-variable value into the molecule bucket", {
  task <- testStateVariableMixedTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  simId <- names(priv$.simulations)[[1]]

  # currVals order matches the parameters list: state-variable first, constant
  # second. Values differ from the start values so we can confirm the update
  # is routed into the molecule bucket rather than silently dropped.
  cost <- priv$.objectiveFunction(c(0.06, -0.1))

  expect_true(is.finite(cost$modelCost))
  expect_equal(
    priv$.variableMolecules[[simId]][[stateVariableParameterPath]],
    0.06
  )
  expect_equal(
    priv$.variableParameters[[simId]][["Aciclovir|Lipophilicity"]],
    -0.1
  )
})

test_that("a non-base state-variable unit reaches the molecules bucket in base units", {
  sim <- loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite"),
    loadFromCache = FALSE,
    addToCache = FALSE
  )

  # Organism|Lumen|Stomach|Liquid is RHS-defined, so .applyParameterValues()
  # routes it into the molecules bucket rather than the parameters bucket. Its
  # base unit is l, so values declared in ml must arrive divided by 1000.
  piParameter <- PIParameters$new(
    parameters = list(getParameter(stateVariableParameterPath, container = sim))
  )
  piParameter$unit <- ospUnits$Volume$ml
  # Start, then max, then min: $unit does not rescale the values left from
  # construction, and the bound setters cross-validate against them.
  piParameter$startValue <- 45
  piParameter$maxValue <- 450
  piParameter$minValue <- 4.5

  mapping <- PIOutputMapping$new(
    quantity = getQuantity(
      "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      container = sim
    )
  )
  mapping$addObservedDataSets(
    testObservedData()$`AciclovirLaskinData.Laskin 1982.Group A`
  )

  task <- ParameterIdentification$new(
    simulations = sim,
    parameters = piParameter,
    outputMappings = mapping
  )
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  simId <- names(priv$.simulations)[[1]]

  # .batchInitialization() seeds the start value: 45 ml is 0.045 l.
  expect_equal(
    priv$.variableMolecules[[simId]][[stateVariableParameterPath]],
    0.045
  )

  # .evaluate() deposits a trial value: 50 ml is 0.05 l.
  suppressMessages(priv$.evaluate(50))
  expect_equal(
    priv$.variableMolecules[[simId]][[stateVariableParameterPath]],
    0.05
  )

  expect_false(
    stateVariableParameterPath %in% names(priv$.variableParameters[[simId]])
  )
})

test_that("a grouped parameter spanning two simulations is seeded in base units", {
  clPath <- "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS-Aciclovir|TSspec"
  outputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

  # Two separately loaded simulations get distinct IDs, so the single converted
  # value fans out into two variable buckets. Own simulations, so that assigning
  # $unit cannot leak into the shared module-level fixtures.
  simulations <- replicate(
    2,
    loadSimulation(
      system.file("extdata", "Aciclovir.pkml", package = "ospsuite"),
      loadFromCache = FALSE,
      addToCache = FALSE
    ),
    simplify = FALSE
  )

  piParameter <- PIParameters$new(
    parameters = lapply(simulations, function(sim) {
      getParameter(clPath, container = sim)
    })
  )
  piParameter$unit <- ospUnits$`Inversed time`$`1/h`
  # Start, then max, then min: $unit does not rescale the values left from
  # construction, and the bound setters cross-validate against them.
  piParameter$startValue <- 6
  piParameter$maxValue <- 60
  piParameter$minValue <- 0.6

  mappings <- lapply(simulations, function(sim) {
    mapping <- PIOutputMapping$new(
      quantity = getQuantity(outputPath, container = sim)
    )
    mapping$addObservedDataSets(
      testObservedData()$`AciclovirLaskinData.Laskin 1982.Group A`
    )
    mapping
  })

  task <- ParameterIdentification$new(
    simulations = simulations,
    parameters = piParameter,
    outputMappings = mappings
  )
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  simIds <- names(priv$.simulations)

  expect_length(simIds, 2)
  # Independent oracle: 1/h to 1/min is a factor of 60, so the start value of
  # 6 1/h must reach both simulations as 0.1 1/min.
  for (simId in simIds) {
    expect_equal(priv$.variableParameters[[simId]][[clPath]], 0.1)
  }

  # .evaluate() deposits a trial value: 30 1/h is 0.5 1/min in both buckets.
  suppressMessages(priv$.evaluate(30))
  for (simId in simIds) {
    expect_equal(priv$.variableParameters[[simId]][[clPath]], 0.5)
  }
})

test_that("objective function runs with only a state-variable parameter", {
  task <- testStateVariableOnlyTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()

  # .variableParameters is empty for this simulation (empty parametersOrPaths).
  simId <- names(priv$.simulations)[[1]]
  expect_length(priv$.variableParameters[[simId]], 0L)

  cost <- priv$.objectiveFunction(currStartValues(task))
  expect_true(is.finite(cost$modelCost))
})

test_that("state-variable initial value reaches the solver", {
  sim <- loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  )
  svQuantity <- getQuantity(stateVariableParameterPath, container = sim)

  stateVar <- stateVarPIParameter(sim)

  # Map the output to the state variable's own quantity so its simulated
  # trajectory is observable. Observed values are placeholders in the state
  # variable's (Volume) dimension.
  obs <- DataSet$new(name = "obs")
  obs$yDimension <- svQuantity$dimension
  obs$setValues(xValues = c(0, 1, 2), yValues = c(0.05, 0.05, 0.05))
  mapping <- PIOutputMapping$new(quantity = svQuantity)
  mapping$addObservedDataSets(obs)

  task <- ParameterIdentification$new(
    simulations = sim,
    parameters = stateVar,
    outputMappings = mapping
  )
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()

  simulatedInitialValue <- function(startValue) {
    simulated <- priv$.simulateOutputs(startValue)[[1]]
    simulated$yValues[which.min(simulated$xValues)]
  }

  # Two distinct initial values must reach the solver and appear as the
  # simulated initial value, proving the molecule value is consumed downstream
  # (via addRunValues) and not merely stored in the R-side bucket.
  expect_equal(simulatedInitialValue(0.02), 0.02, tolerance = 1e-4)
  expect_equal(simulatedInitialValue(0.09), 0.09, tolerance = 1e-4)
})

test_that(".simulateOutputs returns the simulated rows of a full evaluation", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  currVals <- currStartValues(task)

  full <- priv$.evaluate(currVals)[[1]]$toDataFrame()
  fullSimulated <- full[full$dataType == "simulated", , drop = FALSE]
  simulated <- priv$.simulateOutputs(currVals)

  expect_length(simulated, 1)
  expect_identical(simulated[[1]]$xValues, fullSimulated$xValues)
  expect_identical(simulated[[1]]$yValues, fullSimulated$yValues)
})

test_that(".simulateOutputs reads several outputs of one simulation", {
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

  full <- priv$.evaluate(currVals)
  simulated <- priv$.simulateOutputs(currVals)

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
  sim <- loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite"),
    loadFromCache = FALSE,
    addToCache = FALSE
  )
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
    loadSimulation(
      system.file("extdata", "Aciclovir.pkml", package = "ospsuite"),
      loadFromCache = FALSE,
      addToCache = FALSE
    ),
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

  # The buckets of the simulations take the value of their group, in the
  # order of the variable paths of their batches
  task <- ParameterIdentification$new(
    simulations = simulations,
    parameters = groups,
    outputMappings = lapply(simulations, function(sim) {
      mapping <- PIOutputMapping$new(quantity = testQuantity(sim))
      mapping$addObservedDataSets(testObservedData())
      mapping
    })
  )
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  values <- c(0.5, -0.2, 0.3, 0.05, 2.5)
  priv$.applyParameterValues(values)
  expected <- list(
    list(
      parameters = values[c(1, 2, 5)],
      parameterPaths = c(clPath, lipophilicityPath, volumePath)
    ),
    list(
      parameters = values[c(1, 3)],
      parameterPaths = c(clPath, lipophilicityPath)
    )
  )
  for (idx in 1:2) {
    simId <- ids[[idx]]
    batch <- priv$.simulationBatches[[simId]]
    expect_equal(
      priv$.variableParameters[[simId]],
      stats::setNames(
        expected[[idx]]$parameters,
        expected[[idx]]$parameterPaths
      )
    )
    expect_equal(
      priv$.variableMolecules[[simId]],
      stats::setNames(values[[4]], stateVariableParameterPath)
    )
    expect_identical(
      batch$getVariableParameters(),
      names(priv$.variableParameters[[simId]])
    )
    expect_identical(
      batch$getVariableMolecules(),
      names(priv$.variableMolecules[[simId]])
    )
  }
})

test_that("parameter targets are resolved again when the batches are built", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.parameterTargets <- list()
  priv$.batchInitialization()

  expect_identical(
    priv$.parameterTargets,
    .resolveParameterTargets(task$parameters)
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
    tasks[[name]]$.__enclos_env__$private$.objectiveFunction(values)
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

test_that(".simulateOutputs reads the outputs of two simulations", {
  task <- clarithromycinTasks()$both
  priv <- task$.__enclos_env__$private
  currVals <- currStartValues(task)

  full <- priv$.evaluate(currVals)
  simulated <- priv$.simulateOutputs(currVals)

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

test_that(".evaluate includes simulated and observed data", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()

  dcList <- priv$.evaluate(currStartValues(task))
  df <- dcList[[1]]$toDataFrame()

  expect_true(all(c("simulated", "observed") %in% df$dataType))
})

test_that("objective function reads the observed data once and reuses them", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  currVals <- currStartValues(task)
  observedDataReads <- localObservedDataReads()

  expect_null(priv$.observedData)

  cost1 <- priv$.objectiveFunction(currVals)
  observedData <- priv$.observedData
  expect_false(is.null(observedData))
  expect_equal(observedDataReads$reads, 1)

  cost2 <- priv$.objectiveFunction(currVals)
  priv$.objectiveFunction(currVals * 2)
  expect_equal(observedDataReads$reads, 1)
  expect_identical(priv$.observedData, observedData)
  expect_identical(cost2, cost1)
})

test_that("observed data equal the observed rows of a full evaluation", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  currVals <- currStartValues(task)

  full <- priv$.evaluate(currVals)[[1]]$toDataFrame()
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

  priv$.objectiveFunction(currVals)
  observed <- priv$.observedData[[1]]

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
applyCostSetting <- function(task, scaling = "lin", options = list()) {
  mappings <- task$outputMappings
  scaling <- rep_len(scaling, length(mappings))
  for (idx in seq_along(mappings)) {
    mappings[[idx]]$scaling <- scaling[[idx]]
  }
  task$configuration$objectiveFunctionOptions <- utils::modifyList(
    PIConfiguration$new()$objectiveFunctionOptions,
    options
  )
  task$.__enclos_env__$private$.batchInitialization()
}

# Expects identical results and warnings from the objective function and from
# the objective function of 2.2.0.9009, for each parameter value, in this
# order. The first evaluation reads the observed data, the later ones reuse
# them.
expectFrozenObjective <- function(task, values, bootstrapSeed = NULL) {
  priv <- task$.__enclos_env__$private
  for (value in values) {
    testthat::expect_identical(
      withWarningMessages(
        priv$.objectiveFunction(value, bootstrapSeed = bootstrapSeed)
      ),
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
  sim <- ospsuite::loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite"),
    loadFromCache = FALSE,
    addToCache = FALSE
  )
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

test_that("objective function equals 2.2.0.9009 with an LLOQ", {
  expectFrozenForSettings(
    twoOutputsTask(lloq = 0.5),
    list(
      list(),
      list(scaling = "log"),
      list(options = list(objectiveFunctionType = "m3", linScaleCV = 0.2)),
      list(
        scaling = "log",
        options = list(objectiveFunctionType = "m3", logScaleSD = 0.086)
      )
    ),
    lipophilicityValues
  )
})

test_that("objective function passes the data of 2.2.0.9009 to M3 with two LLOQs in an output mapping", {
  # The frozen objective function calls the M3 contribution of the package
  # (see helper-frozen-objective-function.R), so this shows that both pass
  # the same data to it. With "lsq", the LLOQ rule of 2.2.0.9009 used the
  # lowest LLOQ for both data sets (see the next test).
  dataSets <- testObservedDataMultiple()
  dataSets$dataSet1$LLOQ <- 0.5
  dataSets$dataSet2$LLOQ <- 2
  expectFrozenForSettings(
    aciclovirTask(stats::setNames(list(dataSets), aciclovirPlasmaPaths[[1]])),
    list(
      list(options = list(objectiveFunctionType = "m3", linScaleCV = 0.2)),
      list(
        scaling = "log",
        options = list(objectiveFunctionType = "m3", logScaleSD = 0.086)
      )
    ),
    lipophilicityValues
  )
})

test_that("values below the LLOQ contribute alike in one or in two output mappings", {
  dataSets <- testObservedDataMultiple()
  dataSets$dataSet1$LLOQ <- 0.5
  dataSets$dataSet2$LLOQ <- 2
  # The cost terms with the data sets in the given output mappings. Each data
  # set has its own LLOQ, so the cost of one output mapping with both equals
  # the sum of the costs of one output mapping per data set, which has one
  # LLOQ and equals 2.2.0.9009 (see the tests above).
  costVariables <- function(dataSetsByMapping, scaling, options) {
    sim <- ospsuite::loadSimulation(
      system.file("extdata", "Aciclovir.pkml", package = "ospsuite"),
      loadFromCache = FALSE,
      addToCache = FALSE
    )
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
    applyCostSetting(task, scaling = scaling, options = options)
    task$.__enclos_env__$private$.objectiveFunction(
      lipophilicityValues[[1]]
    )$costVariables
  }

  for (setting in list(
    list(scaling = "lin", options = list()),
    list(scaling = "log", options = list()),
    list(
      scaling = "lin",
      options = list(objectiveFunctionType = "m3", linScaleCV = 0.2)
    ),
    list(
      scaling = "log",
      options = list(objectiveFunctionType = "m3", logScaleSD = 0.086)
    )
  )) {
    together <- costVariables(
      list(unname(dataSets)),
      setting$scaling,
      setting$options
    )
    separate <- costVariables(
      list(dataSets$dataSet1, dataSets$dataSet2),
      setting$scaling,
      setting$options
    )
    if (identical(setting$options$objectiveFunctionType, "m3")) {
      expect_gt(together$M3Contribution, 0)
    }
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
  logYValues <- task$.__enclos_env__$private$.observedData[[1]]$logYValues
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
# weight, the same data times 1.5 without the last point, with a data weight
# per point, and molar data. The second output has molar data with an LLOQ,
# with x and y transformations.
#
# With `lloq = TRUE`, the first output has only the first two data sets, both
# with the same LLOQ, and a y factor but no y offset. The objective function
# equals that of 2.2.0.9009 only for such data, where all observed values of
# an output mapping have the same LLOQ and no y offset: it now leaves the
# values without an LLOQ uncensored, and a y offset changes the value below
# the LLOQ and the standard deviation of "m3" (#331).
severalDataSetsTask <- function(lloq = FALSE) {
  dataSets <- testObservedDataMultiple()
  if (lloq) {
    dataSets$dataSet1$LLOQ <- 1
    dataSets$dataSet2$LLOQ <- 1
  } else {
    dataSets$dataSet3 <- molarDataSet("dataSet3")
  }
  task <- aciclovirTask(stats::setNames(
    list(dataSets, molarDataSet("dataSet4", lloq = 1000)),
    aciclovirPlasmaPaths
  ))
  task$outputMappings[[1]]$setDataTransformations(
    yOffsets = if (lloq) 0 else 0.05,
    yFactors = 0.9
  )
  task$outputMappings[[1]]$setDataWeights(
    list(dataSet1 = 2, dataSet2 = seq(1, 0.1, length.out = 10))
  )
  # M3 compares the censored observations with the simulated values at the
  # same times, so the transformed times must be times of the simulation
  # results, which are single precision numbers
  task$outputMappings[[2]]$setDataTransformations(
    xOffsets = 2,
    xFactors = 1.5,
    yFactors = 1.2
  )
  task
}

test_that("objective function equals 2.2.0.9009 for several data sets", {
  expectFrozenForSettings(
    severalDataSetsTask(),
    list(
      list(),
      list(scaling = "log"),
      list(options = list(residualWeightingMethod = "error")),
      list(scaling = "log", options = list(residualWeightingMethod = "error")),
      list(
        scaling = c("lin", "log"),
        options = list(robustMethod = "huber", scaleVar = TRUE)
      )
    ),
    lipophilicityValues
  )
  expectFrozenForSettings(
    severalDataSetsTask(lloq = TRUE),
    list(
      list(),
      list(scaling = "log"),
      list(options = list(objectiveFunctionType = "m3", linScaleCV = 0.2)),
      list(
        scaling = "log",
        options = list(objectiveFunctionType = "m3", logScaleSD = 0.086)
      )
    ),
    lipophilicityValues
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
  priv <- task$.__enclos_env__$private
  applyCostSetting(task)
  priv$.gprModels <- .prepareGPRModels(priv$.outputMappings)

  for (seed in 1:2) {
    expectFrozenObjective(task, lipophilicityValues, bootstrapSeed = seed)
    expect_false(identical(
      mapping$dataWeights,
      priv$.initialOutputMappingState$dataSetWeights[[1]]
    ))
  }
  applyCostSetting(task, scaling = "log")
  expectFrozenObjective(task, lipophilicityValues[[2]], bootstrapSeed = 3L)

  # The restored weights
  priv$.restoreOutputMappingsState()
  expectFrozenObjective(task, lipophilicityValues[[2]])
})

test_that(".combineCostTerms equals the sum of the costs of 2.2.0.9009", {
  kernelTerms <- function(index) {
    .costKernel(
      simulatedYApprox = .simulatedAtObservedTimes(
        simulatedX = c(0, 1, 2, 3),
        simulatedY = c(1, 2, 3, 2) * index,
        observedX = c(0.5, 1.5, 2.5)
      ),
      observedX = c(0.5, 1.5, 2.5),
      observedY = c(1.4, 2.7, 2.2),
      userWeights = c(NA, 2, 0.5),
      yErrorValues = NULL,
      yErrorType = NULL,
      residualWeightingMethod = "none",
      robustMethod = "huber",
      scaleVar = FALSE,
      censoredContribution = 0,
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
    .createErrorCostStructure(index = 2L),
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
  lloqDf <- obsVsPredDf
  lloqDf$lloq <- 2.5
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
    list(df = lloqDf, objectiveFunctionType = "lsq"),
    list(
      df = lloqDf,
      objectiveFunctionType = "m3",
      scaling = "lin",
      linScaleCV = 0.2
    ),
    list(df = obsVsPredDf, scaleVar = TRUE),
    list(df = infiniteDf)
  )
  for (setting in settings) {
    expect_identical(
      withWarningMessages(do.call(.calculateCostMetrics, setting)),
      withWarningMessages(do.call(frozenCalculateCostMetrics, setting))
    )
  }
})

test_that("observed data are read again per bootstrap sample and after it", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  currVals <- currStartValues(task)
  original <- lapply(priv$.outputMappings, .prepareObservedData)

  # The values of aggregated data are resampled from a GPR model
  suppressMessages(
    priv$.gprModels <- .prepareGPRModels(priv$.outputMappings)
  )
  samples <- list()
  for (seed in 1:2) {
    priv$.objectiveFunction(currVals, bootstrapSeed = seed)
    samples[[seed]] <- priv$.observedData
    # The observed data of the evaluation are those of the resampled data
    expect_identical(
      samples[[seed]],
      lapply(priv$.outputMappings, .prepareObservedData)
    )
    expect_false(identical(samples[[seed]][[1]]$yValues, original[[1]]$yValues))
  }
  expect_false(identical(samples[[1]][[1]]$yValues, samples[[2]][[1]]$yValues))

  # The restored data are read again
  priv$.restoreOutputMappingsState()
  expect_null(priv$.observedData)
  priv$.objectiveFunction(currVals)
  expect_identical(priv$.observedData, original)
})

test_that("observed data are read again at the start of every public call", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  currVals <- currStartValues(task)
  costBefore <- priv$.objectiveFunction(currVals)$modelCost

  # Public methods start with the batch initialization, so a change of the
  # data transformations between two calls takes effect
  task$outputMappings[[1]]$setDataTransformations(yFactors = 0.5)
  priv$.batchInitialization()
  expect_null(priv$.observedData)
  costAfter <- priv$.objectiveFunction(currVals)$modelCost

  freshTask <- testPiTask()
  freshTask$outputMappings[[1]]$setDataTransformations(yFactors = 0.5)
  freshPriv <- freshTask$.__enclos_env__$private
  freshPriv$.batchInitialization()

  expect_false(costAfter == costBefore)
  expect_identical(
    costAfter,
    freshPriv$.objectiveFunction(currStartValues(freshTask))$modelCost
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

test_that("new observed times without simulated values are warned about", {
  # Mass concentrations at times in min that are not observed times of the
  # task. The output time points of Aciclovir.pkml end at 1440 min.
  laterData <- function(xValues, yValues, lloq = NULL) {
    dataSet <- DataSet$new(name = "later")
    dataSet$xUnit <- "min"
    dataSet$yDimension <- ospDimensions$`Concentration (mass)`
    dataSet$yUnit <- "mg/l"
    dataSet$setValues(xValues = xValues, yValues = yValues)
    if (!is.null(lloq)) {
      dataSet$LLOQ <- lloq
    }
    dataSet
  }
  # The objective function values and the warnings of a grid search
  gridSearch <- function(task) {
    warnings <- character()
    grid <- withCallingHandlers(
      task$gridSearch(lower = -0.5, upper = 0.5, totalEvaluations = 2),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    list(ofv = grid$ofv, warnings = warnings)
  }
  # Grid searches before and after `dataSet` is added to the output mapping,
  # and once more
  searchesAroundNewData <- function(dataSet, lloq = NULL, options = NULL) {
    task <- testPiTask()
    if (!is.null(options)) {
      task$configuration$objectiveFunctionOptions <- options
    }
    mapping <- task$outputMappings[[1]]
    if (!is.null(lloq)) {
      firstDataSet <- mapping$observedDataSets[[1]]
      firstDataSet$LLOQ <- lloq
    }
    before <- gridSearch(task)
    mapping$addObservedDataSets(dataSet)
    list(
      before = before,
      after = gridSearch(task),
      again = gridSearch(task),
      warning = messages$warningObservedTimesNotSimulated(
        1,
        mapping$quantity$path
      )
    )
  }
  # Every call after the change warns once, besides the warnings of the
  # infinite costs
  expectWarnedCalls <- function(searches) {
    expect_length(searches$before$warnings, 0)
    expect_true(all(is.finite(searches$before$ofv)))
    for (search in searches[c("after", "again")]) {
      expect_identical(sum(search$warnings == searches$warning), 1L)
      expect_identical(search$ofv, c(Inf, Inf))
    }
  }

  # Least squares with times after the last simulated time
  expectWarnedCalls(
    searchesAroundNewData(laterData(c(97, 1500, 3000), c(1, 0.05, 0.01)))
  )

  # M3 with censored values at times that were not simulated
  expectWarnedCalls(searchesAroundNewData(
    laterData(c(97, 193, 1013), rep(0.1, 3), lloq = 0.5),
    lloq = 0.5,
    options = list(objectiveFunctionType = "m3", linScaleCV = 0.2)
  ))

  # With least squares, new times inside the simulated times are
  # interpolated, without a warning
  inside <- searchesAroundNewData(laterData(c(97, 193, 1013), c(3, 2, 0.5)))
  expect_length(inside$after$warnings, 0)
  expect_true(all(is.finite(inside$after$ofv)))
  expect_false(identical(inside$after$ofv, inside$before$ofv))

  # Observed times that were output time points at the first call are not
  # new. With M3 and x transformations set before the first call, the
  # censored values have no simulated value at exactly their times, which
  # are not exact in single precision, so the cost is infinite in every
  # call. A later call without a change does not warn about new times.
  task <- testPiTask()
  task$configuration$objectiveFunctionOptions <- list(
    objectiveFunctionType = "m3",
    linScaleCV = 0.2
  )
  mapping <- task$outputMappings[[1]]
  firstDataSet <- mapping$observedDataSets[[1]]
  firstDataSet$LLOQ <- 0.5
  mapping$setDataTransformations(xOffsets = 0.1, xFactors = 1.05)
  first <- gridSearch(task)
  expect_identical(first$ofv, c(Inf, Inf))
  again <- gridSearch(task)
  expect_false(
    messages$warningObservedTimesNotSimulated(1, mapping$quantity$path) %in%
      again$warnings
  )
  expect_identical(again, first)
})

test_that("the LLOQ rule stops when simulated values are missing", {
  costControl <- PIConfiguration$new()$objectiveFunctionOptions
  costControl$scaling <- "lin"
  costTerms <- function(lloq) {
    dataSet <- testObservedData()$`AciclovirLaskinData.Laskin 1982.Group A`
    if (!is.null(lloq)) {
      dataSet$LLOQ <- lloq
    }
    mapping <- PIOutputMapping$new(quantity = testQuantity())
    mapping$addObservedDataSets(dataSet)
    .mappingCostTerms(
      simulated = list(xValues = c(0, 60, 1e4), yValues = c(0, NA, 1)),
      observed = .prepareObservedData(mapping),
      dataWeights = mapping$dataWeights,
      costControl = costControl,
      index = 1L
    )
  }

  expect_error(
    costTerms(lloq = 0.5),
    messages$errorSimulatedValuesMissing(),
    fixed = TRUE
  )
  # Without an LLOQ, a missing simulated value is left out
  expect_true(is.finite(costTerms(lloq = NULL)$modelCost))
})

# The LLOQ with y transformations (#331), for the example of the issue: values
# of 10, 5 and 1 nmol/l at 60, 120 and 180 min, where 1 nmol/l is a value
# below the LLOQ of 2 nmol/l, which the importer stores as half the LLOQ
blqDataSet <- function(
  name = "withLloq",
  lloq = 2,
  yValues = c(10, 5, 1),
  yUnit = "nmol/l"
) {
  dataSet <- ospsuite::DataSet$new(name = name)
  dataSet$xUnit <- ospsuite::ospUnits$Time$min
  dataSet$yDimension <- ospsuite::ospDimensions$`Concentration (molar)`
  dataSet$yUnit <- yUnit
  dataSet$setValues(xValues = c(60, 120, 180), yValues = yValues)
  dataSet$LLOQ <- lloq
  dataSet
}

# The y transformations of the example, and one with an offset and a factor,
# with, in nmol/l, the transformed values and LLOQ and the transformed value
# of 0, the lower limit of the values below the LLOQ. A negative y factor sets
# the LLOQ to NA.
blqTransformations <- list(
  none = list(
    yOffsets = 0,
    yFactors = 1,
    yValues = c(10, 5, 1),
    lloq = 2,
    zero = 0
  ),
  offset2 = list(
    yOffsets = 2,
    yFactors = 1,
    yValues = c(12, 7, 3),
    lloq = 4,
    zero = 2
  ),
  offset2Factor3 = list(
    yOffsets = 2,
    yFactors = 3,
    yValues = c(36, 21, 9),
    lloq = 12,
    zero = 6
  ),
  offsetMinus2 = list(
    yOffsets = -2,
    yFactors = 1,
    yValues = c(8, 3, -1),
    lloq = 0,
    zero = -2
  ),
  offsetMinus3 = list(
    yOffsets = -3,
    yFactors = 1,
    yValues = c(7, 2, -2),
    lloq = -1,
    zero = -3
  ),
  factorMinus2 = list(
    yOffsets = 0,
    yFactors = -2,
    yValues = c(-20, -10, -2),
    lloq = NA_real_,
    zero = 0
  )
)
# The transformations with a positive value below the LLOQ, which log scaling
# needs
positiveBlq <- c("none", "offset2", "offset2Factor3")

# nmol/l in the base unit of the output, µmol/l
nmolPerL <- 1e-3

# The tolerance for observed values, which ospsuite stores in single precision
singlePrecision <- 1e-6

# The prepared observed data of an output mapping with the data sets and the
# y transformations, without the warnings of ospsuite about the LLOQ
blqObservedData <- function(dataSets, yOffsets = 0, yFactors = 1) {
  mapping <- PIOutputMapping$new(quantity = testQuantity())
  mapping$addObservedDataSets(dataSets)
  mapping$setDataTransformations(yOffsets = yOffsets, yFactors = yFactors)
  withoutLloqWarnings(.prepareObservedData(mapping))
}

withoutLloqWarnings <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("LLOQ", conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  })
}

# The cost terms of the prepared observed data, with the given simulated
# values at the observed times, in nmol/l or in the base unit for
# `unitFactor = 1`, and other values at 0 and 240 min
blqCostTerms <- function(
  observed,
  simulatedAtObserved,
  options = list(),
  scaling = "lin",
  unitFactor = nmolPerL
) {
  costControl <- utils::modifyList(
    PIConfiguration$new()$objectiveFunctionOptions,
    options
  )
  costControl$scaling <- scaling
  .mappingCostTerms(
    simulated = list(
      xValues = c(0, 60, 120, 180, 240),
      yValues = c(20, simulatedAtObserved, 0.1) * unitFactor
    ),
    observed = observed,
    dataWeights = NULL,
    costControl = costControl,
    index = 1L,
    quantityPath = testQuantity()$path
  )
}

# Simulated values 1 nmol/l above the first two values, and `fromLloq` nmol/l
# from the LLOQ at the third, or from the third value without an LLOQ
blqSimulated <- function(transformation, fromLloq = -0.5) {
  third <- if (is.na(transformation$lloq)) {
    transformation$yValues[[3]]
  } else {
    transformation$lloq
  }
  c(transformation$yValues[1:2] + 1, third + fromLloq)
}

test_that("prepared observed data hold the transformed LLOQ and the value below it", {
  for (name in names(blqTransformations)) {
    transformation <- blqTransformations[[name]]
    observed <- blqObservedData(
      blqDataSet(),
      transformation$yOffsets,
      transformation$yFactors
    )
    lloq <- rep(transformation$lloq, 3)
    expect_equal(
      observed[c("yValues", "lloq", "transformedZero", "blqValue")],
      list(
        yValues = transformation$yValues * nmolPerL,
        lloq = lloq * nmolPerL,
        transformedZero = rep(transformation$zero, 3) * nmolPerL,
        # The transformed value below the LLOQ, the third value
        blqValue = ifelse(is.na(lloq), NA, transformation$yValues[[3]]) *
          nmolPerL
      ),
      tolerance = singlePrecision,
      info = name
    )
  }
})

test_that("the LLOQ rule gives values below the LLOQ a residual of 0 with y transformations", {
  for (name in names(blqTransformations)) {
    transformation <- blqTransformations[[name]]
    observed <- blqObservedData(
      blqDataSet(),
      transformation$yOffsets,
      transformation$yFactors
    )
    terms <- blqCostTerms(observed, blqSimulated(transformation))
    # Without an LLOQ, the simulated value is not replaced
    third <- if (is.na(transformation$lloq)) -0.5 else 0
    expect_equal(
      terms$rawResiduals,
      c(1, 1, third) * nmolPerL,
      tolerance = singlePrecision,
      info = name
    )
  }

  # With log scaling
  for (name in positiveBlq) {
    transformation <- blqTransformations[[name]]
    observed <- blqObservedData(
      blqDataSet(),
      transformation$yOffsets,
      transformation$yFactors
    )
    terms <- blqCostTerms(
      observed,
      blqSimulated(transformation),
      scaling = "log"
    )
    yValues <- transformation$yValues
    expect_equal(
      terms$rawResiduals,
      c(log((yValues[1:2] + 1) / yValues[1:2]), 0),
      tolerance = singlePrecision,
      info = name
    )
  }
  # Without an LLOQ, the simulated values are not replaced
  observed <- blqObservedData(blqDataSet(), yFactors = -2)
  expect_equal(
    blqCostTerms(observed, c(9, 4, 0.5), scaling = "log")$ySimulated,
    log(c(9, 4, 0.5) * nmolPerL)
  )
})

test_that("the LLOQ rule replaces simulated values by the value below the LLOQ of each data set", {
  # Two data sets with the same LLOQ of 4 µmol/l after their transformations
  # and different values below it: 3 µmol/l with an LLOQ of 2 and a y offset
  # of 2, and 2 µmol/l with an LLOQ of 4 without an offset. The values are in
  # the base unit, so that both LLOQs are exactly 4.
  observed <- blqObservedData(
    list(
      blqDataSet("offset", lloq = 2, yUnit = "µmol/l"),
      blqDataSet("noOffset", lloq = 4, yValues = c(12, 7, 2), yUnit = "µmol/l")
    ),
    yOffsets = c(2, 0)
  )
  expect_identical(observed$lloq, rep(4, 6))
  terms <- blqCostTerms(observed, c(9, 6, 3.5), unitFactor = 1)
  expect_equal(
    split(terms$ySimulated, observed$name),
    list(noOffset = c(9, 6, 2), offset = c(9, 6, 3))
  )
})

test_that("M3 calculates the standard deviation from the LLOQ before a y offset", {
  # The simulated value at the censored value is 0.5 nmol/l above the LLOQ,
  # and the standard deviation of the censored value is linScaleCV times the
  # LLOQ of 2 nmol/l times the y factor, whatever the y offset
  for (name in setdiff(names(blqTransformations), "factorMinus2")) {
    transformation <- blqTransformations[[name]]
    observed <- blqObservedData(
      blqDataSet(),
      transformation$yOffsets,
      transformation$yFactors
    )
    terms <- blqCostTerms(
      observed,
      blqSimulated(transformation, fromLloq = 0.5),
      options = list(objectiveFunctionType = "m3", linScaleCV = 0.2)
    )
    expect_equal(
      terms$M3Contribution,
      -2 * log10(stats::pnorm(-0.5 / (0.2 * 2 * transformation$yFactors))),
      tolerance = singlePrecision,
      info = name
    )
  }

  # With log scaling
  for (name in positiveBlq) {
    transformation <- blqTransformations[[name]]
    observed <- blqObservedData(
      blqDataSet(),
      transformation$yOffsets,
      transformation$yFactors
    )
    terms <- blqCostTerms(
      observed,
      blqSimulated(transformation, fromLloq = 0.5),
      options = list(objectiveFunctionType = "m3", logScaleSD = 0.086),
      scaling = "log"
    )
    lloq <- transformation$lloq
    expect_equal(
      terms$M3Contribution,
      -2 * log10(stats::pnorm((log(lloq) - log(lloq + 0.5)) / 0.086)),
      tolerance = singlePrecision,
      info = name
    )
  }

  # A negative y factor leaves the output mapping without an LLOQ
  observed <- blqObservedData(blqDataSet(), yFactors = -2)
  for (scaling in c("lin", "log")) {
    expect_error(
      blqCostTerms(
        observed,
        c(9, 4, 0.5),
        options = list(objectiveFunctionType = "m3"),
        scaling = scaling
      ),
      messages$errorNoLloqForM3(1L, testQuantity()$path),
      fixed = TRUE
    )
  }
})

test_that("an LLOQ that is not positive stops log scaling and linear M3", {
  path <- testQuantity()$path
  expect_identical(
    messages$errorLloqNotPositive("withLloq", "log", 1L, path),
    paste0(
      "The LLOQ of data set 'withLloq' of output mapping 1 ('", path, "'), ",
      "or the values below it, are not positive after the data ",
      "transformations. With log scaling, the LLOQ and the values below it, ",
      "which the importer stores as half the LLOQ, must be positive to have ",
      "a logarithm. Use linear scaling for the output mapping, a positive ",
      "LLOQ, or a y offset greater than minus half the LLOQ."
    )
  )
  expect_identical(
    messages$errorLloqNotPositive(c("first", "second"), "lin"),
    paste0(
      "The LLOQs of data sets 'first', 'second' are not positive before the ",
      "data transformations. With objectiveFunctionType 'm3' and linear ",
      "scaling, the standard deviation of the values below the LLOQ is ",
      "linScaleCV times this LLOQ, so it must be positive. Set a positive ",
      "LLOQ, or use objectiveFunctionType 'lsq'."
    )
  )

  # With log scaling, a y offset of minus half the LLOQ or less, with an LLOQ
  # of 2 nmol/l: the value below the LLOQ is not positive, the LLOQ is
  # positive for -1.5 and not positive for the others
  for (yOffsets in c(-1, -1.5, -2, -3)) {
    observed <- blqObservedData(blqDataSet(), yOffsets = yOffsets)
    for (options in list(
      list(),
      list(objectiveFunctionType = "m3", logScaleSD = 0.086)
    )) {
      expect_error(
        blqCostTerms(observed, c(9, 4, 0.5), options, scaling = "log"),
        messages$errorLloqNotPositive("withLloq", "log", 1L, path),
        fixed = TRUE
      )
    }
  }
  # Just above minus half the LLOQ
  observed <- blqObservedData(blqDataSet(), yOffsets = -0.99)
  expect_true(is.finite(
    blqCostTerms(observed, c(9, 4, 0.5), scaling = "log")$modelCost
  ))

  # With linear scaling, an LLOQ that a y offset makes 0 or negative is valid
  # (see above), but not an LLOQ of 0 before the data transformations with M3
  observed <- blqObservedData(blqDataSet(lloq = 0))
  expect_error(
    blqCostTerms(
      observed,
      c(9, 4, 0.5),
      options = list(objectiveFunctionType = "m3", linScaleCV = 0.2)
    ),
    messages$errorLloqNotPositive("withLloq", "lin", 1L, path),
    fixed = TRUE
  )
  expect_true(is.finite(blqCostTerms(observed, c(9, 4, 0.5))$modelCost))

  # The error stops the call
  task <- testPiTask()
  mapping <- task$outputMappings[[1]]
  dataSet <- mapping$observedDataSets[[1]]
  dataSet$LLOQ <- 0.5
  mapping$scaling <- "log"
  mapping$setDataTransformations(yOffsets = -1)
  expect_error(
    withoutLloqWarnings(
      task$gridSearch(lower = -0.5, upper = 0.5, totalEvaluations = 2)
    ),
    messages$errorLloqNotPositive(
      names(mapping$observedDataSets),
      "log",
      1,
      mapping$quantity$path
    ),
    fixed = TRUE
  )
})

test_that("values without an LLOQ are not censored by the LLOQ of another data set", {
  # A data set with an LLOQ of 2 nmol/l, and one whose LLOQ a negative y
  # factor sets to NA. Its transformed values are below 2 nmol/l.
  observed <- blqObservedData(
    list(
      blqDataSet(),
      blqDataSet("negativeFactor", yValues = c(8, 1.5, 0.5))
    ),
    yFactors = c(1, -1)
  )
  simulated <- c(9, 4, 0.5)

  # The LLOQ rule replaces the simulated value below the LLOQ for the first
  # data set only
  for (scaling in c("lin", "log")) {
    transform <- if (scaling == "log") log else identity
    terms <- blqCostTerms(observed, simulated, scaling = scaling)
    expect_equal(
      split(terms$ySimulated, observed$name),
      list(
        negativeFactor = transform(c(9, 4, 0.5) * nmolPerL),
        withLloq = transform(c(9, 4, 1) * nmolPerL)
      ),
      tolerance = singlePrecision,
      info = scaling
    )
  }

  # M3 censors the value below the LLOQ of the first data set only
  terms <- blqCostTerms(
    observed,
    c(9, 4, 2.5),
    options = list(objectiveFunctionType = "m3", linScaleCV = 0.2)
  )
  expect_equal(
    terms$M3Contribution,
    -2 * log10(stats::pnorm((2 - 2.5) / (0.2 * 2))),
    tolerance = singlePrecision
  )

  # A value of the second data set at a time that was not simulated is
  # interpolated, so it has a simulated value. The censored value of the
  # first data set at such a time has none.
  costControl <- list(objectiveFunctionType = "m3", scaling = "lin")
  hasUnsimulatedTimes <- function(simulatedTimes) {
    .hasUnsimulatedObservedTimes(
      simulated = list(
        xValues = simulatedTimes,
        yValues = rep(1, length(simulatedTimes)) * nmolPerL
      ),
      observed = observed,
      costControl = costControl,
      outputTimePoints = simulatedTimes
    )
  }
  expect_false(hasUnsimulatedTimes(c(0, 60, 180, 240)))
  expect_true(hasUnsimulatedTimes(c(0, 60, 120, 240)))
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
