# ParameterIdentification - run()

resetTestFactories()

test_that("run() errors if initial simulation fails", {
  modPiTask <- testModifiedTask()
  suppressMessages(suppressWarnings(
    expect_error(
      modPiTask$run(),
      messages$initialSimulationError()
    )
  ))
})


# BOBYQA Algorithm (Default)

test_that("run() runs successfully using default BOBYQA algorithm", {
  piTask <- testPiTask()
  piTask$configuration$autoEstimateCI <- FALSE
  startValue <- piTask$parameters[[1]]$startValue

  expect_no_error(
    expect_message(
      piResults <- piTask$run(),
      messages$optimizationAlgorithm("BOBYQA", startValue, FALSE),
      fixed = TRUE
    )
  )
  piResults$.__enclos_env__$private$.result$elapsed <- 0
  expect_snapshot_value(
    piResults$toDataFrame(),
    style = "deparse",
    tolerance = 1e-03
  )
})

test_that("run() outputs expected evaluation feedback using BOBYQA algorithm", {
  piTask <- testPiTask()
  piTask$configuration$algorithm <- "BOBYQA"
  piTask$configuration$printEvaluationFeedback <- TRUE
  piTask$configuration$algorithmOptions <- list(maxeval = 3)
  piTask$configuration$autoEstimateCI <- FALSE

  evalOutput <- capture_output(
    suppressMessages(
      temp <- piTask$run()
    )
  )
  expect_snapshot_value(evalOutput, style = "deparse", tolerance = 1)
})

test_that("run() stores best running cost in costDetails", {
  piTask <- testPiTask()
  piTask$configuration$algorithm <- "BOBYQA"
  piTask$configuration$algorithmOptions <- list(maxeval = 3)
  piTask$configuration$autoEstimateCI <- FALSE

  suppressMessages(result <- piTask$run())
  resultList <- result$toList()

  expect_equal(resultList$objectiveValue, resultList$costDetails$modelCost)
})

test_that("run() succeeds with a state-variable optimization parameter (#156)", {
  piTask <- testStateVariableMixedTask()
  piTask$configuration <- lowIterPiConfiguration()
  piTask$configuration$autoEstimateCI <- FALSE

  suppressMessages(
    expect_no_error(piResults <- piTask$run())
  )
  expect_true(is.finite(piResults$toList()$objectiveValue))
})


# HJBK Algorithm

test_that("run() fails with HJKB algorithm and one parameter", {
  piTask <- testPiTask()
  piTask$configuration$algorithm <- "HJKB"
  startValue <- piTask$parameters[[1]]$startValue

  expect_error(
    expect_message(
      piResults <- piTask$run(),
      messages$optimizationAlgorithm("HJKB", startValue, FALSE),
      fixed = TRUE
    )
  )
})


# DEoptim Algorithm

test_that("run() runs successfully using DEoptim algorithm", {
  piTask <- testPiTask()
  piTask$configuration$algorithm <- "DEoptim"
  piTask$configuration$printEvaluationFeedback <- FALSE
  piTask$configuration$algorithmOptions <- list(itermax = 3, trace = FALSE)
  piTask$configuration$autoEstimateCI <- FALSE
  startValue <- piTask$parameters[[1]]$startValue

  expect_no_error(
    expect_message(
      piResults <- piTask$run(),
      messages$optimizationAlgorithm("DEoptim", startValue, FALSE),
      fixed = TRUE
    )
  )
})


# User weights

yLen1 <- length(testObservedDataMultiple()[[1]]$yValues)
yLen2 <- length(testObservedDataMultiple()[[2]]$yValues)

test_that("run() works with one dataset and scalar weight", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity())
  outputMapping$addObservedDataSets(testObservedDataMultiple()[[1]])
  outputMapping$setDataWeights(list(dataSet1 = 2))

  piTask <- ParameterIdentification$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = outputMapping,
    configuration = lowIterPiConfiguration()
  )
  piTask$configuration$autoEstimateCI <- FALSE

  suppressMessages(result <- piTask$run())
  expect_equal(result$toList()$objectiveValue, 3112.517, tolerance = 0.1)
})

test_that("run() works with two datasets and single vector weight", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity())
  outputMapping$addObservedDataSets(testObservedDataMultiple())
  outputMapping$setDataWeights(list(dataSet2 = rep(2, yLen2)))

  piTask <- ParameterIdentification$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = outputMapping,
    configuration = lowIterPiConfiguration()
  )
  piTask$configuration$autoEstimateCI <- FALSE

  suppressMessages(result <- piTask$run())
  expect_equal(result$toList()$objectiveValue, 3220.573, tolerance = 0.1)
})

test_that("run() works with two datasets and individual weights", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity())
  outputMapping$addObservedDataSets(testObservedDataMultiple())
  outputMapping$setDataWeights(
    list(dataSet1 = rep(2, yLen1), dataSet2 = rep(1.5, yLen2))
  )

  piTask <- ParameterIdentification$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = outputMapping,
    configuration = lowIterPiConfiguration()
  )
  piTask$configuration$autoEstimateCI <- FALSE

  suppressMessages(result <- piTask$run())
  expect_equal(result$toList()$objectiveValue, 4486.392, tolerance = 0.1)
})

test_that("gridSearch OFVs are invariant to a non-base parameter unit (#298)", {
  pkmlPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  clPath <- "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS-Aciclovir|TSspec"
  outputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  observed <- testObservedData()$`AciclovirLaskinData.Laskin 1982.Group A`

  # Each task loads its own simulation and builds its own PIParameters, so that
  # assigning $unit cannot leak into the shared module-level fixtures.
  clearanceTask <- function(unit) {
    sim <- loadSimulation(pkmlPath, loadFromCache = FALSE, addToCache = FALSE)
    piParameter <- PIParameters$new(
      parameters = list(getParameter(clPath, container = sim))
    )
    piParameter$unit <- unit

    mapping <- PIOutputMapping$new(
      quantity = getQuantity(outputPath, container = sim)
    )
    mapping$addObservedDataSets(observed)

    ParameterIdentification$new(
      simulations = sim,
      parameters = piParameter,
      outputMappings = mapping
    )
  }

  # [1e-4, 1e-3] 1/min and [6e-3, 6e-2] 1/h are the same physical interval.
  # Explicit bounds mean the stale base-unit min/max are never consulted.
  baseGrid <- suppressMessages(
    clearanceTask(ospUnits$`Inversed time`$`1/min`)$gridSearch(
      lower = 1e-4,
      upper = 1e-3,
      totalEvaluations = 3
    )
  )
  hourGrid <- suppressMessages(
    clearanceTask(ospUnits$`Inversed time`$`1/h`)$gridSearch(
      lower = 1e-4 * 60,
      upper = 1e-3 * 60,
      totalEvaluations = 3
    )
  )

  expect_equal(hourGrid$ofv, baseGrid$ofv, tolerance = 1e-6)
})

test_that("a non-base state-variable unit reaches the molecules bucket in base units (#298)", {
  sim <- loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite"),
    loadFromCache = FALSE,
    addToCache = FALSE
  )

  # Organism|Lumen|Stomach|Liquid is RHS-defined, so .setVariableValue() routes
  # it into the molecules bucket rather than the parameters bucket. Its base
  # unit is l, so values declared in ml must arrive divided by 1000.
  piParameter <- PIParameters$new(
    parameters = list(getParameter(stateVariableParameterPath, container = sim))
  )
  piParameter$unit <- ospUnits$Volume$ml
  # Start value first, then max, then min: the bound setters cross-validate
  # against startValue and against the base-unit bounds left from construction,
  # because changing $unit does not rescale them (#246).
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

test_that("run() reports the estimate in the declared unit and applies the matching base value", {
  # Verifies the reporting contract, not the #298 conversion. The estimate is
  # reported in $unit and .applyFinalValues() writes the matching base value.
  # It cannot detect a conversion regression, because setValue() converts
  # correctly regardless. The conversion itself is covered by the gridSearch
  # and molecules bucket tests above.
  pkmlPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  clPath <- "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS-Aciclovir|TSspec"
  outputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

  # Own simulation and PIParameters, so that assigning $unit cannot leak into
  # the shared module-level fixtures used by unrelated snapshot tests.
  sim <- ospsuite::loadSimulation(
    pkmlPath,
    loadFromCache = FALSE,
    addToCache = FALSE
  )
  piParameter <- PIParameters$new(
    parameters = list(ospsuite::getParameter(clPath, container = sim))
  )
  piParameter$unit <- ospUnits$`Inversed time`$`1/h`
  # Start value first, then max, then min: the bound setters cross-validate
  # against startValue and against the base-unit bounds left from
  # construction, because changing $unit does not rescale them (#246).
  piParameter$startValue <- 56.4
  piParameter$maxValue <- 100
  piParameter$minValue <- 10

  mapping <- PIOutputMapping$new(
    quantity = ospsuite::getQuantity(outputPath, container = sim)
  )
  mapping$addObservedDataSets(
    testObservedData()$`AciclovirLaskinData.Laskin 1982.Group A`
  )

  piTask <- ParameterIdentification$new(
    simulations = sim,
    parameters = piParameter,
    outputMappings = mapping,
    configuration = lowIterPiConfiguration(iter = 1)
  )
  piTask$configuration$autoEstimateCI <- FALSE

  suppressMessages(piResult <- piTask$run())
  resultRow <- piResult$toDataFrame()
  modelParameter <- ospsuite::getParameter(clPath, container = sim)

  expect_equal(resultRow$unit, ospUnits$`Inversed time`$`1/h`)
  expect_equal(
    modelParameter$value,
    ospsuite::toBaseUnit(
      quantityOrDimension = modelParameter,
      values = resultRow$estimate,
      unit = resultRow$unit
    ),
    tolerance = 1e-6
  )
})
