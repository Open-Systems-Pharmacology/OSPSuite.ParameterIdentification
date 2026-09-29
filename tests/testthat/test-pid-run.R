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

test_that("run() stops on unconvertible observed data", {
  # The error is raised by the unit conversion, not reported as a failed
  # simulation
  expect_error(
    suppressMessages(testUnconvertibleDataTask()$run()),
    "Molecular Weight not available",
    fixed = TRUE
  )
})

test_that("a failed simulation is reported by name and reason", {
  modPiTask <- testModifiedTask()
  priv <- modPiTask$.__enclos_env__$private
  startValues <- vapply(
    modPiTask$parameters,
    function(p) p$startValue,
    numeric(1)
  )

  # plotResults() shows the warning of the simulation engine and stops with
  # the name of the failed simulation and the reason from that warning
  engineWarning <- expect_warning(
    plotError <- expect_error(modPiTask$plotResults(par = startValues))
  )
  failedMessage <- messages$errorSimulationsFailed(
    modPiTask$simulations[[1]]$name,
    reasons = conditionMessage(engineWarning)
  )
  expect_identical(conditionMessage(plotError), failedMessage)

  # The objective function reports the same failure without the warning of
  # the simulation engine
  logged <- character()
  expect_no_warning(
    expect_error(
      withCallingHandlers(
        priv$.objectiveFunction(startValues),
        message = function(m) {
          logged <<- c(logged, conditionMessage(m))
          invokeRestart("muffleMessage")
        }
      ),
      messages$initialSimulationError(),
      fixed = TRUE
    )
  )
  expect_true(any(grepl(failedMessage, logged, fixed = TRUE)))
})

test_that("several failed simulations are reported with their reasons", {
  piTask <- testTwoSimulationsTask(failing = 1:2)
  priv <- piTask$.__enclos_env__$private
  priv$.batchInitialization()
  startValue <- piTask$parameters[[1]]$startValue
  simulationNames <- vapply(
    piTask$simulations,
    function(simulation) simulation$name,
    character(1)
  )

  # Both simulations fail for the same reason, which the message gives once
  engineWarnings <- character()
  runError <- expect_error(
    withCallingHandlers(
      priv$.runSimulations(startValue),
      warning = function(w) {
        engineWarnings <<- c(engineWarnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  )
  expect_length(engineWarnings, 2)
  expect_length(unique(engineWarnings), 1)
  failedMessage <- messages$errorSimulationsFailed(
    simulationNames,
    reasons = engineWarnings
  )
  expect_identical(conditionMessage(runError), failedMessage)
  # The simulations share their name, so their positions tell them apart
  expect_match(
    failedMessage,
    paste0(
      "Simulations '",
      simulationNames[[1]],
      "' (position 1), '",
      simulationNames[[2]],
      "' (position 2) failed: ",
      engineWarnings[[1]]
    ),
    fixed = TRUE
  )

  # The objective function logs the same message without the warnings
  priv$.fnEvaluations <- 1
  logged <- character()
  expect_no_warning(
    withCallingHandlers(
      priv$.objectiveFunction(startValue),
      message = function(m) {
        logged <<- c(logged, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
  )
  expect_true(any(grepl(failedMessage, logged, fixed = TRUE)))
})

test_that("a failed simulation with a shared name is named with its position", {
  piTask <- testTwoSimulationsTask(failing = 2)
  priv <- piTask$.__enclos_env__$private
  priv$.batchInitialization()

  runError <- expect_error(
    suppressWarnings(priv$.runSimulations(piTask$parameters[[1]]$startValue))
  )
  expect_match(
    conditionMessage(runError),
    paste0(
      "Simulation '",
      piTask$simulations[[2]]$name,
      "' (position 2) failed: "
    ),
    fixed = TRUE
  )
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

test_that("run() succeeds with a state-variable optimization parameter", {
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

test_that("run() reports the estimate in the declared unit and applies the matching base value", {
  # Verifies the reporting contract, not the unit conversion. The estimate is
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
  # Start, then max, then min: $unit does not rescale the values left from
  # construction, and the bound setters cross-validate against them.
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
  # Independent oracle: 1/h to 1/min is a factor of 60, computed here rather
  # than by re-using the conversion the implementation delegates to (T-6).
  expect_equal(
    modelParameter$value,
    resultRow$estimate / 60,
    tolerance = 1e-6
  )
})
