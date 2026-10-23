# ParameterIdentification - confidence interval

resetTestFactories()

# Hessian CI Method

test_that("estimateCI() throws error if optimization has not been executed", {
  piTask <- testPiTask()

  expect_error(
    expect_message(
      ciResult <- piTask$estimateCI(),
      messages$errorMissingOptimizationResult(),
      fixed = TRUE
    )
  )
})

test_that("estimateCI() works as expected using Hessian", {
  piTask <- testPiTask()
  piTask$configuration$algorithmOptions <- list(maxeval = 200L)
  piTask$configuration$autoEstimateCI <- FALSE
  suppressMessages(piResult <- piTask$run())

  expect_no_error(
    expect_message(
      piResult <- piTask$estimateCI(),
      messages$ciMethod("hessian", piTask$parameters[[1]]$currValue, FALSE),
      fixed = TRUE
    )
  )

  expect_snapshot_value(
    piResult$toList()$ciDetails,
    style = "deparse",
    tolerance = 1e-3
  )
  expect_snapshot_value(
    piResult$toDataFrame(),
    style = "deparse",
    tolerance = 1e-3
  )
})

test_that("estimateCI() stops when the estimate fails to simulate", {
  piTask <- testPiTask()
  piTask$configuration <- lowIterPiConfiguration()
  piTask$configuration$autoEstimateCI <- FALSE
  suppressMessages(piTask$run())
  # Every simulation returns no results
  local_mocked_bindings(
    runSimulationBatches = function(simulationBatches, ...) {
      lapply(simulationBatches, function(batch) list())
    },
    .package = "ospsuite"
  )
  expect_error(
    suppressMessages(piTask$estimateCI()),
    class = "initialSimulationError"
  )
})

test_that("run() reads the observed data once when it estimates the CI", {
  piTask <- testPiTask()
  piTask$configuration <- lowIterPiConfiguration()
  expect_true(piTask$configuration$autoEstimateCI)
  observedDataReads <- localObservedDataReads()

  suppressMessages(piResult <- piTask$run())
  expect_false(is.null(piResult$toList()$ciDetails))
  expect_equal(observedDataReads$reads, 1)

  # A later call reads them again
  suppressMessages(piTask$estimateCI())
  expect_equal(observedDataReads$reads, 2)
})


# Bootstrap CI Method

weights <- rep(list(rep(2, 11)), 4) |>
  c(list(c(0, 1, 1, 2, 2, 1, 1, 1, 1, 1, 0.5))) |>
  setNames(names(syntheticObservedData()))

test_that("estimateCI() works with bootstrap and individual data", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity())
  outputMapping$addObservedDataSets(syntheticObservedData())
  outputMapping$setDataWeights(weights)

  piTask <- ParameterIdentification$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = outputMapping,
    configuration = bootstrapPiConfiguration()
  )
  piTask$configuration$algorithmOptions <- list(maxeval = 200L)
  piTask$configuration$autoEstimateCI <- FALSE
  suppressMessages(piResult <- piTask$run())

  suppressMessages(
    expect_no_error(piResult <- piTask$estimateCI())
  )

  expect_snapshot_value(
    piResult$toDataFrame(),
    style = "deparse",
    tolerance = 1e-3
  )

  # Test initial OutputMapping weights are restored
  expect_equal(
    lapply(piTask$outputMappings, `[[`, "dataWeights"),
    list(weights)
  )
})

test_that("estimateCI() works with bootstrap and aggregated data", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity())
  outputMapping$addObservedDataSets(testObservedData())
  outputMapping$setSubjectCount(13)
  yValuesInitial <- outputMapping$observedDataSets[[1]]$yValues

  piTask <- ParameterIdentification$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = outputMapping,
    configuration = bootstrapPiConfiguration()
  )
  piTask$configuration$algorithmOptions <- list(maxeval = 200L)
  piTask$configuration$autoEstimateCI <- FALSE
  suppressMessages(piResult <- piTask$run())

  suppressMessages(
    expect_no_error(piResult <- piTask$estimateCI())
  )

  expect_snapshot_value(
    piResult$toDataFrame(),
    style = "deparse",
    tolerance = 1e-3
  )

  # Test initial OutputMapping yValues are restored
  expect_equal(
    outputMapping$observedDataSets[[1]]$yValues,
    yValuesInitial
  )
})

test_that("bootstrap stops before the first replicate when aggregated data have no subject count", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity())
  outputMapping$addObservedDataSets(testObservedData())
  piTask <- ParameterIdentification$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = outputMapping,
    configuration = bootstrapPiConfiguration()
  )
  piTask$configuration$autoEstimateCI <- FALSE
  suppressMessages(piTask$run())
  expect_snapshot(piTask$estimateCI(), error = TRUE)
})

test_that("run() with autoEstimateCI and bootstrap checks the subject counts before the optimization", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity())
  outputMapping$addObservedDataSets(testObservedData())
  piTask <- ParameterIdentification$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = outputMapping,
    configuration = bootstrapPiConfiguration()
  )
  expect_snapshot(piTask$run(), error = TRUE)
  expect_equal(
    piTask$parameters[[1]]$currValue,
    piTask$parameters[[1]]$startValue
  )
})

test_that("run() that stops on a missing subject count keeps the previous fit", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity())
  outputMapping$addObservedDataSets(testObservedData())
  piTask <- ParameterIdentification$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = outputMapping,
    configuration = bootstrapPiConfiguration()
  )
  piTask$configuration$autoEstimateCI <- FALSE
  suppressMessages(piTask$run())
  piTask$configuration$autoEstimateCI <- TRUE
  expect_snapshot(suppressMessages(piTask$run()), error = TRUE)
  suppressMessages(piTask$configuration$ciMethod <- "hessian")
  expect_no_error(suppressMessages(piTask$estimateCI()))
})

test_that("bootstrap results report the subject counts used", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity())
  outputMapping$addObservedDataSets(testObservedData())
  outputMapping$setSubjectCount(13)
  piTask <- ParameterIdentification$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = outputMapping,
    configuration = bootstrapPiConfiguration(iter = 50, nBootstrap = 2)
  )
  piTask$configuration$autoEstimateCI <- FALSE
  suppressMessages(piTask$run())
  suppressMessages(result <- piTask$estimateCI())
  expect_identical(result$toList()$ciDetails$subjectCounts, 13L)
  expect_gt(result$toDataFrame()$sd[[1]], 0)
})

test_that("estimateCI() with bootstrap gives identical intervals for the same seed on mixed data", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity())
  outputMapping$addObservedDataSets(testObservedData()) # aggregated
  outputMapping$setSubjectCount(13)
  outputMapping$addObservedDataSets(syntheticObservedData()) # individual
  piTask <- ParameterIdentification$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = outputMapping,
    configuration = bootstrapPiConfiguration(iter = 50, nBootstrap = 2)
  )
  piTask$configuration$autoEstimateCI <- FALSE
  suppressMessages(piTask$run())

  suppressMessages(first <- piTask$estimateCI()$toDataFrame())
  suppressMessages(second <- piTask$estimateCI()$toDataFrame())
  expect_gt(first$sd[[1]], 0)
  expect_identical(first, second)
})

test_that("estimateCI() restarts evaluation counter from 1 for each bootstrap iteration", {
  nBootstrap <- 3L

  piTask <- testPiTask()
  piTask$configuration$algorithmOptions <- list(maxeval = 3L)
  piTask$configuration$ciMethod <- "bootstrap"
  piTask$configuration$ciOptions <- list(seed = 2203L, nBootstrap = nBootstrap)
  piTask$configuration$printEvaluationFeedback <- TRUE
  piTask$configuration$autoEstimateCI <- FALSE
  capture_output(suppressMessages(piTask$run()))

  evalOutput <- capture_output(suppressMessages(piTask$estimateCI()))

  fnevals <- parseFnevals(evalOutput)
  expect_equal(sum(fnevals == 1L), nBootstrap)
})

test_that("estimateCI() outputs expected messages for mixed datasets", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity())
  outputMapping$addObservedDataSets(testObservedData()) # aggregated
  outputMapping$setSubjectCount(13)
  outputMapping$addObservedDataSets(syntheticObservedData()) # individual

  piTask <- PIResampleTester$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = outputMapping,
    configuration = bootstrapPiConfiguration()
  )
  piTask$configuration$algorithmOptions <- list(maxeval = 200L)
  piTask$configuration$autoEstimateCI <- FALSE
  suppressMessages(piResult <- piTask$run())

  expect_snapshot(temp <- piTask$estimateCI())
})

test_that("estimateCI() applies bootstrap resampling to individual data", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity())
  outputMapping$addObservedDataSets(syntheticObservedData())

  piTaskResample <- PIResampleTester$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = outputMapping,
    configuration = bootstrapPiConfiguration(1, 1)
  )
  piTaskResample$configuration$algorithmOptions <- list(maxeval = 200L)
  piTaskResample$configuration$autoEstimateCI <- FALSE
  suppressMessages(piResult <- piTaskResample$run())

  suppressMessages(temp <- piTaskResample$estimateCI())
  expect_snapshot_value(
    piTaskResample$outputMappings[[1]]$dataWeights,
    style = "deparse",
    tolerance = 1e-12
  )
})

test_that("estimateCI() applies bootstrap resampling to individual data with initial weights", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity())
  outputMapping$addObservedDataSets(syntheticObservedData())
  outputMapping$setDataWeights(weights)

  piTaskResample <- PIResampleTester$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = outputMapping,
    configuration = bootstrapPiConfiguration(1, 1)
  )
  piTaskResample$configuration$algorithmOptions <- list(maxeval = 200L)
  piTaskResample$configuration$autoEstimateCI <- FALSE
  suppressMessages(piResult <- piTaskResample$run())

  suppressMessages(temp <- piTaskResample$estimateCI())
  expect_snapshot_value(
    piTaskResample$outputMappings[[1]]$dataWeights,
    style = "deparse",
    tolerance = 1e-12
  )
})

test_that("estimateCI() applies bootstrap resampling to mixed data (individual and aggregated)", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity())
  outputMapping$addObservedDataSets(testObservedData())
  outputMapping$setSubjectCount(13)
  outputMapping$addObservedDataSets(syntheticObservedData())
  outputMapping$setDataWeights(
    list("AciclovirLaskinData.Laskin 1982.Group A" = 20)
  )

  piTaskResample <- PIResampleTester$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = outputMapping,
    configuration = bootstrapPiConfiguration(1, 2)
  )
  piTaskResample$configuration$algorithmOptions <- list(maxeval = 200L)
  piTaskResample$configuration$autoEstimateCI <- FALSE
  suppressMessages(piResult <- piTaskResample$run())

  suppressMessages(temp <- piTaskResample$estimateCI())
  expect_snapshot_value(
    piTaskResample$outputMappings[[1]]$dataWeights,
    style = "deparse",
    tolerance = 1e-12
  )
  expect_snapshot_value(
    lapply(
      piTaskResample$outputMappings[[1]]$observedDataSets,
      "[[",
      "yValues"
    ),
    style = "deparse",
    tolerance = 1e-6
  )
})
