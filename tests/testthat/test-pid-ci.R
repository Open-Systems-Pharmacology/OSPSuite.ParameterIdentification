# ParameterIdentification - confidence interval

resetTestFactories()

# Hessian CI Method

test_that("estimateCI() throws error if optimization has not been executed", {
  piTask <- createPiTask()

  expect_error(
    expect_message(
      ciResult <- piTask$estimateCI(),
      messages$errorMissingOptimizationResult(),
      fixed = TRUE
    )
  )
})

test_that("estimateCI() works as expected using Hessian", {
  piTask <- createPiTask() # default BOBYQA, hessian
  piTask$configuration$algorithmOptions <- list(itermax = 3, trace = FALSE)
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
  piTask$configuration$algorithmOptions <- list(itermax = 3, trace = FALSE)
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
  yValuesInitial <- outputMapping$observedDataSets[[1]]$yValues

  piTask <- ParameterIdentification$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = outputMapping,
    configuration = bootstrapPiConfiguration()
  )
  piTask$configuration$algorithmOptions <- list(itermax = 3, trace = FALSE)
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

test_that("estimateCI() outputs expected messages for mixed datasets", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity())
  outputMapping$addObservedDataSets(testObservedData()) # aggregated
  outputMapping$addObservedDataSets(syntheticObservedData()) # individual

  piTask <- PIResampleTester$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = outputMapping,
    configuration = bootstrapPiConfiguration()
  )
  piTask$configuration$algorithmOptions <- list(itermax = 3, trace = FALSE)
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
  piTaskResample$configuration$algorithmOptions <- list(
    itermax = 3,
    trace = FALSE
  )
  piTaskResample$configuration$autoEstimateCI <- FALSE
  suppressMessages(piResult <- piTaskResample$run())

  suppressMessages(temp <- piTaskResample$estimateCI())
  expect_snapshot_value(
    piTaskResample$outputMappings[[1]]$dataWeights,
    style = "deparse",
    tolerance = 0
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
  piTaskResample$configuration$algorithmOptions <- list(
    itermax = 3,
    trace = FALSE
  )
  piTaskResample$configuration$autoEstimateCI <- FALSE
  suppressMessages(piResult <- piTaskResample$run())

  suppressMessages(temp <- piTaskResample$estimateCI())
  expect_snapshot_value(
    piTaskResample$outputMappings[[1]]$dataWeights,
    style = "deparse",
    tolerance = 0
  )
})

test_that("estimateCI() applies bootstrap resampling to mixed data (individual and aggregated)", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity())
  outputMapping$addObservedDataSets(testObservedData())
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
  piTaskResample$configuration$algorithmOptions <- list(
    itermax = 3,
    trace = FALSE
  )
  piTaskResample$configuration$autoEstimateCI <- FALSE
  suppressMessages(piResult <- piTaskResample$run())

  suppressMessages(temp <- piTaskResample$estimateCI())
  expect_snapshot_value(
    piTaskResample$outputMappings[[1]]$dataWeights,
    style = "deparse",
    tolerance = 0
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
