# ParameterIdentification - confidence interval

# Hessian CI Method

test_that("estimateCI() works w/o optimization using Hessian", {
  piTask <- createPiTask() # default BOBYQA, hessian

  # estimate CI without prior optimization
  expect_no_error(
    suppressMessages(ciResult <- piTask$estimateCI())
  )
})

test_that("estimateCI() works as expected using Hessian", {
  piTask <- createPiTask() # default BOBYQA, hessian
  piTask$configuration$algorithmOptions <- list(itermax = 3, trace = FALSE)

  suppressMessages(piResult <- piTask$run())
  expect_no_error(
    expect_message(
      ciResult <- piTask$estimateCI(),
      messages$ciMethod("hessian", piTask$parameters[[1]]$currValue, FALSE),
      fixed = TRUE
    )
  )
  ciResult$elapsed <- 0
  expect_snapshot_value(ciResult, style = "deparse", tolerance = 1e-3)
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

  suppressMessages(
    expect_no_error(ciResult <- piTask$estimateCI())
  )

  ciResult$elapsed <- 0
  expect_snapshot_value(ciResult, style = "deparse", tolerance = 1e-3)

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

  suppressMessages(
    expect_no_error(ciResult <- piTask$estimateCI())
  )

  ciResult$elapsed <- 0
  expect_snapshot_value(ciResult, style = "deparse", tolerance = 1e-3)

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

  suppressMessages(temp <- piTaskResample$estimateCI())
  expect_snapshot_value(
    piTaskResample$outputMappings[[1]]$dataWeights,
    style = "deparse", tolerance = 0
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

  suppressMessages(temp <- piTaskResample$estimateCI())
  expect_snapshot_value(
    piTaskResample$outputMappings[[1]]$dataWeights,
    style = "deparse", tolerance = 0
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

  suppressMessages(temp <- piTaskResample$estimateCI())
  expect_snapshot_value(
    piTaskResample$outputMappings[[1]]$dataWeights,
    style = "deparse", tolerance = 0
  )
  expect_snapshot_value(
    lapply(piTaskResample$outputMappings[[1]]$observedDataSets, "[[", "yValues"),
    style = "deparse", tolerance = 1e-6
  )
})
