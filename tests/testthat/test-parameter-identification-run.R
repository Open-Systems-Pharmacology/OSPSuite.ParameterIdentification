# ParameterIdentification - run()

test_that("run() errors if initial simulation fails", {
  modPiTask <- testModifiedTask()
  suppressMessages(suppressWarnings(
    expect_error(
      modPiTask$run(),
      messages$initialSimulationError()
    )
  ))
})

test_that("run() errors on invalid objective function options", {
  piTask <- createPiTask()
  piTask$configuration$objectiveFunctionOptions$objectiveFunctionType <- "invalidType"
  expect_error(
    suppressMessages(piTask$run()),
    "not included in allowed values"
  )

  piTask <- createPiTask()
  piTask$configuration$objectiveFunctionOptions$linScaleCV <- 10
  expect_error(
    suppressMessages(piTask$run()),
    regexp = "out of the allowed range"
  )
})


# BOBYQA Algorithm (Default)

test_that("run() runs successfully using default BOBYQA algorithm", {
  piTask <- createPiTask()
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
  expect_snapshot_value(piResults$toDataFrame(), style = "deparse", tolerance = 1e-03)
})

test_that("run() outputs expected evaluation feedback using BOBYQA algorithm", {
  piTask <- createPiTask()
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


# HJBK Algorithm

test_that("run() fails with HJKB algorithm and one parameter", {
  piTask <- createPiTask()
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
  piTask <- createPiTask()
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
  expect_equal(result$toList()$objectiveValue, 3112.516, tolerance = 1e-3)
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
  expect_equal(result$toList()$objectiveValue, 163.346, tolerance = 1e-3)
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
  expect_equal(result$toList()$objectiveValue, 227.5477, tolerance = 1e-3)
})
