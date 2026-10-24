startValues <- function(task) {
  vapply(task$parameters, function(p) p$startValue, numeric(1))
}

testObjective <- function(task) {
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  PIObjective$new(
    model = priv$.model,
    data = PIObservations$new(priv$.outputMappings),
    objectiveFunctionOptions = task$configuration$objectiveFunctionOptions
  )
}

test_that("evaluate() returns the modelCost of the frozen objective function", {
  task <- testPiTask()
  objective <- testObjective(task)
  currVals <- startValues(task)
  cost <- objective$evaluate(currVals)
  expect_s3_class(cost, "modelCost")
  expect_identical(cost, frozenObjectiveFunction(task, currVals))
})

test_that("evaluate() is the same on every call and leaves the task alone", {
  task <- testPiTask()
  objective <- testObjective(task)
  priv <- task$.__enclos_env__$private
  currVals <- startValues(task)
  expect_identical(objective$evaluate(currVals), objective$evaluate(currVals))
  expect_identical(priv$.fnEvaluations, 0)
})

test_that("evaluate() logs a failed simulation and returns the error cost structure", {
  task <- testModifiedTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  objective <- PIObjective$new(
    model = priv$.model,
    data = PIObservations$new(priv$.outputMappings),
    objectiveFunctionOptions = task$configuration$objectiveFunctionOptions
  )
  suppressMessages(
    expect_message(
      cost <- objective$evaluate(startValues(task)),
      messages$simulationError(),
      fixed = TRUE
    )
  )
  expect_identical(cost, .createErrorCostStructure())
  expect_identical(priv$.model$loggedFailures, 1)
  expect_identical(priv$.failedEvaluations, 0)
})

test_that("the start check of run() logs nothing", {
  task <- testModifiedTask()
  expect_no_message(
    expect_error(task$run(), class = "initialSimulationError")
  )
})

test_that("an infinite cost of successful simulations is not counted as a failed evaluation", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  priv$.objective <- list(
    evaluate = function(values) .createErrorCostStructure(),
    observedData = NULL
  )
  cost <- priv$.evaluationClosure(priv$.objective)(startValues(task))
  expect_identical(priv$.failedEvaluations, 0)
  expect_identical(priv$.lastCostSummary, cost)
})

test_that("observedData of PIObjective is read-only", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  options <- task$configuration$objectiveFunctionOptions
  data <- PIObservations$new(priv$.outputMappings)
  objective <- PIObjective$new(priv$.model, data, options)
  expect_snapshot(objective$observedData <- NULL, error = TRUE)
})

test_that("PIObjective validates the model and the data", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  options <- task$configuration$objectiveFunctionOptions
  data <- PIObservations$new(priv$.outputMappings)
  expect_error(
    PIObjective$new(list(1), data, options),
    messages$errorWrongType("model", "list", "SimulationModel"),
    fixed = TRUE
  )
  expect_error(
    PIObjective$new(priv$.model, list(1), options),
    messages$errorWrongType("data", "list", "PIObservations"),
    fixed = TRUE
  )
})

test_that("withData() gives an objective on the new data and leaves the original alone", {
  task <- testPiTask()
  objective <- testObjective(task)
  data <- PIObservations$new(task$outputMappings)
  currVals <- startValues(task)
  cost <- objective$evaluate(currVals)
  resampled <- objective$withData(data$resample(1))
  expect_identical(resampled$observedData, data$resample(1)$observedData)
  expect_false(identical(resampled$evaluate(currVals), cost))
  expect_identical(objective$evaluate(currVals), cost)
})
