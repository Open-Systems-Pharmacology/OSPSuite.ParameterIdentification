test_that("evaluate() returns the modelCost of the frozen objective function", {
  task <- testPiTask()
  objective <- taskObjective(task)
  currVals <- currStartValues(task)
  cost <- objective$evaluate(currVals)
  expect_s3_class(cost, "modelCost")
  expect_identical(asFrozenCost(cost), frozenObjectiveFunction(task, currVals))
})

# The task with an LLOQ of 2.5 on its first observed data set
lloqTask <- function(blqMethod = "lloqHalf") {
  task <- testPiTask()
  dataSet <- task$outputMappings[[1]]$observedDataSets[[1]]
  dataSet$LLOQ <- 2.5
  task$configuration$blqMethod <- blqMethod
  task
}

test_that("evaluate() with an LLOQ equals the costs recorded before the BLQ options moved", {
  task <- lloqTask()
  expect_equal(
    taskObjective(task)$evaluate(currStartValues(task))$modelCost,
    870.98815201914113
  )
  task <- lloqTask("m3")
  expect_equal(
    taskObjective(task)$evaluate(currStartValues(task))$modelCost,
    843.05727080080896
  )
})

test_that("withData() keeps blqMethod and cv", {
  task <- lloqTask("m3")
  task$configuration$blqOptions$cv <- 0.5
  objective <- taskObjective(task)
  priv <- task$.__enclos_env__$private
  replicate <- objective$withData(PIObservations$new(priv$.outputMappings))
  values <- currStartValues(task)
  expect_identical(replicate$evaluate(values), objective$evaluate(values))
  expect_gt(objective$evaluate(values)$costVariables$M3Contribution, 0)
})

test_that("evaluate() is the same on every call and leaves the task alone", {
  task <- testPiTask()
  objective <- taskObjective(task)
  priv <- task$.__enclos_env__$private
  currVals <- currStartValues(task)
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
    objectiveFunctionOptions = task$configuration$objectiveFunctionOptions,
    blqMethod = task$configuration$blqMethod,
    blqOptions = task$configuration$blqOptions
  )
  suppressMessages(
    expect_message(
      cost <- objective$evaluate(currStartValues(task)),
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
  cost <- priv$.evaluationClosure(priv$.objective)(currStartValues(task))
  expect_identical(priv$.failedEvaluations, 0)
  expect_identical(priv$.lastCostSummary, cost)
})

test_that("observedData of PIObjective is read-only", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  options <- task$configuration$objectiveFunctionOptions
  data <- PIObservations$new(priv$.outputMappings)
  objective <- PIObjective$new(priv$.model, data, options, "lloqHalf", BLQOptions)
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
  objective <- taskObjective(task)
  data <- PIObservations$new(task$outputMappings)
  currVals <- currStartValues(task)
  cost <- objective$evaluate(currVals)
  resampled <- objective$withData(data$resample(1))
  expect_identical(resampled$observedData, data$resample(1)$observedData)
  expect_false(identical(resampled$evaluate(currVals), cost))
  expect_identical(objective$evaluate(currVals), cost)
})
