startValues <- function(task) {
  vapply(task$parameters, function(p) p$startValue, numeric(1))
}

testObjective <- function(task) {
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  PIObjective$new(
    outputMappings = priv$.outputMappings,
    objectiveFunctionOptions = task$configuration$objectiveFunctionOptions,
    simulate = function(values) priv$.simulateOutputs(values)
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
  expect_null(priv$.observedData)
})

test_that("evaluate() returns the error cost structure silently when the simulations fail", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  objective <- PIObjective$new(
    outputMappings = priv$.outputMappings,
    objectiveFunctionOptions = task$configuration$objectiveFunctionOptions,
    simulate = function(values) {
      stop(.simulationsFailedError("Aciclovir", 1L, "solver stopped"))
    }
  )
  expect_silent(cost <- objective$evaluate(startValues(task)))
  expect_identical(cost, .createErrorCostStructure())
  expect_identical(priv$.failedEvaluations, 0)
})

test_that("an infinite cost of successful simulations is not counted as a failed evaluation", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  priv$.objective <- list(
    evaluate = function(values) .createErrorCostStructure(),
    observedData = NULL
  )
  cost <- priv$.objectiveFunction(startValues(task))
  expect_identical(priv$.failedEvaluations, 0)
  expect_identical(priv$.lastCostSummary, cost)
})

test_that("observedData of PIObjective is read-only", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  options <- task$configuration$objectiveFunctionOptions
  objective <- PIObjective$new(priv$.outputMappings, options, function(values) {
    NULL
  })
  expect_snapshot(objective$observedData <- NULL, error = TRUE)
})
