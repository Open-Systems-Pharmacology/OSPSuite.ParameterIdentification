test_that(".validateModelCost() accepts the objects of .newModelCost()", {
  cost <- .newModelCost(
    modelCost = 2,
    minLogProbability = 1,
    nObservations = 3,
    weightedSSR = 2
  )
  expect_identical(.validateModelCost(cost), cost)
  expect_invisible(.validateModelCost(.createErrorCostStructure()))
})

test_that(".validateModelCost() names what is wrong", {
  cost <- .newModelCost(
    modelCost = 2,
    minLogProbability = 1,
    nObservations = 3,
    weightedSSR = 2
  )
  expect_snapshot(.validateModelCost(list(modelCost = 2)), error = TRUE)
  expect_snapshot(.validateModelCost(unclass(cost)), error = TRUE)
  noResiduals <- cost
  noResiduals$residualDetails <- NULL
  expect_snapshot(.validateModelCost(noResiduals), error = TRUE)
  fewerColumns <- cost
  fewerColumns$costVariables$rawSSR <- NULL
  expect_snapshot(.validateModelCost(fewerColumns), error = TRUE)
  vectorCost <- cost
  vectorCost$modelCost <- c(1, 2)
  expect_snapshot(.validateModelCost(vectorCost), error = TRUE)
})

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
  expect_invisible(.validateModelCost(cost))
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

test_that("PIObjective validates its inputs", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  options <- task$configuration$objectiveFunctionOptions
  expect_snapshot(
    PIObjective$new(list(1), options, function(values) NULL),
    error = TRUE
  )
  expect_snapshot(
    PIObjective$new(priv$.outputMappings, options, simulate = NULL),
    error = TRUE
  )
  objective <- PIObjective$new(priv$.outputMappings, options, function(values) NULL)
  expect_snapshot(objective$observedData <- NULL, error = TRUE)
})
