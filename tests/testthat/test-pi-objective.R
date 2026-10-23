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
