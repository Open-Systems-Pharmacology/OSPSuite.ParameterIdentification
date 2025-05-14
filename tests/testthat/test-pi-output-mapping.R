# PIOutputMapping

testQuantity <- ospsuite::getQuantity(
  path = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  container = testSimulation()
)

test_that("PIOutputMapping object is correctly created", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  expect_s3_class(outputMapping, "PIOutputMapping")
  expect_equal(outputMapping$observedDataSets, list())
  expect_length(outputMapping$dataTransformations, 4)
  expect_equal(outputMapping$quantity, testQuantity)
  expect_equal(outputMapping$scaling, "lin")
  expect_equal(outputMapping$transformResultsFunction, NULL)
})

test_that("PIOutputMapping instance prints without errors", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity())
  expect_snapshot(print(outputMapping))
})

test_that("PIOutputMapping read-only fields cannot be set", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  expect_error(outputMapping$observedDataSets <- list(), "is readonly")
  expect_error(outputMapping$dataTransformations <- list(), "is readonly")
  expect_error(outputMapping$dataWeights <- list(), "is readonly")
  expect_error(outputMapping$quantity <- NULL, "is readonly")
  expect_error(outputMapping$simId <- NULL, "is readonly")
})

test_that("PIOutputMapping adds and removes observed data sets correctly", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  observedData <- testObservedData()
  label <- observedData[[1]]$name

  expect_no_error(outputMapping$addObservedDataSets(observedData[[1]]))
  expect_equal(outputMapping$observedDataSets[[label]], observedData[[1]])

  expect_no_error(outputMapping$removeObservedDataSet(label))
  expect_equal(length(outputMapping$observedDataSets), 0)
})

test_that("PIOutputMapping allows scaling to be changed to predefined values", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$scaling <- "log"
  expect_equal(outputMapping$scaling, "log")
  outputMapping$scaling <- "lin"
  expect_equal(outputMapping$scaling, "lin")
  expect_error(outputMapping$scaling <- "invalidScaling", "not in defined")
})

test_that("PIOutputMapping applies global x-offsets to datasets", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$setDataTransformations(xOffsets = -5)
  expect_equal(outputMapping$dataTransformations$xOffsets, -5)
})

test_that("PIOutputMapping sets default values for offsets and factors correctly", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)

  expect_equal(outputMapping$dataTransformations$xFactors, 1)
  expect_equal(outputMapping$dataTransformations$yFactors, 1)
  expect_equal(outputMapping$dataTransformations$xOffsets, 0)
  expect_equal(outputMapping$dataTransformations$yOffsets, 0)
})

test_that("PIOutputMapping applies global x-factors to datasets", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$setDataTransformations(xFactors = 2)

  expect_equal(outputMapping$dataTransformations$xFactors, 2)
})

test_that("PIOutputMapping sets global x-offsets and x-factors simultaneously", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$setDataTransformations(xFactors = 2, xOffsets = 5)
  expect_equal(outputMapping$dataTransformations$xFactors, 2)
  expect_equal(outputMapping$dataTransformations$xOffsets, 5)
})

test_that("PIOutputMapping sets x-factors in a dataset-specific manner with labels", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$setDataTransformations(
    labels = c("data1", "data2"), xFactors = c(2, 3)
  )

  expect_equal(outputMapping$dataTransformations$xFactors[["data1"]], 2)
  expect_equal(outputMapping$dataTransformations$xFactors[["data2"]], 3)
  expect_equal(outputMapping$dataTransformations$yFactors[["data1"]], 1)
  expect_equal(outputMapping$dataTransformations$yFactors[["data2"]], 1)
  expect_equal(outputMapping$dataTransformations$xOffsets[["data1"]], 0)
  expect_equal(outputMapping$dataTransformations$xOffsets[["data2"]], 0)
  expect_equal(outputMapping$dataTransformations$yOffsets[["data1"]], 0)
  expect_equal(outputMapping$dataTransformations$yOffsets[["data2"]], 0)
})

test_that("PIOutputMapping errors when non-function passed to transformResultsFunction", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  expect_error(outputMapping$transformResultsFunction("invalid"), "non-function")
})

test_that("PIOutputMapping applies transformResultsFunction correctly", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$transformResultsFunction <- function(x) x * 2
  expect_equal(outputMapping$transformResultsFunction(5), 10)
})

test_that("PIOutputMapping cannot set weights when no observed data is present", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  weights <- list(c(1, 2, 3))
  expect_error(
    outputMapping$setDataWeights(weights), messages$errorNoObservedDataSets()
  )
})

test_that("PIOutputMapping cannot set weights with invalid input", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$addObservedDataSets(testObservedData())

  weights <- c(1, 2, 3)
  expect_error(
    outputMapping$setDataWeights(weights),
    "argument 'weights' is of type 'numeric', but expected 'list'"
  )
})

test_that("PIOutputMapping cannot set weights with invalid dataset label", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$addObservedDataSets(testObservedData())

  weights <- list(invalidLabel = c(1, 2))
  expect_error(
    outputMapping$setDataWeights(weights), messages$errorWeightsNames()
  )

  weights <- list(c(1, 2), c(2, 3))
  names(weights) <- c(outputMapping$observedDataSets[[1]]$name, "invalidLabel")
  expect_error(
    outputMapping$setDataWeights(weights), messages$errorWeightsNames()
  )
})

test_that("PIOutputMapping cannot set weights when lengths do not match y-values", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$addObservedDataSets(testObservedData())

  label <- outputMapping$observedDataSets[[1]]$name
  weights <- list(c(1, 2))
  names(weights) <- label

  yLen <- length(outputMapping$observedDataSets[[1]]$yValues)
  expect_error(
    outputMapping$setDataWeights(weights),
    messages$errorWeightsVectorLengthMismatch(label, yLen, length(weights[[1]]))
  )
})

test_that("PIOutputMapping can set scalar weights correctly", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$addObservedDataSets(testObservedData())

  label <- outputMapping$observedDataSets[[1]]$name
  weights <- list(2)
  names(weights) <- label

  expect_no_error(outputMapping$setDataWeights(weights))
  expect_equal(
    outputMapping$dataWeights[[label]],
    rep(2, length(outputMapping$observedDataSets[[1]]$yValues))
  )
})

test_that("PIOutputMapping can set weight vectors correctly", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$addObservedDataSets(testObservedData())

  label <- outputMapping$observedDataSets[[1]]$name
  weights <- list(c(rep(2, 5), rep(1.5, 6)))
  names(weights) <- label

  expect_no_error(outputMapping$setDataWeights(weights))
  expect_equal(outputMapping$dataWeights[[label]], weights[[1]])
})

test_that("PIOutputMapping fails with unknown label in weights for multiple datasets", {
  dataSet1 <- DataSet$new(name = "dataSet1")
  dataSet1$setValues(xValues = c(1, 2, 3, 4), yValues = c(10, 20, 30, 40))

  dataSet2 <- DataSet$new(name = "dataSet2")
  dataSet2$setValues(xValues = c(1, 2, 3), yValues = c(20, 30, 40))

  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$addObservedDataSets(dataSet1)
  outputMapping$addObservedDataSets(dataSet2)

  weights <- list(
    dataSet1 = rep(1.5, 4),
    dataSet3 = rep(2, 3) # incorrect label
  )

  expect_error(
    outputMapping$setDataWeights(weights), messages$errorWeightsNames()
  )
})

test_that("PIOutputMapping fails with wrong weight length for one of multiple datasets", {
  dataSet1 <- DataSet$new(name = "dataSet1")
  dataSet1$setValues(xValues = c(1, 2, 3, 4), yValues = c(10, 20, 30, 40))

  dataSet2 <- DataSet$new(name = "dataSet2")
  dataSet2$setValues(xValues = c(1, 2, 3), yValues = c(20, 30, 40))

  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$addObservedDataSets(dataSet1)
  outputMapping$addObservedDataSets(dataSet2)

  weights <- list(
    dataSet1 = rep(1.5, 4),
    dataSet2 = rep(2, 4) # incorrect length
  )

  expect_error(
    outputMapping$setDataWeights(weights),
    messages$errorWeightsVectorLengthMismatch("dataSet2", 3, 4)
  )
})

test_that("PIOutputMapping warns if data weights exist and new datasets are added", {
  dataSet1 <- DataSet$new(name = "dataSet1")
  dataSet1$setValues(xValues = c(1, 2, 3, 4), yValues = c(10, 20, 30, 40))

  dataSet2 <- DataSet$new(name = "dataSet2")
  dataSet2$setValues(xValues = c(1, 2, 3), yValues = c(20, 30, 40))

  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$addObservedDataSets(dataSet1)

  weights <- list(dataSet1 = rep(1, 4))
  outputMapping$setDataWeights(weights)

  expect_warning(
    outputMapping$addObservedDataSets(dataSet2),
    messages$warningDataWeightsPresent()
  )
})

test_that("PIOutputMapping can set weight vectors correctly through `addObservedDataSets`", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  label <- testObservedData()[[1]]$name
  weights <- list(c(rep(2, 5), rep(1.5, 6)))
  names(weights) <- label

  expect_no_error(
    outputMapping$addObservedDataSets(testObservedData(), weights = weights)
  )
  expect_equal(outputMapping$dataWeights[[label]], weights[[1]])
})

test_that("PIOutputMapping adds data without molecular weight and retrieves it", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  observedData <- testObservedData()
  observedData[[1]]$molWeight <- NA_real_

  expect_no_error(
    outputMapping$addObservedDataSets(
      observedData$`AciclovirLaskinData.Laskin 1982.Group A`
    )
  )
  expect_equal(
    outputMapping$observedDataSets[[1]]$molWeight,
    225.21
  )
})

test_that("PIOutputMapping throws error when unit conversion fails", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  observedData <- testObservedData()

  observedData[[1]]$yDimension <- "Amount"
  observedData[[1]]$yUnit <- "mol"

  expect_error(
    outputMapping$addObservedDataSets(
      observedData[[1]]
    ),
    "Unit conversion failed for quantity .* and observed data .*"
  )
})

test_that("PIOutputMapping errors when mol weight is missing and can't be retrieved", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  observedData <- testObservedData()
  observedData[[1]]$molWeight <- NA_real_

  mockthat::with_mock(
    `ospsuite::getMolWeightFor` = function(quantity) stop(),
    {
      expect_error(
        outputMapping$addObservedDataSets(
          observedData$`AciclovirLaskinData.Laskin 1982.Group A`
        ),
        "Unit conversion failed for quantity .* and observed data .*"
      )
    }
  )
})
