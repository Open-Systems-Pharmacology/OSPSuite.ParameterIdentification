# PIOutputMapping

test_that("PIOutputMapping object is correctly created", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  expect_s3_class(outputMapping, "PIOutputMapping")
  expect_equal(outputMapping$observedDataSets, list())
  expect_length(outputMapping$dataTransformations, 4)
  expect_equal(outputMapping$quantity, testQuantity)
  expect_equal(outputMapping$scaling, "lin")
  expect_equal(outputMapping$transformResultsFunction, NULL)
})

test_that("PIOutputMapping read-only fields cannot be set", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  expect_error(outputMapping$observedDataSets <- list(), "is readonly")
  expect_error(outputMapping$dataTransformations <- list(), "is readonly")
  expect_error(outputMapping$quantity <- NULL, "is readonly")
  expect_error(outputMapping$simId <- NULL, "is readonly")
})

test_that("PIOutputMapping adds and removes observed data sets correctly", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  observedData <- testObservedData()

  expect_no_error(
    outputMapping$addObservedDataSets(
      observedData$`AciclovirLaskinData.Laskin 1982.Group A`
    )
  )
  expect_equal(
    outputMapping$observedDataSets[["AciclovirLaskinData.Laskin 1982.Group A"]],
    observedData$`AciclovirLaskinData.Laskin 1982.Group A`
  )

  expect_no_error(outputMapping$removeObservedDataSet(names(observedData)))
  expect_equal(length(outputMapping$observedDataSets), 0)
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
      observedData$`AciclovirLaskinData.Laskin 1982.Group A`
    ),
    "Unit conversion failed for quantity .* and observed data .*"
  )
})

test_that("PIOutputMapping errors when mol weight is missing and can't be retrieved", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  observedData <- testObservedData()
  observedData[[1]]$molWeight <- NA_real_

  mockthat::with_mock(
    `.getMolWeightFor` = function(quantity) stop(),
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
