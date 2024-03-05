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

test_that("Read-only fields cannot be set", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  expect_error(outputMapping$observedDataSets <- list(), "is readonly")
  expect_error(outputMapping$dataTransformations <- list(), "is readonly")
  expect_error(outputMapping$quantity <- NULL, "is readonly")
})

test_that("Observed data sets can be added", {
  filePath <- getTestDataFilePath("AciclovirLaskinData.xlsx")
  dataConfiguration <- ospsuite::createImporterConfigurationForFile(filePath = filePath)
  dataConfiguration$sheets <- "Laskin 1982.Group A"
  dataConfiguration$namingPattern <- "{Source}.{Sheet}"
  observedData <- loadDataSetsFromExcel(
    xlsFilePath = filePath,
    importerConfigurationOrPath = dataConfiguration
  )

  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  expect_no_error(
    outputMapping$addObservedDataSets(
      observedData$`AciclovirLaskinData.Laskin 1982.Group A`
    )
  )

  expect_equal(
    outputMapping$observedDataSets[["AciclovirLaskinData.Laskin 1982.Group A"]],
    observedData$`AciclovirLaskinData.Laskin 1982.Group A`
  )
})

test_that("Scaling can be changed to predefined values", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$scaling <- "log"
  expect_equal(outputMapping$scaling, "log")
  outputMapping$scaling <- "lin"
  expect_equal(outputMapping$scaling, "lin")
  expect_error(outputMapping$scaling <- "invalidScaling", "not in defined")
})

test_that("Global x-offsets are correctly applied to all datasets", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$setDataTransformations(xOffsets = -5)
  expect_equal(outputMapping$dataTransformations$xOffsets, -5)
})

test_that("Default values for offsets and factors are correctly set", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  expect_equal(outputMapping$dataTransformations$xFactors, 1)
  expect_equal(outputMapping$dataTransformations$yFactors, 1)
  expect_equal(outputMapping$dataTransformations$xOffsets, 0)
  expect_equal(outputMapping$dataTransformations$yOffsets, 0)
})

test_that("Global x-factors are correctly applied to all datasets", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$setDataTransformations(xFactors = 2)
  expect_equal(outputMapping$dataTransformations$xFactors, 2)
})

test_that("Global x-offsets and x-factors can be set simultaneously", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$setDataTransformations(xFactors = 2, xOffsets = 5)
  expect_equal(outputMapping$dataTransformations$xFactors, 2)
  expect_equal(outputMapping$dataTransformations$xOffsets, 5)
})

test_that("X-factors can be set in a dataset-specific manner, with labels", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$setDataTransformations(labels = c("data1", "data2"), xFactors = c(2, 3))
  expect_equal(outputMapping$dataTransformations$xFactors[["data1"]], 2)
  expect_equal(outputMapping$dataTransformations$xFactors[["data2"]], 3)
  expect_equal(outputMapping$dataTransformations$yFactors[["data1"]], 1)
  expect_equal(outputMapping$dataTransformations$yFactors[["data2"]], 1)
  expect_equal(outputMapping$dataTransformations$xOffsets[["data1"]], 0)
  expect_equal(outputMapping$dataTransformations$xOffsets[["data2"]], 0)
  expect_equal(outputMapping$dataTransformations$yOffsets[["data1"]], 0)
  expect_equal(outputMapping$dataTransformations$yOffsets[["data2"]], 0)
})

test_that("Error is thrown when a non-function is passed to transformResultsFunction", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  expect_error(outputMapping$transformResultsFunction("invalid"), "non-function")
})

test_that("transformResultsFunction is correctly applied", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$transformResultsFunction <- function(x) x * 2
  expect_equal(outputMapping$transformResultsFunction(5), 10)
})
