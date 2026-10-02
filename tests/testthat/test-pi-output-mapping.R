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
  expect_error(
    outputMapping$scaling <- "invalidScaling",
    "is not a valid value"
  )
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

# An output mapping with the data sets `dataSet1` and `dataSet2`, in this order
twoDataSetsMapping <- function() {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$addObservedDataSets(testObservedDataMultiple())
  outputMapping
}

test_that("PIOutputMapping applies values without labels by position, also named ones (#311)", {
  outputMapping <- twoDataSetsMapping()
  # A named value, as taken from a named vector, applies to all data sets
  parameters <- c(shift = 0.5, scale = 2)
  outputMapping$setDataTransformations(
    xOffsets = parameters["shift"],
    yFactors = parameters["scale"]
  )
  expect_equal(
    outputMapping$dataTransformations,
    list(xOffsets = 0.5, yOffsets = 0, xFactors = 1, yFactors = 2)
  )
  expect_equal(
    .transformationsByDataSet(
      outputMapping$dataTransformations,
      c("dataSet1", "dataSet2")
    ),
    list(
      xOffsets = c(dataSet1 = 0.5, dataSet2 = 0.5),
      yOffsets = c(dataSet1 = 0, dataSet2 = 0),
      xFactors = c(dataSet1 = 1, dataSet2 = 1),
      yFactors = c(dataSet1 = 2, dataSet2 = 2)
    )
  )

  # One value per data set applies in the order of the data sets, whatever
  # the names of the values
  outputMapping$setDataTransformations(
    xOffsets = c(0, 0.2),
    yFactors = c(second = 3, first = 4)
  )
  expect_equal(
    .transformationsByDataSet(
      outputMapping$dataTransformations,
      c("dataSet1", "dataSet2")
    ),
    list(
      xOffsets = c(dataSet1 = 0, dataSet2 = 0.2),
      yOffsets = c(dataSet1 = 0, dataSet2 = 0),
      xFactors = c(dataSet1 = 1, dataSet2 = 1),
      yFactors = c(dataSet1 = 3, dataSet2 = 4)
    )
  )

  # Labels then keep the values of the other data sets
  outputMapping$setDataTransformations(labels = "dataSet1", yOffsets = 1)
  expect_equal(
    outputMapping$dataTransformations,
    list(
      xOffsets = c(dataSet1 = 0, dataSet2 = 0.2),
      yOffsets = c(dataSet1 = 1, dataSet2 = 0),
      xFactors = c(dataSet1 = 1, dataSet2 = 1),
      yFactors = c(dataSet1 = 1, dataSet2 = 4)
    )
  )
})

test_that("PIOutputMapping sets the transformations of labeled data sets (#311)", {
  # Labels for some data sets: the others keep the default transformations
  outputMapping <- twoDataSetsMapping()
  outputMapping$setDataTransformations(
    labels = "dataSet2",
    xOffsets = 0.2,
    yFactors = 0.8
  )
  expect_equal(
    outputMapping$dataTransformations,
    list(
      xOffsets = c(dataSet1 = 0, dataSet2 = 0.2),
      yOffsets = c(dataSet1 = 0, dataSet2 = 0),
      xFactors = c(dataSet1 = 1, dataSet2 = 1),
      yFactors = c(dataSet1 = 1, dataSet2 = 0.8)
    )
  )

  # Labels for all data sets, in another order than the data sets
  outputMapping$setDataTransformations(
    labels = c("dataSet2", "dataSet1"),
    xFactors = c(3, 2),
    yOffsets = 0.5
  )
  expect_equal(
    outputMapping$dataTransformations,
    list(
      xOffsets = c(dataSet1 = 0, dataSet2 = 0),
      yOffsets = c(dataSet1 = 0.5, dataSet2 = 0.5),
      xFactors = c(dataSet1 = 2, dataSet2 = 3),
      yFactors = c(dataSet1 = 1, dataSet2 = 1)
    )
  )
})

test_that("PIOutputMapping keeps the transformations of data sets without a label (#311)", {
  outputMapping <- twoDataSetsMapping()
  outputMapping$setDataTransformations(xOffsets = 0.5, yFactors = 2)
  outputMapping$setDataTransformations(labels = "dataSet2", yOffsets = 1)
  expect_equal(
    outputMapping$dataTransformations,
    list(
      xOffsets = c(dataSet1 = 0.5, dataSet2 = 0),
      yOffsets = c(dataSet1 = 0, dataSet2 = 1),
      xFactors = c(dataSet1 = 1, dataSet2 = 1),
      yFactors = c(dataSet1 = 2, dataSet2 = 1)
    )
  )

  # A call without labels, also with `labels = NULL`, sets the
  # transformations of all data sets
  outputMapping$setDataTransformations(xOffsets = 0.3)
  expect_equal(
    outputMapping$dataTransformations,
    list(xOffsets = 0.3, yOffsets = 0, xFactors = 1, yFactors = 1)
  )
  outputMapping$setDataTransformations(labels = "dataSet2", yOffsets = 1)
  outputMapping$setDataTransformations(labels = NULL, yFactors = 4)
  expect_equal(
    outputMapping$dataTransformations,
    list(xOffsets = 0, yOffsets = 0, xFactors = 1, yFactors = 4)
  )
})

test_that("PIOutputMapping keeps the transformations by data set when data sets are added or removed (#311)", {
  outputMapping <- twoDataSetsMapping()
  outputMapping$setDataTransformations(xOffsets = 0.5)
  outputMapping$setDataTransformations(labels = "dataSet2", xOffsets = 0.2)

  # A new data set gets the transformations of the last call without labels,
  # and a data set that replaces one with its name keeps its transformations
  dataSet3 <- testObservedDataMultiple()$dataSet2
  dataSet3$name <- "dataSet3"
  outputMapping$addObservedDataSets(
    list(dataSet3, testObservedDataMultiple()$dataSet2)
  )
  expect_equal(
    outputMapping$dataTransformations$xOffsets,
    c(dataSet1 = 0.5, dataSet2 = 0.2, dataSet3 = 0.5)
  )

  outputMapping$removeObservedDataSet("dataSet2")
  expect_equal(
    outputMapping$dataTransformations,
    list(
      xOffsets = c(dataSet1 = 0.5, dataSet3 = 0.5),
      yOffsets = c(dataSet1 = 0, dataSet3 = 0),
      xFactors = c(dataSet1 = 1, dataSet3 = 1),
      yFactors = c(dataSet1 = 1, dataSet3 = 1)
    )
  )
  expect_identical(
    names(outputMapping$observedDataSets),
    c("dataSet1", "dataSet3")
  )
})

test_that("PIOutputMapping stops for labels that are not data sets of the mapping (#311)", {
  outputMapping <- twoDataSetsMapping()
  expect_error(
    outputMapping$setDataTransformations(
      labels = c("dataSet1", "dataSet3"),
      xOffsets = 1
    ),
    messages$errorTransformationLabels("dataSet3", c("dataSet1", "dataSet2")),
    fixed = TRUE
  )
  expect_equal(
    outputMapping$dataTransformations,
    list(xOffsets = 0, yOffsets = 0, xFactors = 1, yFactors = 1)
  )

  # Before the data sets are added
  expect_error(
    PIOutputMapping$new(quantity = testQuantity)$setDataTransformations(
      labels = "dataSet1",
      xOffsets = 1
    ),
    messages$errorTransformationLabels("dataSet1", character()),
    fixed = TRUE
  )

  # One value per label, or one value for all labels
  expect_error(
    outputMapping$setDataTransformations(
      labels = c("dataSet1", "dataSet2"),
      yFactors = c(1, 2, 3)
    ),
    messages$errorTransformationValues("yFactors", 3, 2),
    fixed = TRUE
  )

  # Each label once
  expect_error(
    outputMapping$setDataTransformations(
      labels = c("dataSet1", "dataSet1"),
      xOffsets = c(1, 2)
    ),
    messages$errorTransformationDuplicateLabels("dataSet1"),
    fixed = TRUE
  )

  # Values of an earlier call without labels that do not match the data sets
  outputMapping$setDataTransformations(yFactors = c(2, 3, 4))
  expect_error(
    outputMapping$setDataTransformations(labels = "dataSet1", xOffsets = 1),
    messages$errorTransformationValuesPerDataSet("yFactors", 3, 2),
    fixed = TRUE
  )
  expect_equal(outputMapping$dataTransformations$yFactors, c(2, 3, 4))
})

test_that("PIOutputMapping changes nothing for an empty set of labels (#311)", {
  # As for labels that are computed, for example with `intersect()`
  for (outputMapping in list(
    PIOutputMapping$new(quantity = testQuantity),
    twoDataSetsMapping()
  )) {
    outputMapping$setDataTransformations(xOffsets = 0.5)
    outputMapping$setDataTransformations(labels = character(), xOffsets = 1)
    expect_equal(
      outputMapping$dataTransformations,
      list(xOffsets = 0.5, yOffsets = 0, xFactors = 1, yFactors = 1)
    )
  }
})

test_that("PIOutputMapping errors when non-function passed to transformResultsFunction", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  expect_error(
    outputMapping$transformResultsFunction("invalid"),
    "non-function"
  )
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
    outputMapping$setDataWeights(weights),
    messages$errorNoObservedDataSets()
  )
})

test_that("PIOutputMapping cannot set weights with invalid input", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$addObservedDataSets(testObservedData())

  weights <- c(1, 2, 3)
  expect_error(
    outputMapping$setDataWeights(weights),
    messages$errorWrongType("weights", "numeric", "list"),
    fixed = TRUE
  )
})

test_that("PIOutputMapping cannot set weights with invalid dataset label", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$addObservedDataSets(testObservedData())

  weights <- list(invalidLabel = c(1, 2))
  expect_error(
    outputMapping$setDataWeights(weights),
    messages$errorWeightsNames()
  )

  weights <- list(c(1, 2), c(2, 3))
  names(weights) <- c(outputMapping$observedDataSets[[1]]$name, "invalidLabel")
  expect_error(
    outputMapping$setDataWeights(weights),
    messages$errorWeightsNames()
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
    outputMapping$setDataWeights(weights),
    messages$errorWeightsNames()
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
