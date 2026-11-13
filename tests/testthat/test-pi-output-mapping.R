# PIOutputMapping

testQuantity <- ospsuite::getQuantity(
  path = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  container = testSimulation()
)

test_that("PIOutputMapping object is correctly created", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  expect_s3_class(outputMapping, "PIOutputMapping")
  expect_equal(outputMapping$observedDataSets, list())
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

twoStudyMapping <- function() {
  studyA <- DataSet$new(name = "studyA")
  studyA$setValues(xValues = c(1, 2, 3, 4), yValues = c(10, 20, 30, 40))
  studyB <- DataSet$new(name = "studyB")
  studyB$setValues(xValues = c(1, 2, 3), yValues = c(20, 30, 40))
  mapping <- PIOutputMapping$new(quantity = testQuantity)
  mapping$addObservedDataSets(list(studyA, studyB))
  mapping
}

test_that("dataTransformations gives no transformation to every data set", {
  expect_identical(
    twoStudyMapping()$dataTransformations,
    list(
      xOffsets = c(studyA = 0, studyB = 0),
      yOffsets = c(studyA = 0, studyB = 0),
      xFactors = c(studyA = 1, studyB = 1),
      yFactors = c(studyA = 1, studyB = 1)
    )
  )
})

test_that("dataTransformations is empty without observed data sets", {
  transformations <- PIOutputMapping$new(
    quantity = testQuantity
  )$dataTransformations
  expect_named(
    transformations,
    c("xOffsets", "yOffsets", "xFactors", "yFactors")
  )
  expect_true(all(lengths(transformations) == 0))
})

test_that("setDataTransformations() applies one unnamed value to every data set", {
  mapping <- twoStudyMapping()
  mapping$setDataTransformations(xOffsets = -5, yFactors = 0.8)
  expect_identical(
    mapping$dataTransformations$xOffsets,
    c(studyA = -5, studyB = -5)
  )
  expect_identical(
    mapping$dataTransformations$yFactors,
    c(studyA = 0.8, studyB = 0.8)
  )
  expect_identical(
    mapping$dataTransformations$xFactors,
    c(studyA = 1, studyB = 1)
  )
})

test_that("setDataTransformations() sets named data sets and keeps the others", {
  mapping <- twoStudyMapping()
  mapping$setDataTransformations(xOffsets = 0.5)
  mapping$setDataTransformations(
    xOffsets = c(studyB = 0.2),
    yFactors = c(studyB = 0.8)
  )
  expect_identical(
    mapping$dataTransformations$xOffsets,
    c(studyA = 0.5, studyB = 0.2)
  )
  expect_identical(
    mapping$dataTransformations$yFactors,
    c(studyA = 1, studyB = 0.8)
  )
})

test_that("setDataTransformations() orders the values as the data sets", {
  mapping <- twoStudyMapping()
  mapping$setDataTransformations(yOffsets = c(studyB = 2, studyA = 1))
  expect_identical(
    mapping$dataTransformations$yOffsets,
    c(studyA = 1, studyB = 2)
  )
})

test_that("setDataTransformations() keeps the arguments it is not given", {
  mapping <- twoStudyMapping()
  mapping$setDataTransformations(xFactors = 2)
  mapping$setDataTransformations(yOffsets = 3)
  expect_identical(
    mapping$dataTransformations$xFactors,
    c(studyA = 2, studyB = 2)
  )
})

test_that("setDataTransformations() rejects invalid values and changes nothing", {
  expect_snapshot(
    PIOutputMapping$new(quantity = testQuantity)$setDataTransformations(
      xOffsets = 1
    ),
    error = TRUE
  )
  mapping <- twoStudyMapping()
  expect_snapshot(
    mapping$setDataTransformations(xOffsets = c(studyC = 1)),
    error = TRUE
  )
  expect_snapshot(
    mapping$setDataTransformations(xOffsets = c(1, 2)),
    error = TRUE
  )
  expect_snapshot(
    mapping$setDataTransformations(yFactors = NA_real_),
    error = TRUE
  )
  expect_snapshot(mapping$setDataTransformations(yFactors = Inf), error = TRUE)
  expect_snapshot(
    mapping$setDataTransformations(yFactors = numeric()),
    error = TRUE
  )
  expect_snapshot(
    mapping$setDataTransformations(xOffsets = 1, yFactors = c(studyC = 1)),
    error = TRUE
  )
  expect_identical(
    mapping$dataTransformations,
    twoStudyMapping()$dataTransformations
  )
})

test_that("replacing a data set keeps its transformations and weights", {
  mapping <- twoStudyMapping()
  mapping$setDataTransformations(xOffsets = c(studyB = 0.2))
  mapping$setDataWeights(list(studyB = 2))
  mapping$addObservedDataSets(mapping$observedDataSets$studyB)
  expect_identical(
    mapping$dataTransformations$xOffsets,
    c(studyA = 0, studyB = 0.2)
  )
  expect_identical(mapping$dataWeights, list(studyB = c(2, 2, 2)))
})

test_that("removing a data set drops its transformations and weights", {
  mapping <- twoStudyMapping()
  studyB <- mapping$observedDataSets$studyB
  mapping$setDataTransformations(xOffsets = 0.2)
  mapping$setDataWeights(list(studyA = 2, studyB = 3))
  mapping$removeObservedDataSet("studyB")
  expect_identical(mapping$dataTransformations$xOffsets, c(studyA = 0.2))
  expect_named(mapping$dataWeights, "studyA")
  expect_snapshot(mapping$addObservedDataSets(studyB))
  expect_identical(
    mapping$dataTransformations$xOffsets,
    c(studyA = 0.2, studyB = 0)
  )
  expect_named(mapping$dataWeights, "studyA")
})

test_that("observedDataSets stops when a data set was renamed after adding it", {
  mapping <- twoStudyMapping()
  dataSet <- mapping$observedDataSets$studyB
  dataSet$name <- "studyB revised"
  expect_snapshot(mapping$observedDataSets, error = TRUE)
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

test_that("setSubjectCount() gives one number to every data set of the mapping", {
  mapping <- PIOutputMapping$new(quantity = testQuantity())
  expect_null(mapping$subjectCount)
  mapping$addObservedDataSets(testObservedData())
  mapping$addObservedDataSets(syntheticObservedData())
  mapping$setSubjectCount(13)
  labels <- names(mapping$observedDataSets)
  expect_identical(
    mapping$subjectCount,
    setNames(rep(13L, length(labels)), labels)
  )
})

test_that("setSubjectCount() sets counts by data set label and keeps the others", {
  laskin <- "AciclovirLaskinData.Laskin 1982.Group A"
  synthetic <- "AciclovirDataIndividuals.Aciclovir.Synthetic.id1"
  mapping <- PIOutputMapping$new(quantity = testQuantity())
  mapping$addObservedDataSets(testObservedData())
  mapping$addObservedDataSets(syntheticObservedData()[[synthetic]])
  mapping$setSubjectCount(12)
  mapping$setSubjectCount(setNames(48, synthetic))
  expect_identical(
    mapping$subjectCount,
    setNames(c(12L, 48L), c(laskin, synthetic))
  )
})

test_that("replacing or removing a data set clears its subject count", {
  synthetic <- "AciclovirDataIndividuals.Aciclovir.Synthetic.id1"
  mapping <- PIOutputMapping$new(quantity = testQuantity())
  mapping$addObservedDataSets(testObservedData())
  mapping$addObservedDataSets(syntheticObservedData()[[synthetic]])
  mapping$setSubjectCount(12)
  mapping$addObservedDataSets(testObservedData())
  expect_identical(mapping$subjectCount, setNames(12L, synthetic))
  mapping$removeObservedDataSet(synthetic)
  expect_length(mapping$subjectCount, 0)
})

test_that("setSubjectCount() rejects invalid counts and the binding is read-only", {
  mapping <- PIOutputMapping$new(quantity = testQuantity())
  expect_snapshot(mapping$setSubjectCount(13), error = TRUE)
  mapping$addObservedDataSets(testObservedData())
  expect_snapshot(mapping$setSubjectCount(0), error = TRUE)
  expect_snapshot(mapping$setSubjectCount(2.5), error = TRUE)
  expect_snapshot(mapping$setSubjectCount(NA), error = TRUE)
  expect_snapshot(mapping$setSubjectCount(numeric()), error = TRUE)
  expect_snapshot(mapping$setSubjectCount(c(3, 4)), error = TRUE)
  expect_snapshot(mapping$setSubjectCount(c(unknownStudy = 12)), error = TRUE)
  expect_snapshot(mapping$setSubjectCount(1e10), error = TRUE)
  expect_snapshot(mapping$subjectCount <- 5, error = TRUE)
})
