laskinName <- "AciclovirLaskinData.Laskin 1982.Group A"

# Two mappings with five individual data sets and one aggregated data set each
mixedMappings <- function() {
  lapply(1:2, function(i) {
    mapping <- PIOutputMapping$new(quantity = testQuantity())
    mapping$addObservedDataSets(syntheticObservedData())
    mapping$addObservedDataSets(testObservedData())
    mapping$setSubjectCount(13)
    mapping
  })
}

test_that("PIObservations reads the observed data of the output mappings once", {
  mappings <- testPiTask()$outputMappings
  reads <- localObservedDataReads()
  data <- PIObservations$new(mappings)
  expect_equal(reads$reads, length(mappings))
  expect_identical(data$outputMappings, mappings)
  expect_identical(data$observedData, lapply(mappings, .prepareObservedData))
  censored <- data$observedData[[1]]$censored
  expect_identical(censored, rep(FALSE, length(data$observedData[[1]]$yValues)))
})

test_that("resample() is a function of the data and the seed", {
  mappings <- mixedMappings()
  data <- PIObservations$new(mappings)
  individualName <- names(syntheticObservedData())[[1]]
  summary <- function(resampled) {
    lapply(resampled$outputMappings, function(mapping) {
      list(
        weights = mapping$dataWeights,
        means = mapping$observedDataSets[[laskinName]]$yValues
      )
    })
  }
  first <- summary(data$resample(4))
  # The output mappings are drawn independently of each other
  expect_false(identical(first[[1]]$weights, first[[2]]$weights))
  expect_false(identical(first[[1]]$means, first[[2]]$means))
  expect_identical(summary(data$resample(4)), first)
  expect_identical(data$resample(4L)$observedData, data$resample(4)$observedData)
  expect_false(identical(data$resample(5)$observedData, data$resample(4)$observedData))
  # Individual data keep their values, only their weights move
  expect_identical(
    data$resample(4)$outputMappings[[1]]$observedDataSets[[individualName]]$yValues,
    mappings[[1]]$observedDataSets[[individualName]]$yValues
  )
})

test_that("resample() leaves the data, the DataSet objects and the random state unchanged", {
  mappings <- mixedMappings()
  mappings[[1]]$setDataWeights(stats::setNames(list(20), laskinName))
  dataSet <- mappings[[1]]$observedDataSets[[laskinName]]
  yBefore <- dataSet$yValues
  weightsBefore <- mappings[[1]]$dataWeights
  data <- PIObservations$new(mappings)
  observedBefore <- data$observedData
  set.seed(1)
  seedBefore <- .Random.seed

  expect_no_warning(resampled <- data$resample(7))

  expect_identical(.Random.seed, seedBefore)
  expect_identical(dataSet$yValues, yBefore)
  expect_identical(mappings[[1]]$dataWeights, weightsBefore)
  expect_identical(data$observedData, observedBefore)
  expect_identical(data$outputMappings, mappings)
  expect_false(identical(resampled$observedData[[1]]$yValues, observedBefore[[1]]$yValues))
  expect_setequal(
    names(resampled$outputMappings[[1]]$dataWeights),
    names(mappings[[1]]$observedDataSets)
  )
})

test_that("resample() redraws means with errors converted to the unit of the means", {
  aggregatedMapping <- function(yErrorValues, yErrorUnit) {
    dataSet <- ospsuite::DataSet$new(name = "aggregated")
    dataSet$setValues(
      xValues = c(1, 2),
      yValues = c(10, 20),
      yErrorValues = yErrorValues
    )
    dataSet$yDimension <- ospsuite::ospDimensions$`Concentration (mass)`
    dataSet$yUnit <- "mg/l"
    dataSet$yErrorType <- "ArithmeticStdDev"
    dataSet$yErrorUnit <- yErrorUnit
    mapping <- PIOutputMapping$new(quantity = testQuantity())
    mapping$addObservedDataSets(dataSet)
    mapping$setSubjectCount(13)
    list(mapping)
  }
  redrawnMeans <- function(mappings) {
    resampled <- PIObservations$new(mappings)$resample(7)
    resampled$outputMappings[[1]]$observedDataSets$aggregated$yValues
  }
  expect_equal(
    redrawnMeans(aggregatedMapping(c(2000, 4000), "µg/l")),
    redrawnMeans(aggregatedMapping(c(2, 4), "mg/l"))
  )
})

test_that("resample() draws only the weights of a mapping without a subject count", {
  mapping <- PIOutputMapping$new(quantity = testQuantity())
  mapping$addObservedDataSets(syntheticObservedData())
  data <- PIObservations$new(list(mapping))
  resampled <- data$resample(2)
  expect_identical(resampled$outputMappings[[1]]$observedDataSets, mapping$observedDataSets)
  expect_identical(resampled$observedData, data$observedData)
  expect_false(identical(resampled$outputMappings[[1]]$dataWeights, .completeDataWeights(mapping)))
})

test_that("resample() redraws each data set with its own subject count", {
  redrawnLogDeviations <- function(subjectCounts) {
    mapping <- aggregatedStudiesMapping()
    mapping$setSubjectCount(subjectCounts)
    resampled <- PIObservations$new(list(mapping))$resample(3)
    lapply(resampled$outputMappings[[1]]$observedDataSets, function(dataSet) {
      log(dataSet$yValues / c(10, 20))
    })
  }
  sameCount <- redrawnLogDeviations(c(studyA = 12, studyB = 12))
  fourfoldCount <- redrawnLogDeviations(c(studyA = 12, studyB = 48))
  # DataSet stores values in single precision
  expect_equal(fourfoldCount$studyA, sameCount$studyA, tolerance = 1e-6)
  expect_equal(fourfoldCount$studyB, sameCount$studyB / 2, tolerance = 1e-6)
})

test_that("resample() redraws only the data sets with a subject count and keeps the counts", {
  mapping <- aggregatedStudiesMapping()
  mapping$setSubjectCount(c(studyA = 12))
  resampled <- PIObservations$new(list(mapping))$resample(3)$outputMappings[[1]]
  original <- mapping$observedDataSets
  expect_false(identical(resampled$observedDataSets$studyA$yValues, original$studyA$yValues))
  expect_identical(resampled$observedDataSets$studyB$yValues, original$studyB$yValues)
  expect_identical(resampled$subjectCount, c(studyA = 12L))
})

test_that("resample() keeps the weights of aggregated data sets in every replicate", {
  mapping <- aggregatedStudiesMapping()
  mapping$setSubjectCount(c(studyA = 8, studyB = 20))
  data <- PIObservations$new(list(mapping))
  for (seed in 1:20) {
    resampled <- data$resample(seed)
    expect_identical(resampled$outputMappings[[1]]$dataWeights, .completeDataWeights(mapping))
  }
})

test_that("resample() draws only the weights of individual data sets when data are mixed", {
  mapping <- mixedMappings()[[1]]
  data <- PIObservations$new(list(mapping))
  individualWeights <- list()
  for (seed in 1:20) {
    resampled <- data$resample(seed)$outputMappings[[1]]
    expect_identical(
      resampled$dataWeights[[laskinName]],
      .completeDataWeights(mapping)[[laskinName]]
    )
    individualWeights[[seed]] <- resampled$dataWeights[
      setdiff(names(resampled$dataWeights), laskinName)
    ]
  }
  expect_gt(length(unique(individualWeights)), 1)
})

test_that(".copyDataSet() prepares like the original and takes new y values", {
  aggregated <- ospsuite::DataSet$new(name = "aggregated")
  aggregated$setValues(xValues = c(1, 2), yValues = c(10, 20), yErrorValues = c(2000, 4000))
  aggregated$yDimension <- ospsuite::ospDimensions$`Concentration (mass)`
  aggregated$yUnit <- "mg/l"
  aggregated$yErrorType <- "ArithmeticStdDev"
  aggregated$yErrorUnit <- "µg/l"
  aggregated$LLOQ <- 0.5
  aggregated$molWeight <- 225.2
  for (dataSet in list(testObservedData()[[1]], aggregated)) {
    mapping <- PIOutputMapping$new(quantity = testQuantity())
    mapping$addObservedDataSets(dataSet)
    copied <- mapping$clone()
    copied$addObservedDataSets(.copyDataSet(dataSet))
    expect_identical(.prepareObservedData(copied), .prepareObservedData(mapping))
  }
  yBefore <- aggregated$yValues
  doubled <- .copyDataSet(aggregated, yValues = 2 * yBefore)
  expect_equal(doubled$yValues, 2 * yBefore)
  expect_equal(doubled$LLOQ, 0.5)
  expect_identical(aggregated$yValues, yBefore)
})

test_that(".completeDataWeights() fills the data sets without weights with 1", {
  mapping <- PIOutputMapping$new(quantity = testQuantity())
  mapping$addObservedDataSets(syntheticObservedData())
  names <- names(mapping$observedDataSets)
  mapping$setDataWeights(stats::setNames(list(2), names[[2]]))
  weights <- .completeDataWeights(mapping)
  expect_identical(names(weights), names)
  expect_identical(weights[[2]], rep(2, length(mapping$observedDataSets[[2]]$yValues)))
  expect_identical(weights[[1]], rep(1, length(mapping$observedDataSets[[1]]$yValues)))
})

test_that("PIObservations validates its input and is read-only", {
  expect_error(
    PIObservations$new(list(1)),
    messages$errorWrongType("outputMappings", "list", "PIOutputMapping"),
    fixed = TRUE
  )
  data <- PIObservations$new(testPiTask()$outputMappings)
  expect_snapshot(data$observedData <- NULL, error = TRUE)
  expect_snapshot(data$outputMappings <- NULL, error = TRUE)
})
