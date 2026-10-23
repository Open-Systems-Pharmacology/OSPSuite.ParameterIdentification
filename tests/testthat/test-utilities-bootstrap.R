test_that(".validateSubjectCounts() stops for aggregated data without a count", {
  mapping <- PIOutputMapping$new(quantity = testQuantity())
  mapping$addObservedDataSets(testObservedData())
  expect_snapshot(.validateSubjectCounts(list(mapping)), error = TRUE)
  mapping$setSubjectCount(13)
  expect_invisible(.validateSubjectCounts(list(mapping)))
})

test_that(".validateSubjectCounts() ignores individual data and aggregated data without errors", {
  individual <- PIOutputMapping$new(quantity = testQuantity())
  individual$addObservedDataSets(syntheticObservedData())
  noErrors <- PIOutputMapping$new(quantity = testQuantity())
  dataSet <- testObservedData()[[1]]
  dataSet$setValues(
    dataSet$xValues,
    dataSet$yValues,
    rep(NA_real_, length(dataSet$yValues))
  )
  noErrors$addObservedDataSets(dataSet)
  expect_no_error(.validateSubjectCounts(list(individual, noErrors)))
})

test_that(".withSeed() leaves the global random state as it was", {
  set.seed(1)
  before <- .Random.seed
  .withSeed(99, stats::rnorm(5))
  expect_identical(.Random.seed, before)
  expect_identical(.withSeed(99, stats::rnorm(2)), .withSeed(99, stats::rnorm(2)))
})

test_that(".resampleDataSetValues() redraws geometric means with SD log(GSD) / sqrt(n)", {
  values <- list(
    xValues = 1:3,
    yValues = c(10, 5, 2),
    yErrorValues = c(1.3, 1.5, 1.8),
    yErrorType = "GeometricStdDev"
  )
  draws <- .withSeed(
    1,
    replicate(4000, .resampleDataSetValues(values, subjectCount = 13)$yValues)
  )
  expect_equal(rowMeans(log(draws)), log(values$yValues), tolerance = 0.01)
  expect_equal(
    apply(log(draws), 1, sd),
    log(values$yErrorValues) / sqrt(13),
    tolerance = 0.05
  )
})

test_that(".resampleDataSetValues() redraws arithmetic means with mean mu and SD sigma / sqrt(n), all positive", {
  values <- list(
    xValues = 1:3,
    yValues = c(10, 5, 2),
    yErrorValues = c(3, 3, 2),
    yErrorType = "ArithmeticStdDev"
  )
  draws <- .withSeed(
    1,
    replicate(4000, .resampleDataSetValues(values, subjectCount = 13)$yValues)
  )
  expect_equal(rowMeans(draws), values$yValues, tolerance = 0.01)
  expect_equal(apply(draws, 1, sd), values$yErrorValues / sqrt(13), tolerance = 0.05)
  expect_true(all(draws > 0))
})

test_that(".resampleDataSetValues() keeps points without a usable error and individual data", {
  partial <- list(
    xValues = 1:3,
    yValues = c(10, 0, 2),
    yErrorValues = c(NA, 1, 0.5),
    yErrorType = "ArithmeticStdDev"
  )
  resampled <- .withSeed(1, .resampleDataSetValues(partial, subjectCount = 13))
  expect_identical(resampled$yValues[1:2], c(10, 0))
  expect_false(resampled$yValues[3] == 2)
  individual <- list(xValues = 1:2, yValues = c(1, 2), yErrorValues = NULL, yErrorType = NULL)
  expect_identical(.resampleDataSetValues(individual, subjectCount = 13), individual)
  noErrors <- list(
    xValues = 1:2,
    yValues = c(1, 2),
    yErrorValues = c(NA, NA),
    yErrorType = "ArithmeticStdDev"
  )
  expect_identical(.resampleDataSetValues(noErrors, subjectCount = 13), noErrors)
})

test_that(".resampleDataSetWeights() scales point weights by the square root of the pick count", {
  weights <- list(a = c(1, 1), b = c(1, 1), c = c(1, 1))
  resampled <- .withSeed(3, .resampleDataSetWeights(weights))
  counts <- vapply(resampled, function(w) (w[[1]])^2, numeric(1))
  expect_equal(sum(counts), 3)
  expect_true(all(abs(counts - round(counts)) < 1e-12))
})

test_that(".resampleDataSetWeights() warns nowhere when no weight reaches 1 and keeps zero weights zero", {
  weights <- list(a = c(0.5, 0.5), b = c(0, 0))
  expect_no_warning(resampled <- .withSeed(3, .resampleDataSetWeights(weights)))
  expect_identical(resampled$b, c(0, 0))
})
