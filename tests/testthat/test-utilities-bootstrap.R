test_that(".validateSubjectCounts() stops for aggregated data without a count", {
  mapping <- PIOutputMapping$new(quantity = testQuantity())
  mapping$addObservedDataSets(testObservedData())
  expect_snapshot(.validateSubjectCounts(list(mapping)), error = TRUE)
  mapping$setSubjectCount(13)
  expect_invisible(.validateSubjectCounts(list(mapping)))
})

test_that(".validateSubjectCounts() stops for an aggregated data set left out of named counts", {
  mapping <- aggregatedStudiesMapping()
  mapping$setSubjectCount(c(studyA = 12))
  expect_snapshot(.validateSubjectCounts(list(mapping)), error = TRUE)
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

test_that("aggregated data with only zero SDs or unit GSDs need no subject count", {
  for (errorCase in list(
    list(type = "ArithmeticStdDev", value = 0),
    list(type = "GeometricStdDev", value = 1)
  )) {
    dataSet <- testObservedData()[[1]]
    dataSet$setValues(
      dataSet$xValues,
      dataSet$yValues,
      rep(errorCase$value, length(dataSet$yValues))
    )
    dataSet$yErrorType <- errorCase$type
    mapping <- PIOutputMapping$new(quantity = testQuantity())
    mapping$addObservedDataSets(syntheticObservedData())
    mapping$addObservedDataSets(dataSet)
    expect_no_error(.validateSubjectCounts(list(mapping)))
    expect_message(
      .classifyObservedData(list(mapping)),
      messages$statusAggregatedWithoutErrors(dataSet$name),
      fixed = TRUE
    )
  }
})

test_that(".classifyObservedData() reports aggregated data without error values as drawn whole", {
  mapping <- PIOutputMapping$new(quantity = testQuantity())
  dataSet <- testObservedData()[[1]]
  dataSet$setValues(
    dataSet$xValues,
    dataSet$yValues,
    rep(NA_real_, length(dataSet$yValues))
  )
  mapping$addObservedDataSets(dataSet)
  expect_snapshot(.classifyObservedData(list(mapping)))
})

test_that(".withSeed() leaves the global random state as it was", {
  set.seed(1)
  before <- .Random.seed
  .withSeed(99, stats::rnorm(5))
  expect_identical(.Random.seed, before)
  expect_identical(
    .withSeed(99, stats::rnorm(2)),
    .withSeed(99, stats::rnorm(2))
  )
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
  # Without the - s2 / 2 term the means are off by about 1 percent
  expect_equal(rowMeans(draws), values$yValues, tolerance = 0.003)
  expect_equal(
    apply(draws, 1, sd),
    values$yErrorValues / sqrt(13),
    tolerance = 0.05
  )
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
  individual <- list(
    xValues = 1:2,
    yValues = c(1, 2),
    yErrorValues = NULL,
    yErrorType = NULL
  )
  expect_identical(
    .resampleDataSetValues(individual, subjectCount = 13),
    individual
  )
  noErrors <- list(
    xValues = 1:2,
    yValues = c(1, 2),
    yErrorValues = c(NA, NA),
    yErrorType = "ArithmeticStdDev"
  )
  expect_identical(
    .resampleDataSetValues(noErrors, subjectCount = 13),
    noErrors
  )
})

test_that(".resampleDataSetWeights() scales point weights by the square root of the pick count", {
  weights <- list(a = c(1, 1), b = c(1, 1), c = c(1, 1))
  pickCounts <- .withSeed(
    6,
    table(factor(
      sample(names(weights), 3, replace = TRUE),
      levels = names(weights)
    ))
  )
  expect_true(any(pickCounts >= 2))
  resampled <- .withSeed(6, .resampleDataSetWeights(weights))
  expect_equal(
    vapply(resampled, `[[`, numeric(1), 1),
    sqrt(as.numeric(pickCounts)),
    ignore_attr = TRUE
  )
})

test_that(".resampleDataSetWeights() warns nowhere when no weight reaches 1 and keeps zero weights zero", {
  weights <- list(a = c(0.5, 0.5), b = c(0, 0))
  expect_no_warning(resampled <- .withSeed(3, .resampleDataSetWeights(weights)))
  expect_identical(resampled$b, c(0, 0))
})

test_that(".resampleDataSetWeights() keeps a user weight w at its expected pick count", {
  # Pool multiplicity w, so sum(k) is the pool size and w_res = w * sqrt(k / w)
  weights <- list(a = c(20, 20), b = c(1, 1))
  resampled <- .withSeed(5, .resampleDataSetWeights(weights))
  pickCounts <- c(resampled$a[[1]]^2 / 20, resampled$b[[1]]^2 / 1)
  expect_equal(sum(pickCounts), 21)
  expect_true(all(abs(pickCounts - round(pickCounts)) < 1e-9))
})

test_that(".resampleDataSetWeights() keeps a non-integer user weight at its expected pick count", {
  # Weight 1.5 enters the pool once, so w_res = 1.5 * sqrt(k)
  weights <- list(a = c(1.5, 1.5), b = c(1, 1))
  resampled <- .withSeed(5, .resampleDataSetWeights(weights))
  pickCounts <- c(resampled$a[[1]]^2 / 1.5^2, resampled$b[[1]]^2)
  expect_equal(sum(pickCounts), 2)
  expect_true(all(abs(pickCounts - round(pickCounts)) < 1e-9))
})

test_that(".resampleDataSetWeights() drops a data set whose whole weights are all infinite", {
  resampled <- .withSeed(
    5,
    .resampleDataSetWeights(list(a = c(Inf, Inf), b = c(1, 1)))
  )
  expect_identical(resampled$a, c(0, 0))
})

test_that(".resampleAndApplyMappingState() draws output mappings independently and reproducibly", {
  mixedMappings <- function() {
    lapply(1:2, function(i) {
      mapping <- PIOutputMapping$new(quantity = testQuantity())
      mapping$addObservedDataSets(syntheticObservedData())
      mapping$addObservedDataSets(testObservedData())
      mapping$setSubjectCount(13)
      mapping
    })
  }
  resampled <- function(seed) {
    mappings <- mixedMappings()
    state <- .extractOutputMappingState(mappings)
    .resampleAndApplyMappingState(mappings, state, seed = seed)
    lapply(mappings, function(mapping) {
      list(
        weights = mapping$dataWeights,
        means = mapping$observedDataSets[[
          "AciclovirLaskinData.Laskin 1982.Group A"
        ]]$yValues
      )
    })
  }
  first <- resampled(seed = 4)
  expect_false(identical(first[[1]]$weights, first[[2]]$weights))
  expect_false(identical(first[[1]]$means, first[[2]]$means))
  expect_identical(resampled(seed = 4), first)
})

test_that(".resampleAndApplyMappingState() redraws each data set with its own subject count", {
  redrawnLogDeviations <- function(subjectCounts) {
    mapping <- aggregatedStudiesMapping()
    mapping$setSubjectCount(subjectCounts)
    mappings <- list(mapping)
    .resampleAndApplyMappingState(
      mappings,
      .extractOutputMappingState(mappings),
      seed = 3
    )
    lapply(mapping$observedDataSets, function(dataSet) {
      log(dataSet$yValues / c(10, 20))
    })
  }
  sameCount <- redrawnLogDeviations(c(studyA = 12, studyB = 12))
  fourfoldCount <- redrawnLogDeviations(c(studyA = 12, studyB = 48))
  # DataSet stores values in single precision
  expect_equal(fourfoldCount$studyA, sameCount$studyA, tolerance = 1e-6)
  expect_equal(fourfoldCount$studyB, sameCount$studyB / 2, tolerance = 1e-6)
})

test_that(".resampleAndApplyMappingState() redraws means with errors converted to the unit of the means", {
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
    state <- .extractOutputMappingState(mappings)
    .resampleAndApplyMappingState(mappings, state, seed = 7)
    mappings[[1]]$observedDataSets$aggregated$yValues
  }
  expect_equal(
    redrawnMeans(aggregatedMapping(c(2000, 4000), "µg/l")),
    redrawnMeans(aggregatedMapping(c(2, 4), "mg/l"))
  )
})
