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
