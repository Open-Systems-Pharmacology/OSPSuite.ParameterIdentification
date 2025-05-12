createModifiedMappingState <- function(outputMappingState) {
  newWeights <- lapply(outputMappingState$dataSetWeights, function(dsWeights) {
    lapply(dsWeights, function(w) w * 2)
  })

  newValues <- lapply(outputMappingState$dataSetValues, function(dsValues) {
    lapply(dsValues, function(v) {
      v$yValues <- v$yValues + 1
      v
    })
  })

  list(dataSetWeights = newWeights, dataSetValues = newValues)
}


test_that(".extractOutputMappingState works for single mapping with one dataset", {
  mapping <- testOutputMapping()
  mappingState <- .extractOutputMappingState(mapping)
  expect_snapshot_value(capture.output(str(mappingState)), style = "deparse")
})

test_that(".extractOutputMappingState works for single mapping with multiple datasets", {
  mapping <- PIOutputMapping$new(quantity = testQuantity)
  mapping$addObservedDataSets(testObservedDataMultiple())
  mappingState <- .extractOutputMappingState(list(mapping))
  expect_snapshot_value(capture.output(str(mappingState)), style = "deparse")
})

test_that(".extractOutputMappingState works for multiple mappings with single dataset", {
  mappingMultiple <- list(outputMapping_250mg, outputMapping_500mg)
  mappingState <- .extractOutputMappingState(mappingMultiple)
  expect_snapshot_value(capture.output(str(mappingState)), style = "deparse")
})

test_that(".applyOutputMappingState correctly updates mapping state [single/multiple]", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$addObservedDataSets(testObservedDataMultiple())
  outputMappingMultiple <- list(outputMapping)

  originalState <- .extractOutputMappingState(outputMappingMultiple)
  modifiedState <- createModifiedMappingState(originalState)

  .applyOutputMappingState(outputMappingMultiple, modifiedState)

  appliedState <- .extractOutputMappingState(outputMappingMultiple)
  expect_equal(appliedState, modifiedState, tolerance = 1e-6)
})

test_that(".applyOutputMappingState restores original state after modification [single/multiple]", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$addObservedDataSets(testObservedDataMultiple())
  outputMappingMultiple <- list(outputMapping)

  originalState <- .extractOutputMappingState(outputMappingMultiple)
  modifiedState <- createModifiedMappingState(originalState)

  .applyOutputMappingState(outputMappingMultiple, modifiedState)
  .applyOutputMappingState(outputMappingMultiple, originalState)

  restoredState <- .extractOutputMappingState(outputMappingMultiple)
  expect_equal(restoredState, originalState, tolerance = 1e-6)
})

test_that(".applyOutputMappingState correctly updates mapping state [multiple/single]", {
  outputMappingMultiple <- list(outputMapping_250mg, outputMapping_500mg)
  originalState <- .extractOutputMappingState(outputMappingMultiple)
  modifiedState <- createModifiedMappingState(originalState)

  .applyOutputMappingState(outputMappingMultiple, modifiedState)

  appliedState <- .extractOutputMappingState(outputMappingMultiple)
  expect_equal(appliedState, modifiedState, tolerance = 1e-6)
})

test_that(".applyOutputMappingState restores original state after modification [multiple/single]", {
  outputMappingMultiple <- list(outputMapping_250mg, outputMapping_500mg)
  originalState <- .extractOutputMappingState(outputMappingMultiple)
  modifiedState <- createModifiedMappingState(originalState)

  .applyOutputMappingState(outputMappingMultiple, modifiedState)
  .applyOutputMappingState(outputMappingMultiple, originalState)

  restoredState <- .extractOutputMappingState(outputMappingMultiple)
  expect_equal(restoredState, originalState, tolerance = 1e-6)
})
