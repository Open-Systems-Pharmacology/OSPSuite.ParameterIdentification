test_that(".prepareGPRModels fits a GPR model for datasets with ArithmeticStdDev errors", {
  testOutputMapping <- getTestOutputMapping()
  mapping <- testOutputMapping()

  expect_message(
    expect_no_error(
      result <- .prepareGPRModels(mapping)
    ),
    messages$statusGPRModelFitted(mapping[[1]]$observedDataSets[[1]]$name)
  )
  expect_s4_class(result[[1]][[1]], "km")
  expect_equal(result[[1]][[1]]@covariance@range.val, 24.15, tolerance = 1e-3)
})

test_that(".prepareGPRModels fits a GPR model for datasets with GeometricStdDev errors", {
  mapping <- testOutputMapping()

  # Convert yErrorValues to GSD
  y <- mapping[[1]]$observedDataSets[[1]]$yValues
  yErr <- mapping[[1]]$observedDataSets[[1]]$yErrorValues
  relVar <- (yErr / y)^2
  sqrtLogTerm <- sqrt(log1p(relVar))
  gsd <- exp(sqrtLogTerm)

  dataSet <- DataSet$new(name = mapping[[1]]$observedDataSets[[1]]$name)
  dataSet$setValues(
    xValues = mapping[[1]]$observedDataSets[[1]]$xValues,
    yValues = mapping[[1]]$observedDataSets[[1]]$yValues,
    yErrorValues = gsd
  )
  dataSet$yErrorType <- "GeometricStdDev"

  mapping <- PIOutputMapping$new(
    quantity = getQuantity(
      "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      container = testSimulation()
    )
  )
  mapping$addObservedDataSets(dataSet)
  mapping <- list(mapping)

  expect_message(
    expect_no_error(
      result <- .prepareGPRModels(mapping)
    ),
    messages$statusGPRModelFitted(mapping[[1]]$observedDataSets[[1]]$name)
  )
  expect_s4_class(result[[1]][[1]], "km")
  expect_equal(result[[1]][[1]]@covariance@range.val, 25.08, tolerance = 1e-3)
})
