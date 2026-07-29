# Testing PIParameters with a Single Parameter ----------------------------

testParam <- ospsuite::getParameter("Aciclovir|Permeability", testSimulation())
refVal <- testParam$value

test_that("PIParameters object is correctly created", {
  piParam <- PIParameters$new(testParam)
  expect_s3_class(piParam, "PIParameters")
  expect_equal(piParam$parameters, list(testParam))
  expect_equal(piParam$startValue, refVal)
  expect_equal(piParam$currValue, refVal)
  expect_equal(piParam$minValue, refVal * 0.1)
  expect_equal(piParam$maxValue, refVal * 10)
  expect_equal(piParam$unit, testParam$unit)
})

test_that("PIParameters can print PIOutputMapping", {
  piParam <- PIParameters$new(testParam)
  expect_snapshot(print(piParam))
})

test_that("PIParameters can export to data.frame", {
  piParam <- PIParameters$new(testParam)
  expect_snapshot(piParam$toDataFrame())
})

test_that("Start, min, and max values are set correctly (single parameter)", {
  piParam <- PIParameters$new(testParam)
  newStartValue <- refVal * 2
  piParam$startValue <- newStartValue
  expect_equal(piParam$startValue, newStartValue)
  expect_snapshot(piParam$minValue <- (newStartValue * 2), error = TRUE)
  expect_snapshot(piParam$maxValue <- (newStartValue / 2), error = TRUE)
  piParam$minValue <- (newStartValue / 2)
  piParam$maxValue <- (newStartValue * 2)
  expect_equal(piParam$minValue, newStartValue / 2)
  expect_equal(piParam$maxValue, newStartValue * 2)
})

test_that("Parameter value is set and retrieved correctly", {
  piParam <- PIParameters$new(testParam)
  newValue <- refVal * 1.5
  piParam$setValue(newValue)
  expect_equal(piParam$currValue, newValue)

  piParam$setValue(refVal)
  expect_equal(piParam$currValue, refVal)
})

test_that("Read-only fields cannot be set", {
  piParam <- PIParameters$new(testParam)
  expect_error(piParam$parameters <- list(testParam), "is readonly")
  expect_error(piParam$currValue <- refVal, "is readonly")
})

test_that("Unit can be changed correctly", {
  piParam <- PIParameters$new(testParam)
  newUnit <- "cm/min"
  piParam$unit <- newUnit
  expect_equal(piParam$unit, newUnit)
  expect_equal(
    piParam$currValue,
    ospsuite::toUnit(testParam, refVal, targetUnit = newUnit)
  )
  expect_error(
    piParam$unit <- "invalidUnit",
    messages$errorUnitNotSupported("invalidUnit", "Velocity"),
    fixed = TRUE
  )
})

# Testing PIParameters with a List of Parameters --------------------------

testParamsList <- list(
  ospsuite::getParameter("Organism|Liver|Volume", testSimulation()),
  ospsuite::getParameter("Organism|Kidney|Volume", testSimulation()),
  ospsuite::getParameter("Organism|Brain|Volume", testSimulation())
)
refVal <- testParamsList[[1]]$value

test_that("PIParameters object is correctly created from list of parameters", {
  piParam <- PIParameters$new(testParamsList)
  expect_s3_class(piParam, "PIParameters")
  expect_equal(piParam$parameters, testParamsList)
  expect_equal(length(piParam$parameters), length(testParamsList))
  expect_equal(piParam$startValue, refVal)
  expect_equal(piParam$currValue, refVal)
  expect_equal(piParam$minValue, refVal * 0.1)
  expect_equal(piParam$maxValue, refVal * 10)
  expect_equal(piParam$unit, testParamsList[[1]]$unit)
})

test_that("PIParameters with multiple parameters can export to data.frame", {
  piParam <- PIParameters$new(testParam)
  expect_snapshot(piParam$toDataFrame())
})

test_that("Start, min, and max values are set correctly (multiple parameters)", {
  piParam <- PIParameters$new(testParamsList)
  newStartValue <- refVal * 2
  piParam$startValue <- newStartValue
  expect_equal(piParam$startValue, newStartValue)
  expect_snapshot(piParam$minValue <- (newStartValue * 2), error = TRUE)
  expect_snapshot(piParam$maxValue <- (newStartValue / 2), error = TRUE)
  piParam$minValue <- (newStartValue / 2)
  piParam$maxValue <- (newStartValue * 2)
  expect_equal(piParam$minValue, newStartValue / 2)
  expect_equal(piParam$maxValue, newStartValue * 2)
})

test_that("Parameter value is set and retrieved correctly", {
  piParam <- PIParameters$new(testParamsList)
  newValue <- refVal * 1.5
  piParam$setValue(newValue)
  expect_equal(piParam$currValue, newValue)

  piParam$setValue(refVal)
  expect_equal(piParam$currValue, refVal)
})

test_that("Read-only properties cannot be set", {
  piParam <- PIParameters$new(testParamsList)
  expect_error(piParam$parameters <- testParamsList, "is readonly")
  expect_error(piParam$currValue <- refVal, "is readonly")
})

test_that("Unit can be changed correctly", {
  piParam <- PIParameters$new(testParamsList)
  newUnit <- "ml"
  piParam$unit <- newUnit
  expect_equal(piParam$unit, newUnit)
  expect_equal(
    piParam$currValue,
    ospsuite::toUnit(testParamsList[[1]], refVal, targetUnit = newUnit)
  )
  expect_error(
    piParam$unit <- "invalidUnit",
    messages$errorUnitNotSupported("invalidUnit", "Volume"),
    fixed = TRUE
  )
})

test_that("Zero start value without explicit bounds errors", {
  zeroParam <- ospsuite::getParameter(
    "Aciclovir|Permeability",
    testSimulation()
  )
  origValue <- zeroParam$value
  zeroParam$setValue(0)
  on.exit(zeroParam$setValue(origValue))

  expect_snapshot(PIParameters$new(zeroParam), error = TRUE)
})

test_that("Zero start value with explicit bounds is accepted", {
  zeroParam <- ospsuite::getParameter(
    "Aciclovir|Permeability",
    testSimulation()
  )
  origValue <- zeroParam$value
  zeroParam$setValue(0)
  on.exit(zeroParam$setValue(origValue))

  piParam <- PIParameters$new(zeroParam, minValue = -1, maxValue = 1)
  expect_equal(piParam$startValue, 0)
  expect_equal(piParam$minValue, -1)
  expect_equal(piParam$maxValue, 1)
})

test_that("Negative start value auto-generates ordered bounds", {
  negParam <- ospsuite::getParameter(
    "Aciclovir|Permeability",
    testSimulation()
  )
  origValue <- negParam$value
  negParam$setValue(-2)
  on.exit(negParam$setValue(origValue))

  piParam <- PIParameters$new(negParam)
  expect_equal(piParam$startValue, -2)
  expect_equal(piParam$minValue, -20)
  expect_equal(piParam$maxValue, -0.2)
})

test_that("Zero-width bounds are rejected at a zero start value", {
  zeroParam <- ospsuite::getParameter(
    "Aciclovir|Permeability",
    testSimulation()
  )
  origValue <- zeroParam$value
  zeroParam$setValue(0)
  on.exit(zeroParam$setValue(origValue))

  expect_snapshot(
    PIParameters$new(zeroParam, minValue = 0, maxValue = 0),
    error = TRUE
  )
})

test_that("Zero-width bounds are rejected at a non-zero start value", {
  param <- ospsuite::getParameter("Aciclovir|Permeability", testSimulation())
  origValue <- param$value
  param$setValue(5)
  on.exit(param$setValue(origValue))

  expect_snapshot(
    PIParameters$new(param, minValue = 5, maxValue = 5),
    error = TRUE
  )
})

test_that("A narrow but positive-width range is accepted", {
  param <- ospsuite::getParameter("Aciclovir|Permeability", testSimulation())
  origValue <- param$value
  param$setValue(5)
  on.exit(param$setValue(origValue))

  piParam <- PIParameters$new(param, minValue = 5 - 1e-4, maxValue = 5 + 1e-4)
  expect_equal(piParam$minValue, 5 - 1e-4)
  expect_equal(piParam$maxValue, 5 + 1e-4)
})

test_that("Setters cannot collapse the range to zero width", {
  param <- ospsuite::getParameter("Aciclovir|Permeability", testSimulation())
  origValue <- param$value
  param$setValue(5)
  on.exit(param$setValue(origValue))

  piParam <- PIParameters$new(param)
  piParam$minValue <- 5
  expect_snapshot(piParam$maxValue <- 5, error = TRUE)
})
