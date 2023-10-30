sim <- loadTestSimulation("Aciclovir")
param <- getParameter("Aciclovir|Permeability", sim)
livVol <- getParameter("Organism|Liver|Volume", sim)
kidvVol <- getParameter("Organism|Kidney|Volume", sim)
brainVol <- getParameter("Organism|Brain|Volume", sim)
params <- list(livVol, kidvVol, brainVol)
refVal <- param$value

test_that("It can create a PIParameters object", {
  piParam <- PIParameters$new(param)
  expect_error(capture.output(print(piParam)), NA)
})

test_that("It can retrieve the parameter object", {
  piParam <- PIParameters$new(param)
  expect_error(capture.output(piParam$parameters), NA)
})

test_that("It can return the current value", {
  piParam <- PIParameters$new(param)
  expect_equal(piParam$currValue, refVal)
})

test_that("It can change the value", {
  piParam <- PIParameters$new(param)
  piParam$setValue(1)
  expect_equal(piParam$currValue, 1)
  piParam$setValue(refVal)
})

test_that("It can return the start value", {
  piParam <- PIParameters$new(param)
  expect_equal(piParam$startValue, refVal)
})

test_that("It can return and set the min value", {
  piParam <- PIParameters$new(param)
  expect_equal(piParam$minValue, refVal * 0.1)
  piParam$minValue <- refVal * 0.5
  expect_equal(piParam$minValue, refVal * 0.5)

  expect_error(capture.output(piParam$minValue <- 1))
})

test_that("It can return and set the max value", {
  piParam <- PIParameters$new(param)
  expect_equal(piParam$maxValue, refVal * 10)
  piParam$maxValue <- refVal * 5
  expect_equal(piParam$maxValue, refVal * 5)

  expect_error(capture.output(piParam$maxValue <- 0))
})

test_that("It can change the unit", {
  piParam <- PIParameters$new(param)
  expect_equal(piParam$unit, param$unit)
  piParam$unit <- "cm/min"
  expect_equal(piParam$currValue, toUnit(param, refVal, targetUnit = "cm/min"))
})


##### Now the same but with a list of parameters...
refVal <- params[[1]]$value
test_that("It can create a PIParameters object", {
  piParam <- PIParameters$new(params)
  expect_error(capture.output(print(piParam)), NA)
})

test_that("It can retrieve the parameter object", {
  piParam <- PIParameters$new(params)
  expect_error(capture.output(piParam$parameters), NA)
})

test_that("It can return the current value", {
  piParam <- PIParameters$new(params)
  expect_equal(piParam$currValue, refVal)
})

test_that("It can change the value", {
  piParam <- PIParameters$new(params)
  piParam$setValue(1)
  expect_equal(piParam$currValue, 1)
  piParam$setValue(refVal)
})

test_that("It can return the start value", {
  piParam <- PIParameters$new(params)
  piParam$setValue(1)
  expect_equal(piParam$startValue, refVal)
  piParam$setValue(refVal)
})

test_that("It can return and set the min value", {
  piParam <- PIParameters$new(params)
  expect_equal(piParam$minValue, refVal * 0.1)
  piParam$minValue <- refVal * 0.5
  expect_equal(piParam$minValue, refVal * 0.5)

  expect_error(capture.output(piParam$minValue <- 100))
})

test_that("It can return and set the max value", {
  piParam <- PIParameters$new(params)
  expect_equal(piParam$maxValue, refVal * 10)
  piParam$maxValue <- refVal * 5
  expect_equal(piParam$maxValue, refVal * 5)

  expect_error(capture.output(piParam$maxValue <- 0))
})

test_that("It can change the unit", {
  piParam <- PIParameters$new(params)
  expect_equal(piParam$unit, params[[1]]$unit)
  piParam$unit <- "ml"
  expect_equal(piParam$currValue, toUnit(params[[1]], refVal, targetUnit = "ml"))
})

test_that("Trying to set wrong unit", {
  piParam <- PIParameters$new(params)
  expect_error(capture.output(piParam$unit <- "cm"))
})
