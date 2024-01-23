test_that("PIConfiguration object is correctly created", {
  piConfiguration <- PIConfiguration$new()
  expect_s3_class(piConfiguration, "PIConfiguration")
})

test_that("Initialization sets default values correctly", {
  piConfiguration <- PIConfiguration$new()
  expect_false(piConfiguration$simulateSteadyState)
  expect_equal(piConfiguration$steadyStateTime, 1000)
  expect_false(piConfiguration$printEvaluationFeedback)
  expect_equal(piConfiguration$targetFunctionType, "lsq")
  expect_equal(piConfiguration$algorithm, "BOBYQA")
})

test_that("simulateSteadyState can be set and retrieved correctly", {
  piConfiguration <- PIConfiguration$new()
  piConfiguration$simulateSteadyState <- TRUE
  expect_true(piConfiguration$simulateSteadyState)
  piConfiguration$simulateSteadyState <- FALSE
  expect_false(piConfiguration$simulateSteadyState)
})

test_that("steadyStateTime can be set and retrieved correctly", {
  piConfiguration <- PIConfiguration$new()
  piConfiguration$steadyStateTime <- 500
  expect_equal(piConfiguration$steadyStateTime, 500)
  expect_error(piConfiguration$steadyStateTime <- -1,
               "steadyStateTime must be a positive numerical value, but the value is ")
})

test_that("printEvaluationFeedback can be set and retrieved correctly", {
  piConfiguration <- PIConfiguration$new()
  piConfiguration$printEvaluationFeedback <- TRUE
  expect_true(piConfiguration$printEvaluationFeedback)
  piConfiguration$printEvaluationFeedback <- FALSE
  expect_false(piConfiguration$printEvaluationFeedback)
})

test_that("targetFunctionType can be set and retrieved correctly", {
  piConfiguration <- PIConfiguration$new()
  validFunctionType <- "m3"
  piConfiguration$targetFunctionType <- validFunctionType
  expect_equal(piConfiguration$targetFunctionType, validFunctionType)
  expect_error(piConfiguration$targetFunctionType <- "invalidType")
})

test_that("algorithm can be set and retrieved correctly", {
  piConfiguration <- PIConfiguration$new()
  validAlgorithm <- "HJKB"
  piConfiguration$algorithm <- validAlgorithm
  expect_equal(piConfiguration$algorithm, validAlgorithm)
  expect_error(piConfiguration$algorithm <- "invalidAlgorithm")
})
