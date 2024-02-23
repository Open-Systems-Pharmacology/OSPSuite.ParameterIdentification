test_that("PIConfiguration object is correctly created", {
  piConfiguration <- PIConfiguration$new()
  expect_s3_class(piConfiguration, "PIConfiguration")
})

test_that("Initialization sets default values correctly", {
  piConfiguration <- PIConfiguration$new()
  # expect_false(piConfiguration$simulateSteadyState)
  # expect_equal(piConfiguration$steadyStateTime, 1000)
  expect_false(piConfiguration$printEvaluationFeedback)
  expect_equal(
    piConfiguration$objectiveFunctionOptions$objectiveFunctionType,
    "lsq"
  )
  expect_equal(
    piConfiguration$objectiveFunctionOptions$residualWeightingMethod,
    "none"
  )
  expect_equal(
    piConfiguration$objectiveFunctionOptions$robustMethod,
    "none"
  )
  expect_equal(piConfiguration$algorithm, "BOBYQA")
})

# test_that("simulateSteadyState can be set and retrieved correctly", {
#   piConfiguration <- PIConfiguration$new()
#   piConfiguration$simulateSteadyState <- TRUE
#   expect_true(piConfiguration$simulateSteadyState)
#   piConfiguration$simulateSteadyState <- FALSE
#   expect_false(piConfiguration$simulateSteadyState)
# })
#
# test_that("steadyStateTime can be set and retrieved correctly", {
#   piConfiguration <- PIConfiguration$new()
#   piConfiguration$steadyStateTime <- 500
#   expect_equal(piConfiguration$steadyStateTime, 500)
#   expect_error(
#     piConfiguration$steadyStateTime <- -1,
#     "steadyStateTime must be a positive numerical value, but the value is "
#   )
# })

test_that("printEvaluationFeedback can be set and retrieved correctly", {
  piConfiguration <- PIConfiguration$new()
  piConfiguration$printEvaluationFeedback <- TRUE
  expect_true(piConfiguration$printEvaluationFeedback)
  piConfiguration$printEvaluationFeedback <- FALSE
  expect_false(piConfiguration$printEvaluationFeedback)
})

test_that("objectiveFunctionOptions can be set and retrieved correctly", {
  piConfiguration <- PIConfiguration$new()
  expect_silent(
    piConfiguration$objectiveFunctionOptions <- list(
      objectiveFunctionType = "m3",
      residualWeightingMethod = "std",
      robustMethod = "huber",
      scaleVar = TRUE,
      scaling = "log",
      linScaleCV = 0.5,
      logScaleSD = 0.5
    )
  )
  expect_equal(
    piConfiguration$objectiveFunctionOptions$objectiveFunctionType,
    "m3"
  )
  expect_equal(
    piConfiguration$objectiveFunctionOptions$residualWeightingMethod,
    "std"
  )
  expect_equal(
    piConfiguration$objectiveFunctionOptions$robustMethod,
    "huber"
  )
  expect_true(piConfiguration$objectiveFunctionOptions$scaleVar)
  expect_equal(
    piConfiguration$objectiveFunctionOptions$scaling,
    "log"
  )
  expect_equal(
    piConfiguration$objectiveFunctionOptions$linScaleCV,
    0.5
  )
  expect_equal(
    piConfiguration$objectiveFunctionOptions$logScaleSD,
    0.5
  )
  expect_error(
    piConfiguration$objectiveFunctionOptions <- list(
      objectiveFunctionType = "invalidType"
    )
  )
  expect_error(
    piConfiguration$objectiveFunctionOptions <- list(
      invalidField = "lsq"
    )
  )
})

test_that("algorithm can be set and retrieved correctly", {
  piConfiguration <- PIConfiguration$new()
  validAlgorithm <- "HJKB"
  piConfiguration$algorithm <- validAlgorithm
  expect_equal(piConfiguration$algorithm, validAlgorithm)
  expect_error(piConfiguration$algorithm <- "invalidAlgorithm")
})
