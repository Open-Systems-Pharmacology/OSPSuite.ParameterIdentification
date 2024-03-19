# validateIsOption --------------------------------------------------------

validOptions <- list(
  objectiveFunctionType = c("lsq", "m3"),
  residualWeightingMethod = c("none", "std", "mean", "error"),
  scaleVar = c(TRUE, FALSE),
  linScaleCV = list(type = "numeric", min = 0, max = 1)
)

test_that("It runs without error when options are valid", {
  inputOptions <- list(
    objectiveFunctionType = "lsq",
    residualWeightingMethod = "std",
    scaleVar = FALSE,
    linScaleCV = 0.8
  )
  expect_silent(validateIsOption(inputOptions, validOptions))
})

test_that("It throws an error when one option value is invalid", {
  inputOptions <- list(
    objectiveFunctionType = "invalidType",
    residualWeightingMethod = "std",
    scaleVar = FALSE,
    linScaleCV = 0.8
  )
  expect_error(validateIsOption(inputOptions, validOptions))
})

test_that("It throws an error when several option values are invalid", {
  inputOptions <- list(
    objectiveFunctionType = "invalidType",
    residualWeightingMethod = "invalidMethod",
    scaleVar = FALSE,
    linScaleCV = 0.8
  )
  expect_error(validateIsOption(inputOptions, validOptions))
})

test_that("It throws an error when boolean option is invalid", {
  inputOptions <- list(
    objectiveFunctionType = "lsq",
    residualWeightingMethod = "std",
    scaleVar = 1,
    linScaleCV = 0.8
  )
  expect_error(validateIsOption(inputOptions, validOptions))
})

test_that("It throws an error when numeric option is invalid", {
  inputOptions <- list(
    objectiveFunctionType = "lsq",
    residualWeightingMethod = "std",
    scaleVar = 1,
    linScaleCV = "invalidValue"
  )
  expect_error(validateIsOption(inputOptions, validOptions))
})

test_that("It throws an error when numeric option is out of range", {
  inputOptions <- list(
    objectiveFunctionType = "lsq",
    residualWeightingMethod = "std",
    scaleVar = 1,
    linScaleCV = 2
  )
  expect_error(validateIsOption(inputOptions, validOptions))
})
