test_that("PIConfiguration object is correctly created", {
  piConfiguration <- PIConfiguration$new()
  expect_s3_class(piConfiguration, "PIConfiguration")
})

test_that("PIConfiguration instance prints without errors", {
  piConfiguration <- PIConfiguration$new()
  expect_snapshot(print(piConfiguration))
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

test_that("autoEstimateCI can be set and retrieved correctly", {
  piConfiguration <- PIConfiguration$new()
  piConfiguration$autoEstimateCI <- FALSE
  expect_false(piConfiguration$autoEstimateCI)
  piConfiguration$autoEstimateCI <- TRUE
  expect_true(piConfiguration$autoEstimateCI)
})

test_that("objectiveFunctionOptions can be set and retrieved correctly", {
  piConfiguration <- PIConfiguration$new()
  expect_silent(
    piConfiguration$objectiveFunctionOptions <- list(
      objectiveFunctionType = "m3",
      residualWeightingMethod = "std",
      robustMethod = "huber",
      scaleVar = TRUE,
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
    piConfiguration$objectiveFunctionOptions$linScaleCV,
    0.5
  )
  expect_equal(
    piConfiguration$objectiveFunctionOptions$logScaleSD,
    0.5
  )
})

test_that("algorithm can be set and retrieved correctly", {
  piConfiguration <- PIConfiguration$new()
  validAlgorithm <- "HJKB"
  piConfiguration$algorithm <- validAlgorithm
  expect_equal(piConfiguration$algorithm, validAlgorithm)
  expect_error(piConfiguration$algorithm <- "invalidAlgorithm")
})


# objectiveFunctionOptions validation

test_that("objectiveFunctionOptions single-field assignment is validated", {
  piConfiguration <- PIConfiguration$new()
  piConfiguration$objectiveFunctionOptions$objectiveFunctionType <- "m3"

  expect_equal(
    piConfiguration$objectiveFunctionOptions$objectiveFunctionType,
    "m3"
  )
  expect_equal(
    piConfiguration$objectiveFunctionOptions$residualWeightingMethod,
    "none"
  )
  expect_equal(
    piConfiguration$objectiveFunctionOptions$scaleVar,
    FALSE
  )
})

test_that("objectiveFunctionOptions rejects invalid values", {
  piConfiguration <- PIConfiguration$new()
  expect_error(
    piConfiguration$objectiveFunctionOptions$objectiveFunctionType <- "invalid",
    regexp = "objectiveFunctionType"
  )
})

test_that("objectiveFunctionOptions rejects out-of-range numeric", {
  piConfiguration <- PIConfiguration$new()
  expect_error(
    piConfiguration$objectiveFunctionOptions$linScaleCV <- 5,
    regexp = "linScaleCV"
  )
})

test_that("objectiveFunctionOptions partial list merges with current settings", {
  piConfiguration <- PIConfiguration$new()
  piConfiguration$objectiveFunctionOptions <- list(objectiveFunctionType = "m3")

  expect_equal(
    piConfiguration$objectiveFunctionOptions$objectiveFunctionType,
    "m3"
  )
  expect_equal(
    piConfiguration$objectiveFunctionOptions$residualWeightingMethod,
    "none"
  )
})

test_that("objectiveFunctionOptions warns and ignores unknown keys", {
  piConfiguration <- PIConfiguration$new()

  expect_warning(
    piConfiguration$objectiveFunctionOptions <- list(unknownKey = "x"),
    messages$warningUnknownOptions("unknownKey", "objectiveFunctionOptions"),
    fixed = TRUE
  )
  expect_false(
    "unknownKey" %in% names(piConfiguration$objectiveFunctionOptions)
  )
})


# algorithmOptions validation

test_that("algorithmOptions NULL when not set", {
  piConfiguration <- PIConfiguration$new()
  expect_null(piConfiguration$algorithmOptions)
})

test_that("algorithmOptions single-field assignment works", {
  piConfiguration <- PIConfiguration$new()
  piConfiguration$algorithmOptions$maxeval <- 500L
  expect_equal(piConfiguration$algorithmOptions$maxeval, 500L)
})

test_that("algorithmOptions warns and ignores unknown keys", {
  piConfiguration <- PIConfiguration$new()
  expect_warning(
    piConfiguration$algorithmOptions <- list(unknownKey = 1),
    regexp = messages$warningUnknownOptions("unknownKey", "algorithmOptions"),
    fixed = TRUE
  )
  expect_null(piConfiguration$algorithmOptions)
})

test_that("algorithmOptions partial list accumulates overrides", {
  piConfiguration <- PIConfiguration$new()
  piConfiguration$algorithmOptions <- list(maxeval = 500L)
  piConfiguration$algorithmOptions <- list(xtol_rel = 1e-4)

  expect_equal(piConfiguration$algorithmOptions$maxeval, 500L)
  expect_equal(piConfiguration$algorithmOptions$xtol_rel, 1e-4)
  expect_named(
    piConfiguration$algorithmOptions,
    c("maxeval", "xtol_rel"),
    ignore.order = TRUE
  )
})

test_that("algorithmOptions reset to NULL clears overrides", {
  piConfiguration <- PIConfiguration$new()
  piConfiguration$algorithmOptions$maxeval <- 500L
  piConfiguration$algorithmOptions <- NULL
  expect_null(piConfiguration$algorithmOptions)
})

test_that("changing algorithm resets algorithmOptions with message", {
  piConfiguration <- PIConfiguration$new()
  piConfiguration$algorithmOptions$maxeval <- 500L
  
  expect_message(
    piConfiguration$algorithm <- "HJKB",
    messages$messageOptionsReset(
      "algorithm",
      "BOBYQA",
      "HJKB",
      "algorithmOptions"
    ),
    fixed = TRUE
  )
  expect_null(piConfiguration$algorithmOptions)
  piConfiguration$algorithmOptions <- list(tol = 1e-5) # HJKB-specific key
  expect_equal(piConfiguration$algorithmOptions$tol, 1e-5)
})

test_that("changing algorithm does not message when options are default", {
  piConfiguration <- PIConfiguration$new()
  expect_no_message(piConfiguration$algorithm <- "HJKB")
})

test_that("algorithmOptions rejects previous algorithm's keys after switch", {
  piConfiguration <- PIConfiguration$new()
  piConfiguration$algorithm <- "HJKB"
  expect_warning(
    piConfiguration$algorithmOptions <- list(maxeval = 500L),
    regexp = messages$warningUnknownOptions("maxeval", "algorithmOptions"),
    fixed = TRUE
  )
  expect_null(piConfiguration$algorithmOptions)
})


# ciOptions validation

test_that("ciOptions getter returns NULL when not set", {
  piConfiguration <- PIConfiguration$new()
  expect_null(piConfiguration$ciOptions)
})

test_that("ciOptions single-field assignment is validated", {
  piConfiguration <- PIConfiguration$new()
  piConfiguration$ciOptions$confLevel <- 0.9
  expect_equal(piConfiguration$ciOptions$confLevel, 0.9)
})

test_that("ciOptions rejects invalid confLevel", {
  piConfiguration <- PIConfiguration$new()
  expect_error(
    piConfiguration$ciOptions$confLevel <- 1.5,
    regexp = "confLevel"
  )
})

test_that("ciOptions warns and ignores unknown keys", {
  piConfiguration <- PIConfiguration$new()
  expect_warning(
    piConfiguration$ciOptions <- list(unknownKey = 1),
    messages$warningUnknownOptions("unknownKey", "ciOptions"),
    fixed = TRUE
  )
  expect_false("unknownKey" %in% names(piConfiguration$ciOptions))
})

test_that("ciOptions reset to NULL clears overrides", {
  piConfiguration <- PIConfiguration$new()
  piConfiguration$ciOptions$confLevel <- 0.9
  piConfiguration$ciOptions <- NULL
  expect_null(piConfiguration$ciOptions)
})

test_that("changing ciMethod resets ciOptions with message", {
  piConfiguration <- PIConfiguration$new()
  piConfiguration$ciOptions$confLevel <- 0.9
  expect_message(
    piConfiguration$ciMethod <- "PL",
    messages$messageOptionsReset("ciMethod", "hessian", "PL", "ciOptions"),
    fixed = TRUE
  )
  expect_null(piConfiguration$ciOptions)
})

test_that("changing ciMethod does not message when options are default", {
  piConfiguration <- PIConfiguration$new()
  expect_no_message(piConfiguration$ciMethod <- "PL")
})

test_that("ciOptions PL: maxIter validated and epsilon allows NULL and vector", {
  piConfiguration <- PIConfiguration$new()
  piConfiguration$ciMethod <- "PL"
  piConfiguration$ciOptions <- list(maxIter = 200L)
  expect_equal(piConfiguration$ciOptions$maxIter, 200L)
  expect_error(
    piConfiguration$ciOptions <- list(maxIter = 0L),
    regexp = messages$errorOptionValidationFailed(),
    fixed = TRUE
  )
  piConfiguration$ciOptions <- list(epsilon = NULL)
  expect_null(piConfiguration$ciOptions$epsilon)
  piConfiguration$ciOptions <- list(epsilon = c(1e-4, 1e-5))
  expect_equal(piConfiguration$ciOptions$epsilon, c(1e-4, 1e-5))
})

test_that("ciOptions bootstrap: nBootstrap validated and seed allows NULL", {
  piConfiguration <- PIConfiguration$new()
  piConfiguration$ciMethod <- "bootstrap"
  piConfiguration$ciOptions <- list(nBootstrap = 500L)
  expect_equal(piConfiguration$ciOptions$nBootstrap, 500L)
  expect_error(
    piConfiguration$ciOptions <- list(nBootstrap = 0L), regexp = "nBootstrap"
  )
  piConfiguration$ciOptions <- list(seed = 42L)
  expect_equal(piConfiguration$ciOptions$seed, 42L)
  piConfiguration$ciOptions <- list(seed = NULL)
  expect_null(piConfiguration$ciOptions$seed)
})
