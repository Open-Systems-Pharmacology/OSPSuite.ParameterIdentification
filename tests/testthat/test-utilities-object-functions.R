## context(".calculateCensoredContribution")

obsVsPredDf <- readr::read_csv(getTestDataFilePath("Aciclovir_obsVsPredDf.csv"))

obsDf <- obsVsPredDf[obsVsPredDf$dataType == "observed", ]
predDf <- obsVsPredDf[obsVsPredDf$dataType == "simulated", ]

test_that("`.calculateCensoredContribution` correctly calculates result with linear scaling", {
  obsDf$lloq <- 2.5
  result <- ospsuite.parameteridentification:::.calculateCensoredContribution(
    observed = obsDf, simulated = predDf,
    scaling = "lin", linScaleCV = 0.25)
  expect_equal(result, 0.545702, tolerance = 1e-4)
})

test_that("`.calculateCensoredContribution` correctly calculates result with logarithmic scaling", {
  obsVsPredDf$lloq <- 2.5
  obsVsPredDfLog <- ospsuite.parameteridentification:::.applyLogTransformation(obsVsPredDf)
  obsDfLog <- obsVsPredDfLog[obsVsPredDfLog$dataType == "observed", ]
  predDfLog <- obsVsPredDfLog[obsVsPredDfLog$dataType == "simulated", ]
  result <- ospsuite.parameteridentification:::.calculateCensoredContribution(
    observed = obsDfLog, simulated = predDfLog,
    scaling = "log", logScaleSD = 0.086)
  expect_equal(result, 0.437086, tolerance = 1e-4)
})

test_that("`.calculateCensoredContribution` can handle a minimal dataset with a single censored observation", {
  obsDf$lloq <- 2.5
  obsDfSingle <- obsDf[1, , drop = FALSE]
  predDfSingle <- predDf[1, , drop = FALSE]
  result <- ospsuite.parameteridentification:::.calculateCensoredContribution(
    observed = obsDfSingle, simulated = predDfSingle,
    scaling = "lin", linScaleCV = 0.2)
  expect_equal(result, 0, tolerance = 1e-4)
})

test_that("`.calculateCensoredContribution` throws an error when LLOQ values are missing in the observed data", {
  expect_error(
    result <- ospsuite.parameteridentification:::.calculateCensoredContribution(
      observed = obsDf, simulated = predDf,
      scaling = "lin", linScaleCV = 0.2)
  )
  obsDf$lloq <- NULL
  expect_error(
    result <- ospsuite.parameteridentification:::.calculateCensoredContribution(
      observed = obsDf, simulated = predDf,
      scaling = "lin", linScaleCV = 0.2)
  )
})

test_that("`.calculateCensoredContribution` throws errors on invalid options", {
  obsDf$lloq <- 2.5
  expect_error(
    result <- ospsuite.parameteridentification:::.calculateCensoredContribution(
      observed = obsDf, simulated = predDf,
      scaling = "invalidOption", linScaleCV = 0.2)
  )
  expect_error(
    result <- ospsuite.parameteridentification:::.calculateCensoredContribution(
      observed = obsDf, simulated = predDf,
      scaling = "lin", logScaleSD = 0.086)
  )
  expect_error(
    result <- ospsuite.parameteridentification:::.calculateCensoredContribution(
      observed = obsDf, simulated = predDf,
      scaling = "log", linScaleCV = 0.2)
  )
})

## context(".applyLogTransformation")

obsVsPredDf <- readr::read_csv(getTestDataFilePath("Aciclovir_obsVsPredDf.csv"))

test_that("`.applyLogTransformation` correctly log-transforms yValues and lloq", {
  obsVsPredDfLog <- ospsuite.parameteridentification:::.applyLogTransformation(obsVsPredDf)
  expect_snapshot_value(obsVsPredDfLog, style = "serialize")
})

## context(.calculateHuberWeights)

test_that("`.calculateHuberWeights` calculates correct weights for standard residuals", {
  residuals <- c(-2, -1, 0, 1, 2)
  expected_weights <- c(1, 1, 1, 1, 1)
  expect_equal(
    ospsuite.parameteridentification:::.calculateHuberWeights(residuals),
    expected_weights,
    tolerance = 0.01
  )
})

test_that("`.calculateHuberWeights` returns empty vector for empty residuals", {
  residuals <- numeric(0)
  expect_equal(ospsuite.parameteridentification:::.calculateHuberWeights(residuals), logical(0))
})

## context(.calculateBisquareWeights)

test_that("`.calculateBisquareWeights` calculates correct weights for standard residuals", {
  residuals <- c(-2, -1, 0, 1, 2)
  expected_weights <- c(0.841, 0.959, 1, 0.959, 0.841)
  expect_equal(
    ospsuite.parameteridentification:::.calculateBisquareWeights(residuals),
    expected_weights,
    tolerance = 0.01
  )
})

test_that("`.calculateBisquareWeights` returns empty vector for empty residuals", {
  residuals <- numeric(0)
  expect_equal(ospsuite.parameteridentification:::.calculateHuberWeights(residuals), logical(0))
})

## context("calculateCostMetrics")

obsVsPredDf <- readr::read_csv(getTestDataFilePath("Aciclovir_obsVsPredDf.csv"))

test_that("'calculateCostMetrics' returns expected cost metrics for valid input data and default parameters", {
  result <- calculateCostMetrics(obsVsPredDf)
  expect_s3_class(result, "modelCost")
  expect_true(all(c("modelCost", "minLogProbability", "costVariables", "residualDetails") %in% names(result)))
})

test_that("`calculateCostMetrics` returns correct cost metric values for default parameters", {
  result <- calculateCostMetrics(obsVsPredDf)
  expect_snapshot_value(result, style = "serialize")
})

test_that("different residual weighting methods affect the residuals", {
  methods <- c("none", "std", "mean", "error")
  results <- lapply(methods, function(m) calculateCostMetrics(obsVsPredDf, residualWeightingMethod = m))
  expect_true(length(unique(sapply(results, function(x) x$modelCost))) == 4)
  expect_snapshot_value(results, style = "serialize")
})

test_that("robust methods (huber, bisquare) modify the residuals appropriately", {
  result_huber <- calculateCostMetrics(obsVsPredDf, robustMethod = "huber")
  result_bisquare <- calculateCostMetrics(obsVsPredDf, robustMethod = "bisquare")
  expect_true(result_huber$modelCost != result_bisquare$modelCost)
  expect_snapshot_value(list(result_huber, result_bisquare), style = "serialize")
})

test_that("least squares and M3 methods produce different model costs", {
  obsVsPredDf$lloq <- 2.5
  result_lsq <- calculateCostMetrics(obsVsPredDf, objectiveFunctionType = "lsq")
  result_m3 <- calculateCostMetrics(obsVsPredDf, objectiveFunctionType = "m3",
                                    scaling = "lin", linScaleCV = 0.2)
  expect_true(result_lsq$modelCost != result_m3$modelCost)
})

test_that("'calculateCostMetrics' correctly scales residuals when scaleVar is TRUE", {
  result_scaled <- calculateCostMetrics(obsVsPredDf, scaleVar = TRUE)
  result_unscaled <- calculateCostMetrics(obsVsPredDf, scaleVar = FALSE)
  expect_true(result_scaled$modelCost != result_unscaled$modelCost)
})

test_that("'calculateCostMetrics' handles infinite values in xValues and yValues correctly", {
  obsVsPredDfInf <- obsVsPredDf
  obsVsPredDfInf$xValues[1] <- Inf
  obsVsPredDfInf$yValues[1] <- -Inf
  expect_silent(calculateCostMetrics(obsVsPredDfInf))
})
