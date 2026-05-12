# PKResult class

mockPKOptimResult <- function(nPar = 1) {
  list(
    par = rep(0.0005, nPar),
    value = 12.5,
    startValues = rep(0.001, nPar),
    convergence = TRUE,
    iterations = 5,
    fnEvaluations = 12,
    algorithm = "BOBYQA",
    elapsed = 2.3
  )
}

sim <- testSimulation()
q <- testQuantity(sim)

pkMapping1 <- PKOutputMapping$new(
  quantity = q,
  pkParameter = "C_max",
  targetValue = 30,
  targetUnit = q$unit
)

pkMapping2 <- PKOutputMapping$new(
  quantity = q,
  pkParameter = "C_max",
  targetValue = 50,
  targetUnit = q$unit
)

test_that("PKResult can be initialized without error", {
  expect_no_error(
    PKResult$new(
      optimResult = mockPKOptimResult(),
      pkMappings = list(pkMapping1),
      achievedPKValues = list(28.5)
    )
  )
})

test_that("PKResult$toDataFrame() returns correct 7-column schema for single mapping", {
  result <- PKResult$new(
    optimResult = mockPKOptimResult(),
    pkMappings = list(pkMapping1),
    achievedPKValues = list(28.5)
  )
  df <- result$toDataFrame()
  expect_equal(
    names(df),
    c(
      "quantityPath",
      "pkParameter",
      "targetValue",
      "targetUnit",
      "achievedValue",
      "estimatedValue",
      "parameterUnit"
    )
  )
  expect_equal(nrow(df), 1L)
  expect_equal(df$pkParameter, "C_max")
  expect_equal(df$targetValue, 30)
  expect_equal(df$achievedValue, 28.5)
  expect_equal(df$estimatedValue, 0.0005)
  expect_true(is.na(df$parameterUnit))
})

test_that("PKResult$toDataFrame() repeats estimatedValue for multiple mappings", {
  result <- PKResult$new(
    optimResult = mockPKOptimResult(),
    pkMappings = list(pkMapping1, pkMapping2),
    achievedPKValues = list(28.5, 48.1)
  )
  df <- result$toDataFrame()
  expect_equal(nrow(df), 2L)
  expect_equal(df$estimatedValue, c(0.0005, 0.0005))
  expect_equal(df$achievedValue, c(28.5, 48.1))
})

test_that("PKResult$toDataFrame() achievedValue is converted to targetUnit display units", {
  sim <- testSimulation()
  q <- testQuantity(sim)
  mapping <- PKOutputMapping$new(
    quantity = q,
    pkParameter = "C_max",
    targetValue = 500,
    targetUnit = "nmol/l"
  )
  # 500 nmol/l = 0.5 µmol/l in base units; supply base-unit value as achievedPKValues
  result <- PKResult$new(
    optimResult = mockPKOptimResult(),
    pkMappings = list(mapping),
    achievedPKValues = list(0.5)
  )
  df <- result$toDataFrame()
  expect_equal(df$achievedValue, 500)
})

test_that("PKResult$toDataFrame() propagates NA in achievedPKValues", {
  result <- PKResult$new(
    optimResult = mockPKOptimResult(),
    pkMappings = list(pkMapping1),
    achievedPKValues = list(NA_real_)
  )
  df <- result$toDataFrame()
  expect_true(is.na(df$achievedValue))
})

test_that("PKResult$toDataFrame() propagates NA alongside finite value in multi-mapping result", {
  result <- PKResult$new(
    optimResult = mockPKOptimResult(),
    pkMappings = list(pkMapping1, pkMapping2),
    achievedPKValues = list(NA_real_, 1.5)
  )
  df <- result$toDataFrame()
  expect_equal(nrow(df), 2L)
  expect_true(is.na(df$achievedValue[[1]]))
  expect_equal(df$achievedValue[[2]], 1.5)
})

test_that("PKResult$toDataFrame() errors when multiple parameters were optimized", {
  result <- PKResult$new(
    optimResult = mockPKOptimResult(nPar = 2),
    pkMappings = list(pkMapping1),
    achievedPKValues = list(28.5)
  )
  expect_error(
    result$toDataFrame(),
    regexp = messages$errorPKResultMultipleParameters(),
    fixed = TRUE
  )
})

test_that("PKResult$toDataFrame() includes parameterUnit when piParameters provided", {
  piParam <- testPKParameters(sim)
  result <- PKResult$new(
    optimResult = mockPKOptimResult(),
    piParameters = list(piParam),
    pkMappings = list(pkMapping1),
    achievedPKValues = list(28.5)
  )
  df <- result$toDataFrame()
  expect_false(is.na(df$parameterUnit))
})

test_that("PKResult$toList() contains expected fields", {
  result <- PKResult$new(
    optimResult = mockPKOptimResult(),
    pkMappings = list(pkMapping1),
    achievedPKValues = list(28.5)
  )
  lst <- result$toList()
  expect_equal(lst$finalParameters, 0.0005)
  expect_equal(length(lst$pkMappings), 1L)
  expect_equal(lst$pkMappings[[1]]$pkParameter, "C_max")
  expect_equal(lst$pkMappings[[1]]$targetValue, 30)
  expect_equal(lst$achievedPKValues, list(28.5))
})

test_that("PKResult$print() shows achieved value in targetUnit display units", {
  sim <- testSimulation()
  q <- testQuantity(sim)
  mapping <- PKOutputMapping$new(
    quantity = q,
    pkParameter = "C_max",
    targetValue = 500,
    targetUnit = "nmol/l"
  )
  result <- PKResult$new(
    optimResult = mockPKOptimResult(),
    pkMappings = list(mapping),
    achievedPKValues = list(0.45) # 0.45 µmol/l base units = 450 nmol/l
  )
  output <- capture.output(result$print())
  expect_true(any(grepl("Achieved = 450", output)))
})

test_that("PKResult$print() does not error", {
  result <- PKResult$new(
    optimResult = mockPKOptimResult(),
    pkMappings = list(pkMapping1),
    achievedPKValues = list(28.5)
  )
  expect_no_error(result$print())
})
