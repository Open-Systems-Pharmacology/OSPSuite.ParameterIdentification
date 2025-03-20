# Simulate data
xVals <- seq(1, 10, length.out = 10)
trueParams <- c(5, 0.3, 0.5)

set.seed(2203)
yObs <- trueParams[1] * exp(-trueParams[2] * xVals) + trueParams[3] + rnorm(length(xVals), mean = 0, sd = 0.2)

# Function to simulate y from parameters
fnSimulate <- function(p) {
  ySim <- p[1] * exp(-p[2] * xVals) + p[3]
  return(ySim)
}

# Objective Function: sum of squared residuals (SSR)
fnObjective <- function(p) {
  ySim <- fnSimulate(p)
  SSR <- sum((yObs - ySim)^2)
  list(modelCost = SSR)
}

# Initial parameter values
parTest <- c(4.8, 0.3, 0.48)

# Bounds
lowerTest <- c(0, 0, 0)
upperTest <- c(10, 10, 1)

# Expected output fields
expectedFields <- c(
  "par", "value", "convergence", "iterations",
  "fnEvaluations", "algorithm", "elapsed"
)


test_that("Optimizer can be created and algorithm can be changed", {
  expect_silent(optimizer <- Optimizer$new("HJKB"))
  expect_s3_class(optimizer, "R6")
  expect_silent(optimizer$algorithm <- "BOBYQA")
})

test_that("Optimizer fails with unknown algorithm", {
  expect_error(Optimizer$new("strong"))
})

test_that("Optimizer fails when `fn` is not a function", {
  optimizer <- Optimizer$new("BOBYQA")
  expect_error(
    optimizer$run(par = parTest, fn = 1, lower = lowerTest, upper = upperTest),
    "expected 'function'"
  )
})

test_that("Optimizer can set and modify `modelCostField`", {
  optimizer <- Optimizer$new("BOBYQA", modelCostField = "result")
  expect_equal(optimizer$modelCostField, "result")
  optimizer$modelCostField <- "cost"
  expect_equal(optimizer$modelCostField, "cost")
})

test_that("Optimizer fails when objective function output lacks modelCost", {
  fnObjectiveWrong <- function(p) {
    ySim <- fnSimulate(p)
    SSR <- sum((yObs - ySim)^2)
    list(ssr = SSR)
  }

  optimizer <- Optimizer$new("BOBYQA")
  expect_error(
    optimizer$run(
      par = parTest, fn = fnObjectiveWrong, lower = lowerTest, upper = upperTest
    ),
    "Objective function must return a list containing 'modelCost'"
  )
})

test_that("Optimizer returns correct parameters for BOBYQA", {
  optimizer <- Optimizer$new("BOBYQA")
  optResult <- optimizer$run(
    par = parTest, fn = fnObjective, lower = lowerTest, upper = upperTest
  )
  expect_setequal(names(optResult), expectedFields)
  expect_equal(optResult$par, c(5.5106, 0.3841, 0.6838), tolerance = 1e-4)
  expect_equal(optResult$value, 0.4215, tolerance = 1e-4)
  expect_equal(optResult$iterations, 123)
  expect_equal(optResult$algorithm, "BOBYQA")
  expect_true(optResult$convergence)
})

test_that("Optimizer output has correct structure and values for HJKB", {
  optimizer <- Optimizer$new("HJKB")
  optResult <- optimizer$run(
    par = parTest, fn = fnObjective, lower = lowerTest, upper = upperTest
  )
  expect_setequal(names(optResult), expectedFields)
  expect_equal(optResult$par, c(5.5105, 0.3841, 0.6838), tolerance = 1e-4)
  expect_equal(optResult$value, 0.4215, tolerance = 1e-4)
  expect_equal(optResult$iterations, 19)
  expect_true(optResult$fnEvaluations <= 1e3)
  expect_equal(optResult$algorithm, "HJKB")
  expect_true(optResult$convergence)
})

test_that("Optimizer output has correct structure and values for DEoptim", {
  optimizer <- Optimizer$new("DEoptim")
  tmp <- capture.output(
    optResult <- optimizer$run(
      par = parTest, fn = fnObjective, lower = lowerTest, upper = upperTest
    )
  )
  expect_setequal(names(optResult), expectedFields)
  expect_equal(optResult$par, c(5.5106, 0.3841, 0.6838), tolerance = 1e-4)
  expect_equal(optResult$value, 0.4215, tolerance = 1e-4)
  expect_equal(optResult$iterations, 200)
  expect_equal(optResult$fnEvaluations, 6030)
  expect_equal(optResult$algorithm, "DEoptim")
  expect_true(optResult$convergence)
})

test_that("Optimizer works when objective function returns numeric cost", {
  fnObjectiveNumeric <- function(p) {
    ySim <- fnSimulate(p)
    sum((yObs - ySim)^2)
  }

  optimizer <- Optimizer$new("BOBYQA")
  expect_silent(optResult <- optimizer$run(
    par = parTest, fn = fnObjectiveNumeric, lower = lowerTest, upper = upperTest
  ))
  expect_type(optResult$value, "double")
})

test_that("Optimizer works with fixed parameter", {
  fixedPar <- list(idx = 1, values = 5)
  optimizer <- Optimizer$new("BOBYQA")
  optResult <- optimizer$run(
    par = parTest, fn = fnObjective, lower = lowerTest, upper = upperTest,
    fixedParams = fixedPar
  )
  expect_equal(optResult$par, c(5, 0.3393, 0.6392), tolerance = 1e-4)
})

test_that("Optimizer works with two fixed parameters", {
  fixedPar <- list(idx = c(1, 3), values = c(5, 0.68))
  optimizer <- Optimizer$new("BOBYQA")
  optResult <- optimizer$run(
    par = parTest, fn = fnObjective, lower = lowerTest, upper = upperTest,
    fixedParams = fixedPar
  )
  expect_equal(optResult$par, c(5, 0.349, 0.68), tolerance = 1e-4)
})

test_that("Optimizer fails when idx and values length mismatch", {
  fixedPar <- list(idx = c(1), values = c(5, 0.68))
  optimizer <- Optimizer$new("BOBYQA")
  expect_error(
    optimizer$run(
      par = parTest, fn = fnObjective, lower = lowerTest, upper = upperTest,
      fixedParams = fixedPar
    ),
    "`fixedParams.idx` and `fixedParams.values` must have the same length"
  )
})

test_that("Optimizer fails when idx is larger than parameter length", {
  fixedPar <- list(idx = c(1, 2, 3, 4), values = c(5, 0.3, 0.7, 1))
  optimizer <- Optimizer$new("BOBYQA")
  expect_error(
    optimizer$run(
      par = parTest, fn = fnObjective, lower = lowerTest, upper = upperTest,
      fixedParams = fixedPar
    ),
    "All parameters are fixed! Optimization requires at least one free parameter"
  )
})

test_that("It can print default optimizer", {
  optimizer <- Optimizer$new("BOBYQA")
  expect_snapshot(print(optimizer))
})
