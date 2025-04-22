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


test_that("Optimizer can be created and reflects current configuration", {
  piConfig <- PIConfiguration$new()
  expect_silent(optimizer <- Optimizer$new(piConfig))
  expect_equal(optimizer$algorithm, "BOBYQA")
  expect_s3_class(optimizer, "R6")

  piConfig$algorithm <- "HJKB"
  expect_equal(optimizer$algorithm, "HJKB")

  piConfig$algorithm <- "DEoptim"
  expect_equal(optimizer$algorithm, "DEoptim")
})

test_that("Optimizer reflects the CI method from configuration", {
  piConfig <- PIConfiguration$new()
  optimizer <- Optimizer$new(piConfig)
  expect_equal(optimizer$ciMethod, "hessian")

  piConfig$ciMethod <- "PL"
  expect_equal(optimizer$ciMethod, "PL")

  piConfig$ciMethod <- "bootstrap"
  expect_equal(optimizer$ciMethod, "bootstrap")
})

test_that("Optimizer read-only fields cannot be modified directly", {
  piConfig <- PIConfiguration$new()
  optimizer <- Optimizer$new(piConfig)

  expect_error(optimizer$algorithm <- "DEoptim", "readonly")
  expect_error(optimizer$ciMethod <- "PL", "readonly")
  expect_error(optimizer$modelCostField <- "modelCost", "readonly")
})

test_that("Optimizer creation fails if configuration is invalid", {
  expect_error(Optimizer$new("notAConfig"), "expected 'PIConfiguration'")
})

test_that("Optimizer fails when `fn` is not a function", {
  piConfig <- PIConfiguration$new()
  optimizer <- Optimizer$new(piConfig)
  expect_error(
    optimizer$run(par = parTest, fn = 1, lower = lowerTest, upper = upperTest),
    "expected 'function'"
  )
})

test_that("Optimizer fails when objective function output lacks modelCost", {
  fnObjectiveWrong <- function(p) {
    ySim <- fnSimulate(p)
    SSR <- sum((yObs - ySim)^2)
    list(ssr = SSR)
  }

  piConfig <- PIConfiguration$new()
  optimizer <- Optimizer$new(piConfig)
  suppressMessages(
    expect_error(
      optimizer$run(
        par = parTest, fn = fnObjectiveWrong, lower = lowerTest, upper = upperTest
      ),
      messages$objectiveFnOutputError("modelCost")
    )
  )
})

test_that("It can print default optimizer", {
  piConfig <- PIConfiguration$new()
  optimizer <- Optimizer$new(piConfig)
  expect_snapshot(print(optimizer))
})

test_that("Optimizer returns correct parameters for BOBYQA", {
  piConfig <- PIConfiguration$new()
  optimizer <- Optimizer$new(piConfig)
  expect_message(
    expect_no_error(
      optResult <- optimizer$run(
        par = parTest, fn = fnObjective, lower = lowerTest, upper = upperTest
      )
    ), messages$optimizationAlgorithm(piConfig$algorithm)
  )
  optResult$elapsed <- 0
  expect_snapshot_value(optResult, style = "deparse", tolerance = 1e-4)
})

test_that("Optimizer output has correct structure and values for HJKB", {
  piConfig <- PIConfiguration$new()
  piConfig$algorithm <- "HJKB"
  optimizer <- Optimizer$new(piConfig)
  expect_message(
    expect_no_error(
      optResult <- optimizer$run(
        par = parTest, fn = fnObjective, lower = lowerTest, upper = upperTest
      )
    ), messages$optimizationAlgorithm(piConfig$algorithm)
  )
  optResult$elapsed <- 0
  optResult$fnEvaluations <- NA_real_
  expect_snapshot_value(optResult, style = "deparse", tolerance = 1e-4)
})

test_that("Optimizer output has correct structure and values for DEoptim", {
  piConfig <- PIConfiguration$new()
  piConfig$algorithm <- "DEoptim"
  optimizer <- Optimizer$new(piConfig)
  tmp <- capture.output(
    expect_message(
      expect_no_error(
        optResult <- optimizer$run(
          par = parTest, fn = fnObjective, lower = lowerTest, upper = upperTest
        )
      ), messages$optimizationAlgorithm(piConfig$algorithm)
    )
  )
  optResult$elapsed <- 0
  optResult$fnEvaluations <- NA_real_
  expect_snapshot_value(optResult, style = "deparse", tolerance = 1e-4)
})

test_that("Optimizer works when objective function returns numeric cost", {
  fnObjectiveNumeric <- function(p) {
    ySim <- fnSimulate(p)
    sum((yObs - ySim)^2)
  }

  piConfig <- PIConfiguration$new()
  optimizer <- Optimizer$new(piConfig)
  expect_message(
    expect_no_error(
      optResult <- optimizer$run(
        par = parTest, fn = fnObjectiveNumeric, lower = lowerTest, upper = upperTest
      )
    )
  )
  expect_type(optResult$value, "double")
})

test_that("Optimizer works with fixed parameter", {
  fixedPar <- list(idx = 1, values = 5)
  piConfig <- PIConfiguration$new()
  optimizer <- Optimizer$new(piConfig)
  suppressMessages(
    expect_no_error(
      optResult <- optimizer$run(
        par = parTest, fn = fnObjective, lower = lowerTest, upper = upperTest,
        fixedParams = fixedPar
      )
    )
  )
  expect_snapshot_value(optResult$par, style = "deparse", tolerance = 1e-4)
})

test_that("Optimizer works with two fixed parameters", {
  fixedPar <- list(idx = c(1, 3), values = c(5, 0.68))
  piConfig <- PIConfiguration$new()
  optimizer <- Optimizer$new(piConfig)
  suppressMessages(
    expect_no_error(
      optResult <- optimizer$run(
        par = parTest, fn = fnObjective, lower = lowerTest, upper = upperTest,
        fixedParams = fixedPar
      )
    )
  )
  expect_snapshot_value(optResult$par, style = "deparse", tolerance = 1e-4)
})

test_that("Optimizer fails when idx and values length mismatch", {
  fixedPar <- list(idx = c(1), values = c(5, 0.68))
  piConfig <- PIConfiguration$new()
  optimizer <- Optimizer$new(piConfig)
  suppressMessages(
    expect_error(
      optimizer$run(
        par = parTest, fn = fnObjective, lower = lowerTest, upper = upperTest,
        fixedParams = fixedPar
      ),
      messages$fixedParamError(error = "length")
    )
  )
})

test_that("Optimizer fails when idx is larger than parameter length", {
  fixedPar <- list(idx = c(1, 2, 3, 4), values = c(5, 0.3, 0.7, 1))
  piConfig <- PIConfiguration$new()
  optimizer <- Optimizer$new(piConfig)
  suppressMessages(
    expect_error(
      optimizer$run(
        par = parTest, fn = fnObjective, lower = lowerTest, upper = upperTest,
        fixedParams = fixedPar
      ),
      messages$fixedParamError(error = "fixed")
    )
  )
})


# estimateCI

# Simulate data
xVals <- seq(-5, 5, length.out = 20)
trueParams <- c(1, -2, 3)

set.seed(2203)
# Quadratic function to simulate y-values
fnSimulate <- function(p) {
  p[1] * xVals^2 + p[2] * xVals + p[3]
}

# Generate noisy observed data
yObs <- fnSimulate(trueParams) + rnorm(length(xVals), mean = 0, sd = 1)

# Objective Function: sum of squared residuals (SSR)
fnObjective <- function(p) {
  ySim <- fnSimulate(p)
  SSR <- sum((yObs - ySim)^2)
  list(modelCost = SSR)
}

# Initial parameter values
parTest <- c(0.8, -1.5, 2.5)

# Parameter bounds
lowerTest <- c(-10, -10, -10)
upperTest <- c(10, 10, 10)


test_that("Optimizer estimates confidence intervals using Hessian", {
  piConfig <- PIConfiguration$new()
  piConfig$algorithm <- "HJKB"
  optimizer <- Optimizer$new(piConfig)
  suppressMessages(
    optResult <- optimizer$run(
      par = parTest, fn = fnObjective, lower = lowerTest, upper = upperTest
    )
  )
  expect_message(
    expect_no_error(
      ciResult <- optimizer$estimateCI(
        par = optResult$par, fn = fnObjective, lower = lowerTest, upper = upperTest
      )
    ),
    "Starting confidence interval estimation using 'hessian' method"
  )

  ciResult$elapsed <- NA_real_
  expect_snapshot_value(ciResult, style = "deparse", tolerance = 1e-4)
})

test_that("Optimizer estimates confidence intervals using profile likelihood method", {
  piConfig <- PIConfiguration$new()
  piConfig$algorithm <- "HJKB"
  piConfig$ciMethod <- "PL"
  optimizer <- Optimizer$new(piConfig)
  suppressMessages(
    optResult <- optimizer$run(
      par = parTest, fn = fnObjective, lower = lowerTest, upper = upperTest
    )
  )
  suppressMessages(
    expect_message(
      expect_no_error(
        ciResult <- optimizer$estimateCI(
          par = optResult$par, fn = fnObjective, lower = lowerTest, upper = upperTest
        )
      ),
      "Starting confidence interval estimation using 'PL' method"
    )
  )
  ciResult$elapsed <- NA_real_
  ciResult$details$paramHistory <- ciResult$details$paramHistory[1:5, ]
  expect_snapshot_value(ciResult, style = "deparse", tolerance = 1e-4)
})

# Generate bootstrap datasets (unique to bootstrap test)
nBootstrap <- 10
bootstrapSeeds <- sample(1e6, nBootstrap)
yObsList <- vector("list", nBootstrap)

for (i in seq_len(nBootstrap)) {
  noiseSeed <- 1000 + i
  set.seed(noiseSeed)
  yObsList[[i]] <- fnSimulate(trueParams) + rnorm(length(xVals), mean = 0, sd = 1)
}

# Objective Function for bootstrap: sum of squared residuals (SSR)
fnObjectiveBootstrap <- function(p, bootstrapSeed = NULL) {
  if (!is.null(bootstrapSeed)) {
    set.seed(bootstrapSeed)
    selectedData <- sample(yObsList, 1)[[1]]
  } else {
    yObsList[[1]]
  }

  ySim <- fnSimulate(p)
  SSR <- sum((selectedData - ySim)^2)
  list(modelCost = SSR)
}

test_that("Optimizer estimates confidence intervals using bootstrap method", {
  piConfig <- PIConfiguration$new()
  piConfig$algorithm <- "HJKB"
  piConfig$ciMethod <- "bootstrap"
  ciOptions <- CIDefaults[["bootstrap"]]
  ciOptions$seed <- 1803
  piConfig$ciOptions <- ciOptions
  optimizer <- Optimizer$new(piConfig)

  suppressMessages(
    optResult <- optimizer$run(
      par = parTest, fn = fnObjective, lower = lowerTest, upper = upperTest
    )
  )
  suppressMessages(
    expect_message(
      expect_no_error(
        ciResult <- optimizer$estimateCI(
          par = optResult$par, fn = fnObjectiveBootstrap, lower = lowerTest, upper = upperTest
        )
      ),
      "Starting confidence interval estimation using 'bootstrap' method"
    )
  )
  ciResult$elapsed <- NA_real_
  ciResult$details$bootstrapResults <- ciResult$details$bootstrapResults[1:5, ]
  expect_snapshot_value(ciResult, style = "deparse", tolerance = 1e-4)
})
