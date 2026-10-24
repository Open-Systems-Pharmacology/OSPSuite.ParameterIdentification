# Simulate data
xVals <- seq(1, 10, length.out = 10)
trueParams <- c(5, 0.3, 0.5)

set.seed(2203)
yObs <- trueParams[1] *
  exp(-trueParams[2] * xVals) +
  trueParams[3] +
  rnorm(length(xVals), mean = 0, sd = 0.2)

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
  expect_error(
    Optimizer$new("notAConfig"),
    messages$errorWrongType("configuration", "character", "PIConfiguration"),
    fixed = TRUE
  )
})

test_that("Optimizer fails when `fn` is not a function", {
  piConfig <- PIConfiguration$new()
  optimizer <- Optimizer$new(piConfig)
  expect_error(
    optimizer$run(par = parTest, fn = 1, lower = lowerTest, upper = upperTest),
    messages$errorWrongType("fn", "numeric", "function"),
    fixed = TRUE
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
        par = parTest,
        fn = fnObjectiveWrong,
        lower = lowerTest,
        upper = upperTest
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
        par = parTest,
        fn = fnObjective,
        lower = lowerTest,
        upper = upperTest
      )
    ),
    messages$optimizationAlgorithm(piConfig$algorithm, par = parTest),
    fixed = TRUE
  )
  # NLopt reports SUCCESS or XTOL_REACHED depending on platform rounding
  expect_match(optResult$convergenceMessage, "^NLOPT_(SUCCESS|XTOL_REACHED):")
  optResult$convergenceMessage <- "NLOPT_SUCCESS: Generic success return value."
  optResult$elapsed <- 0L
  optResult$iterations <- 100L
  optResult$fnEvaluations <- 100L
  expect_snapshot_value(optResult, style = "deparse", tolerance = 1e-4)
})

test_that("BOBYQA stopped at maxeval is reported as not converged", {
  nCalls <- 0
  fnCounted <- function(p) {
    nCalls <<- nCalls + 1
    fnObjective(p)
  }
  piConfig <- PIConfiguration$new()
  piConfig$algorithmOptions <- list(maxeval = 3)
  optimizer <- Optimizer$new(piConfig)
  optResult <- suppressMessages(
    optimizer$run(
      par = parTest,
      fn = fnCounted,
      lower = lowerTest,
      upper = upperTest
    )
  )
  expect_identical(optResult$convergence, FALSE)
  expect_match(optResult$convergenceMessage, "NLOPT_MAXEVAL_REACHED")
  expect_equal(optResult$fnEvaluations, nCalls)
})

test_that("BOBYQA convergence is a single logical when nloptr fails", {
  piConfig <- PIConfiguration$new()
  optimizer <- Optimizer$new(piConfig)
  optResult <- optimizer$.__enclos_env__$private$.formatOptimizationOutput(
    list(
      par = parTest,
      value = Inf,
      convergence = -1L,
      message = "NLOPT_FAILURE: Generic failure code.",
      iter = 0L,
      nCalls = 2
    )
  )
  expect_identical(optResult$convergence, FALSE)
  expect_identical(
    optResult$convergenceMessage,
    "NLOPT_FAILURE: Generic failure code. The objective value is not finite."
  )
})

test_that("HJKB stopped at maxfeval reports why it stopped", {
  piConfig <- PIConfiguration$new()
  piConfig$algorithm <- "HJKB"
  piConfig$algorithmOptions <- list(maxfeval = 3)
  optimizer <- Optimizer$new(piConfig)
  optResult <- suppressWarnings(suppressMessages(
    optimizer$run(
      par = parTest,
      fn = fnObjective,
      lower = lowerTest,
      upper = upperTest
    )
  ))
  expect_identical(optResult$convergence, FALSE)
  expect_snapshot(cat(optResult$convergenceMessage))
})

test_that("HJKB reports the stop reason at its limits", {
  runHJKB <- function(options) {
    piConfig <- PIConfiguration$new()
    piConfig$algorithm <- "HJKB"
    piConfig$algorithmOptions <- options
    suppressWarnings(suppressMessages(
      Optimizer$new(piConfig)$run(
        par = parTest,
        fn = fnObjective,
        lower = lowerTest,
        upper = upperTest
      )
    ))
  }
  atMaxfeval <- runHJKB(list(maxfeval = 1))
  pastTarget <- runHJKB(list(target = 0))
  # with tol = 0.1 an unlimited run needs 37 evaluations, so its last step
  # overshoots maxfeval = 36
  overshotLastStep <- runHJKB(list(tol = 0.1, maxfeval = 36))
  expect_identical(atMaxfeval$convergence, FALSE)
  expect_identical(pastTarget$convergence, FALSE)
  expect_identical(overshotLastStep$convergence, FALSE)
  expect_snapshot({
    cat(atMaxfeval$convergenceMessage)
    cat(pastTarget$convergenceMessage)
    cat(overshotLastStep$convergenceMessage)
  })
})

test_that("DEoptim with a single generation is not converged", {
  piConfig <- PIConfiguration$new()
  piConfig$algorithm <- "DEoptim"
  piConfig$algorithmOptions <- list(itermax = 1, trace = FALSE)
  optResult <- suppressMessages(
    Optimizer$new(piConfig)$run(
      par = parTest,
      fn = fnObjective,
      lower = lowerTest,
      upper = upperTest
    )
  )
  expect_identical(optResult$convergence, FALSE)
})

test_that("A non-finite objective value is not converged and says why", {
  piConfig <- PIConfiguration$new()
  piConfig$algorithm <- "DEoptim"
  piConfig$algorithmOptions <- list(itermax = 2, trace = FALSE)
  optResult <- suppressMessages(
    Optimizer$new(piConfig)$run(
      par = parTest,
      fn = function(p) Inf,
      lower = lowerTest,
      upper = upperTest
    )
  )
  expect_identical(optResult$convergence, FALSE)
  expect_snapshot(cat(optResult$convergenceMessage))
})

test_that("DEoptim with a constant zero objective counts as converged", {
  piConfig <- PIConfiguration$new()
  piConfig$algorithm <- "DEoptim"
  piConfig$algorithmOptions <- list(itermax = 3, trace = FALSE)
  optResult <- suppressMessages(
    Optimizer$new(piConfig)$run(
      par = parTest,
      fn = function(p) 0,
      lower = lowerTest,
      upper = upperTest
    )
  )
  expect_identical(optResult$convergence, TRUE)
})

test_that("DEoptim reports generations used against itermax", {
  piConfig <- PIConfiguration$new()
  piConfig$algorithm <- "DEoptim"
  piConfig$algorithmOptions <- list(itermax = 3, trace = FALSE)
  optimizer <- Optimizer$new(piConfig)
  set.seed(2203)
  optResult <- suppressMessages(
    optimizer$run(
      par = parTest,
      fn = fnObjective,
      lower = lowerTest,
      upper = upperTest
    )
  )
  expect_snapshot(cat(optResult$convergenceMessage))
})

test_that("Optimizer output has correct structure and values for HJKB", {
  piConfig <- PIConfiguration$new()
  piConfig$algorithm <- "HJKB"
  optimizer <- Optimizer$new(piConfig)
  expect_message(
    expect_no_error(
      optResult <- optimizer$run(
        par = parTest,
        fn = fnObjective,
        lower = lowerTest,
        upper = upperTest
      )
    ),
    messages$optimizationAlgorithm(piConfig$algorithm, par = parTest),
    fixed = TRUE
  )
  optResult$elapsed <- 0
  optResult$fnEvaluations <- NA_real_
  expect_snapshot_value(optResult, style = "deparse", tolerance = 1e-4)
})

test_that("Optimizer output has correct structure and values for DEoptim", {
  piConfig <- PIConfiguration$new()
  piConfig$algorithm <- "DEoptim"
  optimizer <- Optimizer$new(piConfig)
  tmp <- utils::capture.output(
    expect_message(
      expect_no_error(
        optResult <- optimizer$run(
          par = parTest,
          fn = fnObjective,
          lower = lowerTest,
          upper = upperTest
        )
      ),
      messages$optimizationAlgorithm(piConfig$algorithm, par = parTest),
      fixed = TRUE
    )
  )
  optResult$elapsed <- 0
  optResult$fnEvaluations <- NA_real_
  expect_snapshot_value(optResult, style = "deparse", tolerance = 1e-4)
})

test_that(".deoptimInitialPopulation() puts the start values in the first row", {
  population <- .deoptimInitialPopulation(parTest, lowerTest, upperTest, NA)
  expect_identical(dim(population), c(30L, 3L))
  expect_identical(population[1, ], parTest)
  for (idx in seq_along(parTest)) {
    expect_true(all(population[, idx] >= lowerTest[idx]))
    expect_true(all(population[, idx] <= upperTest[idx]))
  }
  expect_identical(
    nrow(.deoptimInitialPopulation(parTest, lowerTest, upperTest, 8)),
    8L
  )
  expect_identical(
    nrow(.deoptimInitialPopulation(parTest, lowerTest, upperTest, NULL)),
    30L
  )
  # DEoptim truncates a fractional population size
  expect_identical(
    dim(.deoptimInitialPopulation(parTest, lowerTest, upperTest, 8.5)),
    c(8L, 3L)
  )
  # DEoptim replaces a population size below 4 with its default
  expect_identical(
    nrow(.deoptimInitialPopulation(parTest, lowerTest, upperTest, 3)),
    30L
  )
})

test_that("DEoptim starts from the start values unless initialpop is given", {
  passedControl <- NULL
  local_mocked_bindings(
    DEoptim = function(fn, lower, upper, control) {
      passedControl <<- control
      stop("captured")
    },
    .package = "DEoptim"
  )
  piConfig <- PIConfiguration$new()
  piConfig$algorithm <- "DEoptim"
  optimizer <- Optimizer$new(piConfig)
  runCaptured <- function() {
    try(
      suppressMessages(optimizer$run(
        par = parTest,
        fn = fnObjective,
        lower = lowerTest,
        upper = upperTest
      )),
      silent = TRUE
    )
  }

  runCaptured()
  expect_identical(passedControl$initialpop[1, ], parTest)
  expect_identical(passedControl$NP, 30L)

  userPopulation <- matrix(
    c(1, 2, 3, 4, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8),
    nrow = 4
  )
  piConfig$algorithmOptions <- list(NP = 4, initialpop = userPopulation)
  runCaptured()
  expect_identical(passedControl$initialpop, userPopulation)
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
        par = parTest,
        fn = fnObjectiveNumeric,
        lower = lowerTest,
        upper = upperTest
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
        par = parTest,
        fn = fnObjective,
        lower = lowerTest,
        upper = upperTest,
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
        par = parTest,
        fn = fnObjective,
        lower = lowerTest,
        upper = upperTest,
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
        par = parTest,
        fn = fnObjective,
        lower = lowerTest,
        upper = upperTest,
        fixedParams = fixedPar
      ),
      messages$fixedParamError(error = "length"),
      fixed = TRUE
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
        par = parTest,
        fn = fnObjective,
        lower = lowerTest,
        upper = upperTest,
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

# Quadratic function to simulate y-values
fnSimulate <- function(p) {
  p[1] * xVals^2 + p[2] * xVals + p[3]
}

# Generate noisy observed data
set.seed(2203)
yObs <- fnSimulate(trueParams) + rnorm(length(xVals), mean = 0, sd = 1)

# Objective Function: sum of squared residuals (SSR)
fnObjective <- function(p) {
  ySim <- fnSimulate(p)
  SSR <- sum((yObs - ySim)^2)

  list(
    modelCost = SSR,
    costVariables = list(
      weightedSSR = SSR,
      nObservations = length(yObs)
    )
  )
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
      par = parTest,
      fn = fnObjective,
      lower = lowerTest,
      upper = upperTest
    )
  )
  expect_message(
    expect_no_error(
      ciResult <- optimizer$estimateCI(
        par = optResult$par,
        fn = fnObjective,
        lower = lowerTest,
        upper = upperTest
      )
    ),
    messages$ciMethod(piConfig$ciMethod, optResult$par),
    fixed = TRUE
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
      par = parTest,
      fn = fnObjective,
      lower = lowerTest,
      upper = upperTest
    )
  )
  suppressMessages(
    expect_message(
      expect_no_error(
        ciResult <- optimizer$estimateCI(
          par = optResult$par,
          fn = fnObjective,
          lower = lowerTest,
          upper = upperTest
        )
      ),
      messages$ciMethod(piConfig$ciMethod, optResult$par),
      fixed = TRUE
    )
  )
  ciResult$elapsed <- NA_real_
  ciResult$details$paramHistory <- ciResult$details$paramHistory[1:5, ]
  expect_snapshot_value(ciResult, style = "deparse", tolerance = 1e-4)
})


# Replicate factory for bootstrap: each seed picks one observed data set
makeReplicateBootstrap <- function(yObsList) {
  force(yObsList)
  function(seed) {
    set.seed(seed)
    selectedData <- yObsList[[sample.int(length(yObsList), 1L)]]
    function(p) list(modelCost = sum((selectedData - fnSimulate(p))^2))
  }
}

test_that("Optimizer estimates confidence intervals using bootstrap method", {
  # Generate observed data for bootstrap
  set.seed(1000)
  nDatasets <- 100
  yObsList <- replicate(
    nDatasets,
    fnSimulate(trueParams) + stats::rnorm(length(xVals), sd = 1),
    simplify = FALSE
  )

  replicateBootstrap <- makeReplicateBootstrap(yObsList)

  piConfig <- PIConfiguration$new()
  piConfig$algorithm <- "HJKB"
  piConfig$ciMethod <- "bootstrap"
  ciOptions <- CIDefaults[["bootstrap"]]
  ciOptions$seed <- 1803
  ciOptions$nBootstrap <- 20
  piConfig$ciOptions <- ciOptions
  optimizer <- Optimizer$new(piConfig)

  suppressMessages(
    optResult <- optimizer$run(
      par = parTest,
      fn = fnObjective,
      lower = lowerTest,
      upper = upperTest
    )
  )
  suppressMessages(
    expect_message(
      expect_no_error(
        ciResult <- optimizer$estimateCI(
          par = optResult$par,
          fn = fnObjective,
          lower = lowerTest,
          upper = upperTest,
          replicate = replicateBootstrap
        )
      ),
      messages$ciMethod(piConfig$ciMethod, optResult$par),
      fixed = TRUE
    )
  )
  ciResult$elapsed <- NA_real_
  ciResult$details$bootstrapResults <- ciResult$details$bootstrapResults[1:5, ]
  expect_snapshot_value(ciResult, style = "deparse", tolerance = 1e-4)
})

test_that("Optimizer estimates bootstrap CI and returns one-sided bound", {
  # Use sparse data and small nBootstrap to induce skewed bootstrap CIs
  set.seed(1000)
  nDatasets <- 10
  yObsList <- replicate(
    nDatasets,
    fnSimulate(trueParams) + stats::rnorm(length(xVals), sd = 1),
    simplify = FALSE
  )

  replicateBootstrap <- makeReplicateBootstrap(yObsList)

  piConfig <- PIConfiguration$new()
  piConfig$algorithm <- "HJKB"
  piConfig$ciMethod <- "bootstrap"
  ciOptions <- CIDefaults[["bootstrap"]]
  ciOptions$seed <- 1803
  ciOptions$nBootstrap <- 10
  piConfig$ciOptions <- ciOptions
  optimizer <- Optimizer$new(piConfig)

  suppressMessages(
    optResult <- optimizer$run(
      par = parTest,
      fn = fnObjective,
      lower = lowerTest,
      upper = upperTest
    )
  )
  suppressMessages(
    expect_message(
      expect_no_error(
        ciResult <- optimizer$estimateCI(
          par = optResult$par,
          fn = fnObjective,
          lower = lowerTest,
          upper = upperTest,
          replicate = replicateBootstrap
        )
      ),
      messages$ciMethod(piConfig$ciMethod, optResult$par),
      fixed = TRUE
    )
  )
  expect_equal(ciResult$ciType, c("two-sided", "one-sided", "two-sided"))
  expect_equal(ciResult$lowerCI, c(0.971, NA, 2.659), tolerance = 0.01)
})

test_that("estimateCI() with the bootstrap method evaluates the function of replicate(seed) per replicate", {
  piConfig <- PIConfiguration$new()
  piConfig$ciMethod <- "bootstrap"
  piConfig$ciOptions <- list(seed = 1803L, nBootstrap = 3L)
  optimizer <- Optimizer$new(piConfig)
  set.seed(1000)
  yObsList <- replicate(
    3,
    fnSimulate(trueParams) + stats::rnorm(length(xVals), sd = 1),
    simplify = FALSE
  )
  replicateBootstrap <- makeReplicateBootstrap(yObsList)
  seeds <- integer()
  replicateFn <- function(seed) {
    seeds <<- c(seeds, seed)
    replicateBootstrap(seed)
  }
  suppressMessages(
    ciResult <- optimizer$estimateCI(
      par = parTest,
      fn = fnObjective,
      lower = lowerTest,
      upper = upperTest,
      replicate = replicateFn
    )
  )
  expect_identical(seeds, .withSeed(1803L, sample(1e6, 3L)))
  expect_equal(nrow(ciResult$details$bootstrapResults), 3L)
})

test_that("estimateCI() with the bootstrap method stops without replicate", {
  piConfig <- PIConfiguration$new()
  piConfig$ciMethod <- "bootstrap"
  optimizer <- Optimizer$new(piConfig)
  expect_snapshot(
    optimizer$estimateCI(par = parTest, fn = fnObjective, lower = lowerTest, upper = upperTest),
    error = TRUE
  )
  # The optimizer is left as it was found, so the next run prints its start
  expect_message(
    optimizer$run(par = parTest, fn = fnObjective, lower = lowerTest, upper = upperTest),
    messages$optimizationAlgorithm(piConfig$algorithm, parTest, FALSE),
    fixed = TRUE
  )
  expect_error(
    optimizer$estimateCI(
      par = parTest,
      fn = fnObjective,
      lower = lowerTest,
      upper = upperTest,
      replicate = 1
    ),
    messages$errorWrongType("replicate", "numeric", "function"),
    fixed = TRUE
  )
})


# Hessian CI method.args tests

test_that("Hessian CI uses r from ciOptions to control number of iterations", {
  piConfig <- PIConfiguration$new()
  piConfig$ciOptions <- list(r = 2L)
  optimizer <- Optimizer$new(piConfig)

  evalCount <- 0L
  fnCounting <- function(p) {
    evalCount <<- evalCount + 1L
    fnObjective(p)
  }

  suppressMessages(
    ciResult <- optimizer$estimateCI(
      par = parTest,
      fn = fnCounting,
      lower = lowerTest,
      upper = upperTest
    )
  )

  # r=2 with N=3: 26 hessian evals + 1 cost eval = 27 total
  # (default r=4: 50 hessian evals + 1 cost eval = 51 total)
  expect_equal(evalCount, 27L)
  expect_null(ciResult$error)
})

test_that("Hessian CI stops when the objective function value has no cost variables", {
  optimizer <- Optimizer$new(PIConfiguration$new())
  evalCount <- 0L
  fnScalar <- function(p) {
    evalCount <<- evalCount + 1L
    fnObjective(p)$modelCost
  }
  expect_snapshot(
    optimizer$estimateCI(
      par = parTest,
      fn = fnScalar,
      lower = lowerTest,
      upper = upperTest
    ),
    error = TRUE
  )
  expect_equal(evalCount, 1L)
})

test_that("Hessian CI accepts cost variables given as a named vector", {
  optimizer <- Optimizer$new(PIConfiguration$new())
  fnVector <- function(p) {
    cost <- fnObjective(p)
    cost$costVariables <- unlist(cost$costVariables)
    cost
  }
  suppressMessages(
    ciResult <- optimizer$estimateCI(
      par = parTest,
      fn = fnVector,
      lower = lowerTest,
      upper = upperTest
    )
  )
  expect_null(ciResult$error)
  expect_true(all(is.finite(ciResult$sd)))
})
