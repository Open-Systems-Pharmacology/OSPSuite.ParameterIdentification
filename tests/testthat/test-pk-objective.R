startValues <- function(task) {
  vapply(task$parameters, function(p) p$startValue, numeric(1))
}

freshSimulation <- function() {
  ospsuite::loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite"),
    loadFromCache = FALSE,
    addToCache = FALSE
  )
}

pkMapping <- function(simulation = testSimulation()) {
  PKOutputMapping$new(
    quantity = testQuantity(simulation),
    pkParameter = "C_max",
    targetValue = 30,
    targetUnit = testQuantity(simulation)$unit
  )
}

pkTask <- function(simulation = testSimulation(), iter = 2) {
  ParameterIdentification$new(
    simulations = simulation,
    parameters = testPKParameters(simulation),
    pkOutputMappings = pkMapping(simulation),
    configuration = lowIterPiConfiguration(iter)
  )
}

withPKOutputs <- function(task, code) {
  task$.__enclos_env__$private$.withPKOutputs(function() code)
}

pkObjective <- function(task) {
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  PKObjective$new(model = priv$.model, pkMappings = task$pkOutputMappings)
}

simulationOutputs <- function(simulation) {
  vapply(
    simulation$outputSelections$allOutputs,
    function(output) output$path,
    character(1)
  )
}

test_that("evaluate() returns the relative squared deviation sum as a modelCost", {
  task <- pkTask()
  objective <- pkObjective(task)
  values <- startValues(task)
  pkValue <- withPKOutputs(task, objective$pkValues(values))[[1]]
  target <- task$pkOutputMappings[[1]]$targetValueInBaseUnit
  cost <- withPKOutputs(task, objective$evaluate(values))
  expect_s3_class(cost, "modelCost")
  expect_identical(cost$modelCost, ((pkValue - target) / target)^2)
  expect_identical(cost$minLogProbability, cost$modelCost)
  expect_identical(cost$costVariables$nObservations, 1L)
  expect_identical(nrow(cost$residualDetails), 0L)
  expect_identical(withPKOutputs(task, objective$evaluate(values)), cost)
})

test_that("evaluate() logs a failed simulation and returns the error cost structure", {
  simulation <- freshSimulation()
  simulation$solver$mxStep <- 1
  task <- pkTask(simulation)
  objective <- pkObjective(task)
  priv <- task$.__enclos_env__$private
  expect_no_warning(
    suppressMessages(expect_message(
      cost <- objective$evaluate(startValues(task)),
      messages$simulationError(),
      fixed = TRUE
    ))
  )
  expect_identical(cost, .createErrorCostStructure())
  expect_identical(priv$.model$loggedFailures, 1)
})

test_that("evaluate() treats a PK value that is not finite as a failed evaluation", {
  task <- pkTask()
  objective <- pkObjective(task)
  priv <- task$.__enclos_env__$private
  local_mocked_bindings(
    calculatePKAnalyses = function(results) {
      list(pKParameterFor = function(quantityPath, pkParameter) {
        list(values = NA_real_)
      })
    },
    .package = "ospsuite"
  )
  logged <- character()
  cost <- withCallingHandlers(
    objective$evaluate(startValues(task)),
    message = function(m) {
      logged <<- c(logged, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_identical(cost, .createErrorCostStructure())
  expect_identical(priv$.model$loggedFailures, 1)
  expect_true(any(grepl(
    messages$errorPKValueNotFinite(
      "C_max",
      task$pkOutputMappings[[1]]$quantity$path
    ),
    logged,
    fixed = TRUE
  )))
  expect_false(any(grepl(messages$simulationError(), logged, fixed = TRUE)))
})

test_that("the PK outputs are selected around the analyses and restored", {
  task <- pkTask()
  simulation <- task$simulations[[1]]
  objective <- pkObjective(task)
  before <- simulationOutputs(simulation)
  seen <- list()
  original <- ospsuite::calculatePKAnalyses
  testthat::with_mocked_bindings(
    pkValues <- withPKOutputs(task, objective$pkValues(startValues(task))),
    calculatePKAnalyses = function(results) {
      seen[[length(seen) + 1]] <<- simulationOutputs(simulation)
      original(results)
    },
    .package = "ospsuite"
  )
  expect_length(pkValues, 1)
  expect_true(is.finite(pkValues[[1]]))
  expect_identical(seen, list(testQuantity(simulation)$path))
  expect_identical(simulationOutputs(simulation), before)
  testthat::with_mocked_bindings(
    expect_snapshot(
      withPKOutputs(task, objective$pkValues(startValues(task))),
      error = TRUE
    ),
    calculatePKAnalyses = function(results) stop("Injected PK failure"),
    .package = "ospsuite"
  )
  expect_identical(simulationOutputs(simulation), before)
})

test_that("PKObjective validates the model and the mappings", {
  task <- pkTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  expect_snapshot(
    PKObjective$new(model = list(1), pkMappings = task$pkOutputMappings),
    error = TRUE,
    transform = scrubCallerName
  )
  expect_snapshot(
    PKObjective$new(model = priv$.model, pkMappings = list(1)),
    error = TRUE,
    transform = scrubCallerName
  )
})

test_that("estimateCI() gives the PK mode error before building the model", {
  task <- pkTask()
  simulation <- task$simulations[[1]]
  before <- simulationOutputs(simulation)
  expect_snapshot(task$estimateCI(), error = TRUE)
  expect_null(task$.__enclos_env__$private$.model)
  expect_identical(simulationOutputs(simulation), before)
})

test_that("ParameterIdentification stops when a simulation has no parameter of the task in PK mode", {
  simulations <- list(freshSimulation(), freshSimulation())
  expect_snapshot(
    ParameterIdentification$new(
      simulations = simulations,
      parameters = testPKParameters(simulations[[1]]),
      pkOutputMappings = list(pkMapping(simulations[[1]]), pkMapping(simulations[[2]]))
    ),
    error = TRUE,
    transform = function(lines) gsub("[[:alnum:]_-]{22}", "<id>", lines)
  )
})

test_that(".evaluationClosure() in PK mode counts a failed evaluation and gives an infinite cost", {
  simulation <- freshSimulation()
  simulation$solver$mxStep <- 1
  task <- pkTask(simulation)
  priv <- task$.__enclos_env__$private
  expect_error(suppressMessages(task$run()), class = "initialSimulationError")
  cost <- suppressMessages(objectiveClosure(task)(startValues(task))$modelCost)
  expect_identical(cost, Inf)
  expect_identical(priv$.failedEvaluations, 1)
  expect_identical(priv$.fnEvaluations, 1)
})

test_that("run() in PK mode warns when most evaluations fail after a good start", {
  task <- pkTask()
  runBatches <- ospsuite::runSimulationBatches
  calls <- 0
  local_mocked_bindings(
    runSimulationBatches = function(simulationBatches, ...) {
      calls <<- calls + 1
      if (calls == 1) {
        return(runBatches(simulationBatches, ...))
      }
      lapply(simulationBatches, function(batch) list(NULL))
    },
    .package = "ospsuite"
  )
  expect_snapshot(result <- suppressMessages(task$run()))
  priv <- task$.__enclos_env__$private
  expect_gt(priv$.failedEvaluations, priv$.fnEvaluations / 2)
})

test_that("run() in PK mode reads the achieved PK values through the objective", {
  task <- pkTask()
  result <- suppressMessages(task$run())
  priv <- task$.__enclos_env__$private
  expect_s3_class(result, "PKResult")
  expect_s3_class(priv$.objective, "PKObjective")
  achieved <- withPKOutputs(
    task,
    priv$.objective$pkValues(priv$.lastOptimResult$par)
  )[[1]]
  expect_equal(
    result$toDataFrame()$achievedValue,
    ospsuite::toUnit(
      task$pkOutputMappings[[1]]$quantity,
      achieved,
      targetUnit = task$pkOutputMappings[[1]]$targetUnit
    )
  )
})
