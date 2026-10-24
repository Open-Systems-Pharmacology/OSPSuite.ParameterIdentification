startValues <- function(task) {
  vapply(task$parameters, function(p) p$startValue, numeric(1))
}

taskModel <- function(task) {
  SimulationModel$new(
    simulations = task$simulations,
    parameters = task$parameters,
    outputMappings = task$outputMappings
  )
}

outputState <- function(simulation) {
  list(
    outputs = vapply(
      simulation$outputSelections$allOutputs,
      function(output) output$path,
      character(1)
    ),
    timePoints = simulation$outputSchema$timePoints
  )
}

test_that("the model builds the batches and buckets of the task", {
  task <- testStateVariableMixedTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  model <- taskModel(task)
  simId <- names(task$simulations)[[1]]

  expect_identical(names(model$simulationBatches), names(task$simulations))
  expect_identical(model$variableParameters, priv$.variableParameters)
  expect_identical(model$variableMolecules, priv$.variableMolecules)
  expect_identical(
    model$parameterTargets,
    .resolveParameterTargets(task$parameters)
  )
  expect_identical(
    model$simulationBatches[[simId]]$getVariableParameters(),
    names(model$variableParameters[[simId]])
  )
  expect_identical(
    model$simulationBatches[[simId]]$getVariableMolecules(),
    names(model$variableMolecules[[simId]])
  )
})

test_that("simulatedOutputs() gives the simulated values of the task", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  model <- taskModel(task)
  values <- startValues(task)
  expect_identical(
    model$simulatedOutputs(values, task$outputMappings),
    priv$.simulateOutputs(values)
  )
  results <- model$simulate(values)
  expect_identical(names(results), names(task$simulations))
  expect_s3_class(results[[1]][[1]], "SimulationResults")
})

test_that("the constructor leaves the simulations unchanged", {
  task <- testPiTask()
  simulation <- task$simulations[[1]]
  before <- outputState(simulation)
  taskModel(task)
  expect_identical(outputState(simulation), before)
})

test_that("simulate() applies the values in base units and checks their length", {
  task <- testPiTask()
  model <- taskModel(task)
  simId <- names(task$simulations)[[1]]
  path <- task$parameters[[1]]$parameters[[1]]$path
  model$simulate(0.5)
  expect_identical(
    model$variableParameters[[simId]][[path]],
    .toBaseValue(task$parameters[[1]], 0.5)
  )
  expect_error(
    model$simulate(c(0.5, 0.5)),
    messages$errorParameterValuesLengthMismatch(1, 2),
    fixed = TRUE
  )
})

test_that("simulate() stops with the failed simulations and their reasons", {
  task <- testModifiedTask()
  model <- taskModel(task)
  simulationError <- expect_error(
    suppressWarnings(model$simulate(startValues(task))),
    class = "simulationsFailedError"
  )
  expect_identical(
    unname(simulationError$simulationNames),
    task$simulations[[1]]$name
  )
  expect_identical(simulationError$failed, 1L)
  expect_length(simulationError$reasons, 1)
  expect_identical(model$loggedFailures, 0)
})

# Simulated values that are NA up to `naUntil` min and 1 afterwards
mockSimulatedValues <- function(naUntil) {
  function(simulationResults, path, times) {
    list(
      xValues = times$xValues,
      yValues = ifelse(times$xValues <= naUntil, NA_real_, 1)
    )
  }
}

test_that("simulatedOutputs() fails when a used output is NA at an observed time", {
  task <- testPiTask()
  model <- taskModel(task)
  firstObservedTime <- min(.observedTimes(task$outputMappings[[1]]))
  expect_gt(firstObservedTime, 0)
  local_mocked_bindings(.simulatedValues = mockSimulatedValues(firstObservedTime))
  expect_snapshot(
    model$simulatedOutputs(startValues(task), task$outputMappings),
    error = TRUE
  )
  simulationError <- expect_error(
    model$simulatedOutputs(startValues(task), task$outputMappings),
    class = "simulationsFailedError"
  )
  expect_identical(simulationError$failed, 1L)
  expect_match(
    simulationError$reasons,
    task$outputMappings[[1]]$quantity$path,
    fixed = TRUE
  )
})

test_that("simulatedOutputs() tolerates NA values before the observed times", {
  task <- testPiTask()
  model <- taskModel(task)
  firstObservedTime <- min(.observedTimes(task$outputMappings[[1]]))
  local_mocked_bindings(
    .simulatedValues = mockSimulatedValues(firstObservedTime / 2)
  )
  simulated <- model$simulatedOutputs(startValues(task), task$outputMappings)
  expect_true(is.na(simulated[[1]]$yValues[[1]]))
  expect_identical(simulated[[1]]$yValues[[length(simulated[[1]]$yValues)]], 1)
})

test_that("simulatedOutputs() ignores observed times below zero", {
  task <- testPiTask()
  mapping <- task$outputMappings[[1]]
  lastTime <- max(unlist(lapply(mapping$observedDataSets, `[[`, "xValues")))
  # Every observed time is below 0 and dropped by `.prepareObservedData()`
  mapping$setDataTransformations(xOffsets = -(lastTime + 1))
  model <- taskModel(task)
  local_mocked_bindings(.simulatedValues = mockSimulatedValues(Inf))
  simulated <- model$simulatedOutputs(startValues(task), task$outputMappings)
  expect_true(all(is.na(simulated[[1]]$yValues)))
})

test_that(".observedTimes() applies the x transformations in min", {
  mapping <- testPiTask()$outputMappings[[1]]
  dataSet <- mapping$observedDataSets[[1]]
  hours <- ospsuite::toUnit(
    ospsuite::ospDimensions$Time,
    dataSet$xValues,
    targetUnit = "h",
    sourceUnit = dataSet$xUnit
  )
  before <- .observedTimes(mapping)
  mapping$setDataTransformations(xOffsets = 1, xFactors = 2)
  expect_identical(.observedTimes(mapping), (hours + 1) * 2 * 60)
  expect_identical(before, hours * 60)
})

test_that("logFailure() counts the failures and shortens repeated reasons", {
  model <- taskModel(testPiTask())
  cond <- .simulationsFailedError("A", 1L, "negative values at t=3\nAciclovir")
  first <- capture_messages(model$logFailure(1, cond))
  second <- capture_messages(model$logFailure(2, cond))
  expect_identical(model$loggedFailures, 2)
  expect_match(first[[2]], "Aciclovir", fixed = TRUE)
  expect_match(second[[2]], "[...]", fixed = TRUE)
  expect_false(grepl("Aciclovir", second[[2]], fixed = TRUE))
})

test_that("the constructor validates its inputs and the fields are read-only", {
  task <- testPiTask()
  expect_error(
    SimulationModel$new(list(1), task$parameters, task$outputMappings),
    messages$errorWrongType("simulations", "list", "Simulation"),
    fixed = TRUE
  )
  expect_error(
    SimulationModel$new(task$simulations, list(1), task$outputMappings),
    messages$errorWrongType("parameters", "list", "PIParameters"),
    fixed = TRUE
  )
  model <- taskModel(task)
  expect_snapshot(model$simulationBatches <- NULL, error = TRUE)
  expect_snapshot(model$loggedFailures <- 1, error = TRUE)
})
