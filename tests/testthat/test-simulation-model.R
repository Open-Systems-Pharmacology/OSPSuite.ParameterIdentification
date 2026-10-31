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
  # The state variable goes into the molecules bucket, the parameter into the
  # parameters bucket, both with their start values in base units
  expect_identical(
    model$variableParameters[[simId]],
    c("Aciclovir|Lipophilicity" = .toBaseValue(task$parameters[[2]], startValues(task)[[2]]))
  )
  expect_identical(
    model$variableMolecules[[simId]],
    stats::setNames(.toBaseValue(task$parameters[[1]], startValues(task)[[1]]), stateVariableParameterPath)
  )
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
  observed <- .prepareObservedData(task$outputMappings[[1]])
  simulated <- model$simulatedOutputs(values, task$outputMappings)
  # One output per mapping, a finite time course covering the observed times
  expect_length(simulated, 1)
  expect_length(simulated[[1]]$yValues, length(simulated[[1]]$xValues))
  expect_true(all(is.finite(simulated[[1]]$yValues)))
  expect_lte(min(simulated[[1]]$xValues), min(observed$xValues))
  expect_gte(max(simulated[[1]]$xValues), max(observed$xValues))
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

test_that("simulate() routes the state-variable value into the molecule bucket", {
  task <- testStateVariableMixedTask()
  model <- taskModel(task)
  simId <- names(task$simulations)[[1]]
  # The state-variable parameter comes first, the constant one second
  model$simulate(c(0.06, -0.1))
  expect_equal(model$variableMolecules[[simId]][[stateVariableParameterPath]], 0.06)
  expect_equal(model$variableParameters[[simId]][["Aciclovir|Lipophilicity"]], -0.1)
})

test_that("simulate() converts a non-base state-variable unit into the base unit", {
  sim <- loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite"),
    loadFromCache = FALSE,
    addToCache = FALSE
  )
  # The stomach liquid volume is defined by a right-hand side, so it goes into
  # the molecules bucket. Its base unit is l, so values in ml arrive divided
  # by 1000.
  piParameter <- PIParameters$new(
    parameters = list(getParameter(stateVariableParameterPath, container = sim))
  )
  piParameter$unit <- ospUnits$Volume$ml
  piParameter$startValue <- 45
  piParameter$maxValue <- 450
  piParameter$minValue <- 4.5
  mapping <- PIOutputMapping$new(quantity = testQuantity(sim))
  mapping$addObservedDataSets(testObservedData())
  task <- ParameterIdentification$new(
    simulations = sim,
    parameters = piParameter,
    outputMappings = mapping
  )
  model <- taskModel(task)
  simId <- names(task$simulations)[[1]]

  expect_equal(model$variableMolecules[[simId]][[stateVariableParameterPath]], 0.045)
  model$simulate(50)
  expect_equal(model$variableMolecules[[simId]][[stateVariableParameterPath]], 0.05)
  expect_false(stateVariableParameterPath %in% names(model$variableParameters[[simId]]))
})

test_that("simulate() seeds a grouped parameter of two simulations in base units", {
  clPath <- "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS-Aciclovir|TSspec"
  simulations <- replicate(
    2,
    loadSimulation(
      system.file("extdata", "Aciclovir.pkml", package = "ospsuite"),
      loadFromCache = FALSE,
      addToCache = FALSE
    ),
    simplify = FALSE
  )
  piParameter <- PIParameters$new(
    parameters = lapply(simulations, function(sim) getParameter(clPath, container = sim))
  )
  piParameter$unit <- ospUnits$`Inversed time`$`1/h`
  piParameter$startValue <- 6
  piParameter$maxValue <- 60
  piParameter$minValue <- 0.6
  task <- ParameterIdentification$new(
    simulations = simulations,
    parameters = piParameter,
    outputMappings = lapply(simulations, function(sim) {
      mapping <- PIOutputMapping$new(quantity = testQuantity(sim))
      mapping$addObservedDataSets(testObservedData())
      mapping
    })
  )
  model <- taskModel(task)
  simIds <- names(task$simulations)
  expect_length(simIds, 2)

  # 6 1/h is 0.1 1/min in both simulations, 30 1/h is 0.5 1/min
  for (simId in simIds) {
    expect_equal(model$variableParameters[[simId]][[clPath]], 0.1)
  }
  model$simulate(30)
  for (simId in simIds) {
    expect_equal(model$variableParameters[[simId]][[clPath]], 0.5)
  }
})

test_that("simulate() fills the buckets of grouped parameters by position", {
  clPath <- "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS-Aciclovir|TSspec"
  lipophilicityPath <- "Aciclovir|Lipophilicity"
  volumePath <- "Organism|Liver|Volume"
  simulations <- replicate(
    2,
    loadSimulation(
      system.file("extdata", "Aciclovir.pkml", package = "ospsuite"),
      loadFromCache = FALSE,
      addToCache = FALSE
    ),
    simplify = FALSE
  )
  group <- function(path, positions) {
    PIParameters$new(
      parameters = lapply(simulations[positions], function(sim) {
        getParameter(path, container = sim)
      })
    )
  }
  task <- ParameterIdentification$new(
    simulations = simulations,
    # Both simulations with the second one first, both simulations, a path of
    # the second simulation that the second group has too (the last group
    # wins), a state variable in both, the same path twice in one group
    parameters = list(
      group(clPath, 2:1),
      group(lipophilicityPath, 1:2),
      group(lipophilicityPath, 2),
      group(stateVariableParameterPath, 1:2),
      group(volumePath, c(1, 1))
    ),
    outputMappings = lapply(simulations, function(sim) {
      mapping <- PIOutputMapping$new(quantity = testQuantity(sim))
      mapping$addObservedDataSets(testObservedData())
      mapping
    })
  )
  model <- taskModel(task)
  ids <- vapply(simulations, function(sim) sim$root$id, character(1))
  values <- c(0.5, -0.2, 0.3, 0.05, 2.5)
  model$simulate(values)
  expected <- list(
    list(
      parameters = values[c(1, 2, 5)],
      parameterPaths = c(clPath, lipophilicityPath, volumePath)
    ),
    list(
      parameters = values[c(1, 3)],
      parameterPaths = c(clPath, lipophilicityPath)
    )
  )
  for (idx in 1:2) {
    simId <- ids[[idx]]
    batch <- model$simulationBatches[[simId]]
    expect_equal(
      model$variableParameters[[simId]],
      stats::setNames(expected[[idx]]$parameters, expected[[idx]]$parameterPaths)
    )
    expect_equal(
      model$variableMolecules[[simId]],
      stats::setNames(values[[4]], stateVariableParameterPath)
    )
    expect_identical(batch$getVariableParameters(), names(model$variableParameters[[simId]]))
    expect_identical(batch$getVariableMolecules(), names(model$variableMolecules[[simId]]))
  }
})

test_that("simulatedOutputs() delivers the state-variable initial value to the solver", {
  sim <- loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  )
  svQuantity <- getQuantity(stateVariableParameterPath, container = sim)
  # The output is the state variable itself, so its initial value is observable
  obs <- DataSet$new(name = "obs")
  obs$yDimension <- svQuantity$dimension
  obs$setValues(xValues = c(0, 1, 2), yValues = c(0.05, 0.05, 0.05))
  mapping <- PIOutputMapping$new(quantity = svQuantity)
  mapping$addObservedDataSets(obs)
  task <- ParameterIdentification$new(
    simulations = sim,
    parameters = stateVarPIParameter(sim),
    outputMappings = mapping
  )
  model <- taskModel(task)
  simulatedInitialValue <- function(startValue) {
    simulated <- model$simulatedOutputs(startValue, task$outputMappings)[[1]]
    simulated$yValues[which.min(simulated$xValues)]
  }
  expect_equal(simulatedInitialValue(0.02), 0.02, tolerance = 1e-4)
  expect_equal(simulatedInitialValue(0.09), 0.09, tolerance = 1e-4)
})
