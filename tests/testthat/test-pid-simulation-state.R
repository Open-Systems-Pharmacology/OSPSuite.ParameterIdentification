# ParameterIdentification - the output selections and the output schema of
# the simulations of the user are restored by every public method

# A new simulation whose output selections do not contain the mapped quantity
# and whose output time points do not contain the observed times
userStateSimulation <- function() {
  simulation <- loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite"),
    loadFromCache = FALSE
  )
  clearOutputs(simulation)
  addOutputs(
    "Organism|VenousBlood|Plasma|Aciclovir|Plasma Unbound",
    simulation
  )
  clearOutputIntervals(simulation)
  addOutputInterval(
    simulation,
    startTime = 0,
    endTime = 1440,
    resolution = 1 / 60,
    intervalName = "User interval"
  )
  simulation$outputSchema$addTimePoints(c(30, 90))
  simulation
}

# A new simulation whose only output is the mapped quantity, with the output
# intervals of the model file
mappedOutputSimulation <- function() {
  simulation <- loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite"),
    loadFromCache = FALSE
  )
  clearOutputs(simulation)
  addOutputs(testQuantity(simulation), simulation)
  simulation
}

simulationState <- function(simulation) {
  list(
    outputs = vapply(
      simulation$outputSelections$allOutputs,
      function(output) output$path,
      character(1)
    ),
    timePoints = simulation$outputSchema$timePoints,
    intervals = lapply(simulation$outputSchema$intervals, function(interval) {
      list(
        name = interval$name,
        startTime = interval$startTime$value,
        endTime = interval$endTime$value,
        resolution = interval$resolution$value
      )
    })
  )
}

stateTask <- function(
  simulation,
  autoEstimateCI = FALSE,
  parameters = testParameters(simulation)
) {
  configuration <- lowIterPiConfiguration()
  configuration$autoEstimateCI <- autoEstimateCI
  ParameterIdentification$new(
    simulations = simulation,
    parameters = parameters,
    outputMappings = testOutputMapping(simulation),
    configuration = configuration
  )
}

# Fails the cost calculation of every evaluation, after the batches are built
localFailingCost <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    .mappingCostTerms = function(...) stop("Injected cost failure"),
    .env = env
  )
}

quietly <- function(x) suppressMessages(suppressWarnings(x))

# A call of every public method of `ParameterIdentification` apart from
# `initialize()` and `print()`. The tests below check every one of them, and a
# test checks that no public method is missing here.
publicMethodCalls <- list(
  run = function(task) task$run(),
  estimateCI = function(task) task$estimateCI(),
  plotResults = function(task) task$plotResults(),
  gridSearch = function(task) task$gridSearch(totalEvaluations = 3),
  calculateOFVProfiles = function(task) {
    task$calculateOFVProfiles(totalEvaluations = 3L)
  }
)

# `estimateCI()` needs the result of `run()`, so `run()` is called first
taskReadyFor <- function(task, name) {
  if (name == "estimateCI") {
    quietly(task$run())
  }
  task
}

test_that("the tests call every public method", {
  # A new public method must be added to `publicMethodCalls`, so that the
  # tests check that it restores the simulations. A public method that does
  # not run the simulations can be added to the methods excluded here instead.
  expect_setequal(
    names(publicMethodCalls),
    setdiff(
      names(ParameterIdentification$public_methods),
      c("initialize", "print")
    )
  )
})

test_that(".batchInitialization() leaves the simulations unchanged", {
  # Every public method builds the batches with `.batchInitialization()`, so a
  # public method does not need to restore the simulations itself
  for (newSimulation in list(userStateSimulation, mappedOutputSimulation)) {
    simulation <- newSimulation()
    before <- simulationState(simulation)
    task <- stateTask(simulation)
    task$.__enclos_env__$private$.batchInitialization()
    expect_identical(simulationState(simulation), before)
  }
})

test_that("the simulations are in the state of the user when they run", {
  originalRunSimulationBatches <- ospsuite::runSimulationBatches
  bootstrapConfiguration <- bootstrapPiConfiguration()
  profileConfiguration <- lowIterPiConfiguration()
  profileConfiguration$ciMethod <- "PL"
  profileConfiguration$ciOptions <- list(maxIter = 2)
  configurations <- list(
    hessian = lowIterPiConfiguration(),
    bootstrap = bootstrapConfiguration,
    PL = profileConfiguration
  )
  # The profile likelihood fixes one parameter and optimizes the others, so
  # its task has a second parameter
  taskParameters <- function(simulation, ciMethod) {
    parameters <- testParameters(simulation)
    if (ciMethod == "PL") {
      fractionUnbound <- PIParameters$new(
        parameters = getParameter(
          "Aciclovir|Fraction unbound (plasma, reference value)",
          simulation
        )
      )
      fractionUnbound$minValue <- 0.5
      fractionUnbound$maxValue <- 1
      parameters <- c(parameters, fractionUnbound)
    }
    parameters
  }
  for (name in names(publicMethodCalls)) {
    for (ciMethod in names(configurations)) {
      # The confidence intervals are estimated only by `run()` with
      # `autoEstimateCI` and by `estimateCI()`
      if (ciMethod != "hessian" && !name %in% c("run", "estimateCI")) {
        next
      }
      simulation <- userStateSimulation()
      before <- simulationState(simulation)
      task <- stateTask(
        simulation,
        parameters = taskParameters(simulation, ciMethod)
      )
      task$configuration <- configurations[[ciMethod]]
      task$configuration$autoEstimateCI <- name == "run"
      task <- taskReadyFor(task, name)
      # The state of the simulation at every run of the batches
      states <- list()
      testthat::with_mocked_bindings(
        quietly(publicMethodCalls[[name]](task)),
        runSimulationBatches = function(...) {
          states[[length(states) + 1]] <<- simulationState(simulation)
          originalRunSimulationBatches(...)
        },
        .package = "ospsuite"
      )
      label <- paste(name, ciMethod)
      expect_identical(unique(states), list(before), label = label)
      expect_identical(simulationState(simulation), before, label = label)
    }
  }
})

test_that("an error while the batches are built restores every simulation", {
  originalCreateSimulationBatch <- ospsuite::createSimulationBatch
  for (name in names(publicMethodCalls)) {
    task <- testTwoSimulationsTask()
    task$configuration <- lowIterPiConfiguration()
    task$configuration$autoEstimateCI <- FALSE
    task <- taskReadyFor(task, name)
    before <- lapply(task$simulations, simulationState)
    # The batch of the first simulation is built, that of the second fails
    batches <- 0
    testthat::with_mocked_bindings(
      quietly(expect_error(
        publicMethodCalls[[name]](task),
        "Injected batch failure"
      )),
      createSimulationBatch = function(...) {
        batches <<- batches + 1
        if (batches == 2) {
          stop("Injected batch failure")
        }
        originalCreateSimulationBatch(...)
      },
      .package = "ospsuite"
    )
    expect_identical(lapply(task$simulations, simulationState), before)
  }
})

test_that("every public method restores the simulations as the first call", {
  # With the second simulation, only the output schema of the simulation
  # changes when it is prepared
  newSimulations <- list(
    userState = userStateSimulation,
    mappedOutput = mappedOutputSimulation
  )
  for (name in names(publicMethodCalls)) {
    for (simulationName in names(newSimulations)) {
      simulation <- newSimulations[[simulationName]]()
      before <- simulationState(simulation)
      task <- stateTask(simulation, autoEstimateCI = name == "run")
      # The first call of `estimateCI()` comes after that of `run()`
      task <- taskReadyFor(task, name)
      quietly(publicMethodCalls[[name]](task))
      expect_identical(
        simulationState(simulation),
        before,
        label = paste(name, simulationName)
      )
    }
  }
})

test_that(".restoreSimulationState() restores a changed output interval", {
  simulation <- userStateSimulation()
  before <- simulationState(simulation)
  savedState <- .storeSimulationState(simulation)
  resolution <- simulation$outputSchema$intervals[[1]]$resolution
  resolution$value <- 2 / 60
  .restoreSimulationState(simulation, savedState)
  expect_identical(simulationState(simulation), before)
})

test_that("every public method restores the simulations after run()", {
  simulation <- userStateSimulation()
  before <- simulationState(simulation)
  task <- stateTask(simulation)

  suppressMessages(task$run())
  expect_identical(simulationState(simulation), before)
  suppressMessages(suppressWarnings(task$estimateCI()))
  expect_identical(simulationState(simulation), before)
  suppressMessages(task$plotResults())
  expect_identical(simulationState(simulation), before)
  suppressMessages(task$gridSearch(totalEvaluations = 3))
  expect_identical(simulationState(simulation), before)
  suppressMessages(task$calculateOFVProfiles(totalEvaluations = 3L))
  expect_identical(simulationState(simulation), before)

  # A call that does not change the simulations leaves the output interval
  # objects of the user in them
  resolution <- simulation$outputSchema$intervals[[1]]$resolution
  suppressMessages(task$gridSearch(totalEvaluations = 3))
  resolution$value <- 2 / 60
  expect_equal(simulation$outputSchema$intervals[[1]]$resolution$value, 2 / 60)
})

test_that("run() with autoEstimateCI restores the simulations", {
  simulation <- userStateSimulation()
  before <- simulationState(simulation)
  task <- stateTask(simulation, autoEstimateCI = TRUE)

  suppressMessages(suppressWarnings(task$run()))
  expect_identical(simulationState(simulation), before)
  suppressMessages(task$plotResults())
  expect_identical(simulationState(simulation), before)
})

test_that("the simulations are restored after an error", {
  # An error after the batches are built, before the simulations run
  simulation <- userStateSimulation()
  before <- simulationState(simulation)
  task <- stateTask(simulation)
  expect_error(task$plotResults(par = c(1, 2)), "must supply one entry")
  expect_identical(simulationState(simulation), before)

  # A failed simulation stops run(), and then plotResults()
  simulation <- userStateSimulation()
  simulation$solver$mxStep <- 1
  before <- simulationState(simulation)
  task <- stateTask(simulation)
  suppressMessages(suppressWarnings(
    expect_error(task$run(), class = "initialSimulationError")
  ))
  expect_identical(simulationState(simulation), before)
  startValues <- vapply(task$parameters, function(p) p$startValue, numeric(1))
  suppressWarnings(
    expect_error(task$plotResults(par = startValues), "failed")
  )
  expect_identical(simulationState(simulation), before)

  # An error while the simulations are prepared
  for (name in c("plotResults", "run")) {
    simulation <- userStateSimulation()
    before <- simulationState(simulation)
    task <- stateTask(simulation)
    testthat::with_mocked_bindings(
      suppressMessages(expect_error(task[[name]](), "Injected unit failure")),
      toBaseUnit = function(...) stop("Injected unit failure"),
      .package = "ospsuite"
    )
    expect_identical(simulationState(simulation), before, label = name)
  }

  # An error after the batches are built: the bounds -10 and 10 have no
  # logarithm
  simulation <- userStateSimulation()
  before <- simulationState(simulation)
  task <- stateTask(simulation)
  expect_error(
    task$gridSearch(logScaleFlag = TRUE, totalEvaluations = 3),
    messages$logScaleFlagError(),
    fixed = TRUE
  )
  expect_identical(simulationState(simulation), before)

  # An error in the cost calculation of the first call on a task. The mock
  # of `localFailingCost()` is active until the end of this test.
  localFailingCost()
  calls <- publicMethodCalls[c("run", "gridSearch", "calculateOFVProfiles")]
  for (name in names(calls)) {
    simulation <- userStateSimulation()
    before <- simulationState(simulation)
    task <- stateTask(simulation)
    suppressMessages(
      expect_error(calls[[name]](task), "Injected cost failure")
    )
    expect_identical(simulationState(simulation), before, label = name)
  }
})

test_that("the simulations are restored with pkOutputMappings", {
  pkTask <- function(simulation) {
    ParameterIdentification$new(
      simulations = simulation,
      parameters = testPKParameters(simulation),
      pkOutputMappings = PKOutputMapping$new(
        quantity = testQuantity(simulation),
        pkParameter = "C_max",
        targetValue = 30,
        targetUnit = testQuantity(simulation)$unit
      ),
      configuration = lowIterPiConfiguration()
    )
  }
  quietRun <- function(task) suppressMessages(suppressWarnings(task$run()))

  # The outputs of the simulation do not contain the quantity of the PK
  # mapping
  simulation <- userStateSimulation()
  before <- simulationState(simulation)
  task <- pkTask(simulation)
  quietRun(task)
  expect_identical(simulationState(simulation), before)

  # `calculatePKAnalyses()` calculates the PK parameters of the outputs that
  # are selected in the simulation when it runs, so a second run() selects
  # the PK outputs again
  secondResult <- quietRun(task)
  expect_identical(simulationState(simulation), before)
  expect_identical(
    secondResult$toDataFrame(),
    quietRun(pkTask(userStateSimulation()))$toDataFrame()
  )

  # The other public methods stop before they change the simulations
  for (name in setdiff(names(publicMethodCalls), "run")) {
    expect_error(
      publicMethodCalls[[name]](task),
      "not applicable for PK metric optimization"
    )
    expect_identical(simulationState(simulation), before, label = name)
  }

  # `calculatePKAnalyses()` sees the quantity of the PK mapping as the only
  # output at every evaluation
  pkOutputs <- list()
  originalCalculatePKAnalyses <- ospsuite::calculatePKAnalyses
  testthat::with_mocked_bindings(
    quietRun(task),
    calculatePKAnalyses = function(results) {
      pkOutputs[[length(pkOutputs) + 1]] <<- simulationState(simulation)$outputs
      originalCalculatePKAnalyses(results)
    },
    .package = "ospsuite"
  )
  expect_gt(length(pkOutputs), 0)
  expect_identical(unique(pkOutputs), list(testQuantity(simulation)$path))
  expect_identical(simulationState(simulation), before)

  # An error after the batches are built
  testthat::with_mocked_bindings(
    expect_error(quietRun(task), "Injected PK failure"),
    calculatePKAnalyses = function(results) stop("Injected PK failure"),
    .package = "ospsuite"
  )
  expect_identical(simulationState(simulation), before)

  # A PK task on a simulation that another task used before gives the result
  # of a PK task on a new simulation
  simulation <- userStateSimulation()
  suppressMessages(stateTask(simulation)$plotResults())
  expect_identical(
    quietRun(pkTask(simulation))$toDataFrame(),
    quietRun(pkTask(userStateSimulation()))$toDataFrame()
  )
})

test_that("two tasks with one simulation both restore it", {
  simulation <- userStateSimulation()
  before <- simulationState(simulation)
  firstTask <- stateTask(simulation)
  secondTask <- stateTask(simulation)

  suppressMessages(firstTask$plotResults())
  expect_identical(simulationState(simulation), before)
  suppressMessages(secondTask$gridSearch(totalEvaluations = 3))
  expect_identical(simulationState(simulation), before)
  suppressMessages(firstTask$gridSearch(totalEvaluations = 3))
  expect_identical(simulationState(simulation), before)
})

test_that("results do not depend on the state of the simulations", {
  # Later calls start from the simulation in the state of the user again. They
  # give the results of a new task.
  simulation <- userStateSimulation()
  task <- stateTask(simulation)
  suppressMessages(task$run())
  par <- vapply(task$parameters, function(p) p$currValue, numeric(1))
  grid <- suppressMessages(task$gridSearch(totalEvaluations = 3))
  profiles <- suppressMessages(
    task$calculateOFVProfiles(par = par, totalEvaluations = 3L)
  )
  plots <- suppressMessages(task$plotResults(par = par))

  newTask <- function() stateTask(userStateSimulation())
  expect_identical(
    suppressMessages(newTask()$gridSearch(totalEvaluations = 3)),
    grid
  )
  # A call that stopped with an error after it built the batches
  stoppedTask <- newTask()
  expect_error(stoppedTask$plotResults(par = c(1, 2)), "must supply one entry")
  expect_identical(
    suppressMessages(stoppedTask$gridSearch(totalEvaluations = 3)),
    grid
  )
  expect_identical(
    suppressMessages(
      newTask()$calculateOFVProfiles(par = par, totalEvaluations = 3L)
    ),
    profiles
  )
  newPlots <- suppressMessages(newTask()$plotResults(par = par))
  expect_identical(newPlots[[1]]$data, plots[[1]]$data)
  expect_identical(
    lapply(newPlots[[1]]$patches$plots, function(p) p$data),
    lapply(plots[[1]]$patches$plots, function(p) p$data)
  )

  # run() with autoEstimateCI estimates the confidence intervals with the
  # batches of the optimization, a later estimateCI() with new batches
  autoTask <- stateTask(userStateSimulation(), autoEstimateCI = TRUE)
  autoResult <- suppressMessages(suppressWarnings(autoTask$run()))
  laterTask <- stateTask(userStateSimulation())
  suppressMessages(laterTask$run())
  laterResult <- suppressMessages(suppressWarnings(laterTask$estimateCI()))
  expect_identical(laterResult$toDataFrame(), autoResult$toDataFrame())
})

# Observed concentrations in min, after the last output time point of
# Aciclovir.pkml at 1440 min
lateData <- function() {
  dataSet <- DataSet$new(name = "late")
  dataSet$xUnit <- "min"
  dataSet$yDimension <- ospDimensions$`Concentration (mass)`
  dataSet$yUnit <- "mg/l"
  dataSet$setValues(xValues = c(97, 1800), yValues = c(3, 0.01))
  dataSet
}

# Changes of a simulation or of the observed times of a task
simulationChanges <- list(
  none = function(task, simulation) NULL,
  dose = function(task, simulation) {
    dose <- getParameter(testDosePath, simulation)
    setParameterValues(dose, 2 * dose$value)
  },
  solver = function(task, simulation) simulation$solver$relTol <- 1e-2,
  dataSet = function(task, simulation) {
    task$outputMappings[[1]]$addObservedDataSets(lateData())
  },
  xOffset = function(task, simulation) {
    task$outputMappings[[1]]$setDataTransformations(xOffsets = 0.25)
  },
  xFactor = function(task, simulation) {
    task$outputMappings[[1]]$setDataTransformations(xFactors = 1.25)
  }
)

allSimulationChanges <- function(task, simulation) {
  for (change in simulationChanges[c("dose", "solver", "dataSet")]) {
    change(task, simulation)
  }
  task$outputMappings[[1]]$setDataTransformations(
    xOffsets = 0.25,
    xFactors = 1.25
  )
}

# A `PIResult` as a list, without its run times
timelessResult <- function(result) {
  result <- result$toList()
  result[setdiff(names(result), c("elapsed", "ciElapsed"))]
}

# The result of `call` on a task whose first call was before `change`
resultAfterFirstCall <- function(change, call, firstCall = NULL) {
  simulation <- userStateSimulation()
  task <- stateTask(simulation)
  firstCall <- firstCall %||%
    function(task) task$gridSearch(totalEvaluations = 1)
  quietly(firstCall(task))
  change(task, simulation)
  call(task)
}

# The result of `call` on a new task with `change`
resultOfNewTask <- function(change, call) {
  simulation <- userStateSimulation()
  task <- stateTask(simulation)
  change(task, simulation)
  call(task)
}

test_that("a change after the first call gives the results of a new task", {
  gridSearch <- function(task) quietly(task$gridSearch(totalEvaluations = 2))
  results <- list()
  for (name in names(simulationChanges)) {
    results[[name]] <- resultOfNewTask(simulationChanges[[name]], gridSearch)
    expect_true(all(is.finite(results[[name]]$ofv)), info = name)
    expect_identical(
      resultAfterFirstCall(simulationChanges[[name]], gridSearch),
      results[[name]],
      info = name
    )
  }
  for (name in setdiff(names(simulationChanges), "none")) {
    expect_false(identical(results[[name]], results$none), info = name)
  }
})

test_that("every public method uses the changes after the first call", {
  calls <- list(
    calculateOFVProfiles = function(task) {
      quietly(task$calculateOFVProfiles(totalEvaluations = 2L))
    },
    plotResults = function(task) {
      plots <- quietly(task$plotResults())
      c(
        list(plots[[1]]$data),
        lapply(plots[[1]]$patches$plots, function(p) p$data)
      )
    },
    run = function(task) timelessResult(quietly(task$run()))
  )
  results <- list()
  for (name in names(calls)) {
    results[[name]] <- resultOfNewTask(allSimulationChanges, calls[[name]])
    expect_identical(
      resultAfterFirstCall(allSimulationChanges, calls[[name]]),
      results[[name]],
      info = name
    )
  }
  expect_true(is.finite(results$run$objectiveValue))

  # The confidence intervals of `estimateCI()` after `run()` and a change of
  # the model. The observed data are read again by every call already.
  run <- function(task) quietly(task$run())
  estimateCI <- function(task) timelessResult(quietly(task$estimateCI()))
  modelChanges <- function(task, simulation) {
    simulationChanges$dose(task, simulation)
    simulationChanges$solver(task, simulation)
  }
  expect_false(identical(
    resultAfterFirstCall(modelChanges, estimateCI, firstCall = run),
    resultAfterFirstCall(simulationChanges$none, estimateCI, firstCall = run)
  ))
})

test_that("the first call keeps the interval objects of the user", {
  simulation <- userStateSimulation()
  interval <- simulation$outputSchema$intervals[[1]]
  task <- stateTask(simulation)
  suppressMessages(task$gridSearch(totalEvaluations = 3))
  interval$resolution$setValue(2 / 60)
  expect_equal(simulation$outputSchema$intervals[[1]]$resolution$value, 2 / 60)
})

test_that("run() with PK mappings keeps the interval objects of the user", {
  simulation <- userStateSimulation()
  before <- simulationState(simulation)
  interval <- simulation$outputSchema$intervals[[1]]
  task <- ParameterIdentification$new(
    simulations = simulation,
    parameters = testPKParameters(simulation),
    pkOutputMappings = PKOutputMapping$new(
      quantity = testQuantity(simulation),
      pkParameter = "C_max",
      targetValue = 30,
      targetUnit = testQuantity(simulation)$unit
    ),
    configuration = lowIterPiConfiguration()
  )
  for (i in 1:2) {
    suppressMessages(suppressWarnings(task$run()))
    expect_identical(simulationState(simulation), before)
  }
  interval$resolution$setValue(2 / 60)
  expect_equal(simulation$outputSchema$intervals[[1]]$resolution$value, 2 / 60)
})

test_that("intervals with the same name are restored", {
  simulation <- userStateSimulation()
  addOutputInterval(simulation, startTime = 0, endTime = 60, resolution = 1)
  intervals <- simulation$outputSchema$intervals
  intervals[[2]]$name <- intervals[[1]]$name
  before <- simulationState(simulation)
  task <- stateTask(simulation)
  # The batch cannot be built from such a simulation. Its error is not hidden
  # by the restore, and the intervals of the user survive.
  expect_error(suppressMessages(task$run()), "NotUniqueNameException")
  expect_identical(simulationState(simulation), before)
  intervals[[2]]$resolution$setValue(2)
  expect_equal(simulation$outputSchema$intervals[[2]]$resolution$value, 2)
})

# Makes the restore fail: only the restore adds the output of the user
userPath <- "Organism|VenousBlood|Plasma|Aciclovir|Plasma Unbound"

localFailingRestore <- function(env = parent.frame()) {
  originalAddOutputs <- ospsuite::addOutputs
  testthat::local_mocked_bindings(
    addOutputs = function(quantitiesOrPaths, simulation) {
      if (is.character(quantitiesOrPaths) && userPath %in% quantitiesOrPaths) {
        stop("Injected restore failure")
      }
      originalAddOutputs(quantitiesOrPaths, simulation)
    },
    .package = "ospsuite",
    .env = env
  )
}

test_that("run() returns its result and warns when the restore fails", {
  simulation <- userStateSimulation()
  task <- stateTask(simulation)
  localFailingRestore()
  expect_warning(
    result <- suppressMessages(task$run()),
    messages$warningSimulationStateNotRestored(
      simulation$name,
      "Injected restore failure"
    ),
    fixed = TRUE
  )
  expect_s3_class(result, "PIResult")
})

test_that("a method keeps its own error and warns when the restore fails", {
  simulation <- userStateSimulation()
  task <- stateTask(simulation)
  localFailingRestore()
  expect_warning(
    expect_error(
      task$plotResults(par = c(1, 2)),
      messages$errorParameterValuesLengthMismatch(length(task$parameters), 2),
      fixed = TRUE
    ),
    messages$warningSimulationStateNotRestored(
      simulation$name,
      "Injected restore failure"
    ),
    fixed = TRUE
  )
})

test_that("a failing restore of one simulation still restores the others", {
  first <- userStateSimulation()
  second <- userStateSimulation()
  secondBefore <- simulationState(second)
  savedState <- .storeSimulationState(list(first, second))
  for (simulation in list(first, second)) {
    clearOutputs(simulation)
    simulation$outputSchema$addTimePoints(c(5, 6))
  }
  originalAddOutputs <- ospsuite::addOutputs
  calls <- 0
  testthat::local_mocked_bindings(
    addOutputs = function(quantitiesOrPaths, simulation) {
      calls <<- calls + 1
      if (calls == 1) {
        stop("Injected restore failure")
      }
      originalAddOutputs(quantitiesOrPaths, simulation)
    },
    .package = "ospsuite"
  )
  expect_warning(
    .restoreSimulationState(list(first, second), savedState),
    messages$warningSimulationStateNotRestored(
      first$name,
      "Injected restore failure"
    ),
    fixed = TRUE
  )
  expect_identical(simulationState(second), secondBefore)
})
