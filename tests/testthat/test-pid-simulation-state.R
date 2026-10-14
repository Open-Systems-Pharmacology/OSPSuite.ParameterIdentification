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

stateTask <- function(simulation, autoEstimateCI = FALSE) {
  configuration <- lowIterPiConfiguration()
  configuration$autoEstimateCI <- autoEstimateCI
  ParameterIdentification$new(
    simulations = simulation,
    parameters = testParameters(simulation),
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

test_that("every public method restores the simulations as the first call", {
  calls <- list(
    run = function(task) task$run(),
    plotResults = function(task) task$plotResults(),
    gridSearch = function(task) task$gridSearch(totalEvaluations = 3),
    calculateOFVProfiles = function(task) {
      task$calculateOFVProfiles(totalEvaluations = 3L)
    }
  )
  # With the second simulation, only the output schema of the simulation
  # changes when it is prepared
  newSimulations <- list(
    userState = userStateSimulation,
    mappedOutput = mappedOutputSimulation
  )
  for (name in names(calls)) {
    for (simulationName in names(newSimulations)) {
      simulation <- newSimulations[[simulationName]]()
      before <- simulationState(simulation)
      task <- stateTask(simulation, autoEstimateCI = TRUE)
      suppressMessages(suppressWarnings(calls[[name]](task)))
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
  # An error after the simulations are prepared, before they are run
  simulation <- userStateSimulation()
  before <- simulationState(simulation)
  task <- stateTask(simulation)
  expect_error(task$plotResults(par = c(1, 2)), "must supply one entry")
  expect_identical(simulationState(simulation), before)

  # A failed simulation stops run(), and then plotResults(), whose batches
  # exist already
  simulation <- userStateSimulation()
  simulation$solver$mxStep <- 1
  before <- simulationState(simulation)
  task <- stateTask(simulation)
  suppressMessages(suppressWarnings(
    expect_error(task$run(), messages$initialSimulationError(), fixed = TRUE)
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

  # estimateCI() needs the result of run(), after which the batches are
  # built. They are built again here, so that estimateCI() prepares the
  # simulations before its error.
  simulation <- userStateSimulation()
  before <- simulationState(simulation)
  task <- stateTask(simulation)
  suppressMessages(task$run())
  task$.__enclos_env__$private$.needBatchInitialization <- TRUE
  localFailingCost()
  suppressMessages(expect_error(task$estimateCI(), "Injected cost failure"))
  expect_identical(simulationState(simulation), before)

  # An error in the cost calculation of the first call on a task. The mock
  # of `localFailingCost()` above is active until the end of this test.
  calls <- list(
    run = function(task) task$run(),
    gridSearch = function(task) task$gridSearch(totalEvaluations = 3),
    calculateOFVProfiles = function(task) {
      task$calculateOFVProfiles(totalEvaluations = 3L)
    }
  )
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
  calls <- list(
    estimateCI = function() task$estimateCI(),
    plotResults = function() task$plotResults(),
    gridSearch = function() task$gridSearch(totalEvaluations = 3),
    calculateOFVProfiles = function() {
      task$calculateOFVProfiles(totalEvaluations = 3L)
    }
  )
  for (name in names(calls)) {
    expect_error(calls[[name]](), "not applicable for PK metric optimization")
    expect_identical(simulationState(simulation), before, label = name)
  }

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
  # Later calls run the batches of the first call while the simulation is in
  # the state of the user again. They give the results of a new task.
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
  # Batches that a call built but did not run, because it stopped with an
  # error, run first in the next call
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

  # run() with autoEstimateCI estimates the confidence intervals while the
  # simulation is prepared, a later estimateCI() while it is in the state of
  # the user
  autoTask <- stateTask(userStateSimulation(), autoEstimateCI = TRUE)
  autoResult <- suppressMessages(suppressWarnings(autoTask$run()))
  laterTask <- stateTask(userStateSimulation())
  suppressMessages(laterTask$run())
  laterResult <- suppressMessages(suppressWarnings(laterTask$estimateCI()))
  expect_identical(laterResult$toDataFrame(), autoResult$toDataFrame())
})
