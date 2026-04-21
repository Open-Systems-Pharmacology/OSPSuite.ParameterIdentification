# RDOutputMapping, ReverseDosimetry, RDResult

testRdOutputMapping <- function(simulation = testSimulation()) {
  RDOutputMapping$new(
    quantity = testQuantity(simulation),
    pkParameter = "C_max",
    targetValue = 1,
    targetUnit = testQuantity(simulation)$unit
  )
}

testRdTask <- function(simulation = testSimulation(), iter = 5) {
  ReverseDosimetry$new(
    simulation = simulation,
    parameters = testRdParameters(simulation),
    outputMappings = testRdOutputMapping(simulation),
    configuration = lowIterPiConfiguration(iter)
  )
}


# RDOutputMapping: construction

test_that("RDOutputMapping constructs correctly with valid inputs", {
  mapping <- testRdOutputMapping()

  expect_s3_class(mapping, "RDOutputMapping")
  expect_equal(mapping$pkParameter, "C_max")
  expect_equal(mapping$targetValue, 1)
  expect_true(is.numeric(mapping$targetValueInBaseUnit))
  expect_true(is.finite(mapping$targetValueInBaseUnit))
})

test_that("RDOutputMapping rejects invalid pkParameter", {
  expect_error(
    RDOutputMapping$new(
      quantity = testQuantity(),
      pkParameter = "not_a_real_pk_param",
      targetValue = 1,
      targetUnit = testQuantity()$unit
    )
  )
})

test_that("RDOutputMapping rejects non-numeric targetValue", {
  expect_error(
    RDOutputMapping$new(
      quantity = testQuantity(),
      pkParameter = "C_max",
      targetValue = "high",
      targetUnit = testQuantity()$unit
    )
  )
})

test_that("RDOutputMapping rejects incompatible targetUnit", {
  expect_error(
    RDOutputMapping$new(
      quantity = testQuantity(),
      pkParameter = "C_max",
      targetValue = 1,
      targetUnit = "kg"
    )
  )
})

test_that("RDOutputMapping rejects non-Quantity object", {
  expect_error(
    RDOutputMapping$new(
      quantity = "not_a_quantity",
      pkParameter = "C_max",
      targetValue = 1,
      targetUnit = "µmol/l"
    )
  )
})


# RDOutputMapping: read-only fields

test_that("RDOutputMapping read-only fields cannot be set", {
  mapping <- testRdOutputMapping()

  expect_error(mapping$quantity <- NULL, "is readonly")
  expect_error(mapping$simId <- NULL, "is readonly")
  expect_error(mapping$targetValueInBaseUnit <- 0, "is readonly")
})

test_that("RDOutputMapping mutable fields can be updated", {
  mapping <- testRdOutputMapping()

  mapping$pkParameter <- "AUC_tEnd"
  expect_equal(mapping$pkParameter, "AUC_tEnd")

  origBase <- mapping$targetValueInBaseUnit
  mapping$targetValue <- 5
  expect_equal(mapping$targetValue, 5)
  expect_false(isTRUE(all.equal(mapping$targetValueInBaseUnit, origBase)))
})


# RDOutputMapping: targetValueInBaseUnit

test_that("RDOutputMapping stores target in correct base units", {
  mapping <- testRdOutputMapping()
  quantity <- testQuantity()

  expected <- ospsuite::toBaseUnit(
    quantityOrDimension = quantity$dimension,
    values = 1,
    unit = quantity$unit
  )

  expect_equal(mapping$targetValueInBaseUnit, expected)
})


# RDOutputMapping: print

test_that("RDOutputMapping prints without error", {
  expect_no_error(print(testRdOutputMapping()))
})


# ReverseDosimetry: construction

test_that("ReverseDosimetry constructs with valid inputs", {
  rd <- testRdTask()

  expect_s3_class(rd, "ReverseDosimetry")
  expect_length(rd$outputMappings, 1)
})

test_that("ReverseDosimetry accepts a list of output mappings", {
  sim <- testSimulation()
  rd <- ReverseDosimetry$new(
    simulation = sim,
    parameters = testRdParameters(sim),
    outputMappings = list(testRdOutputMapping(sim), testRdOutputMapping(sim))
  )

  expect_length(rd$outputMappings, 2)
})

test_that("ReverseDosimetry rejects wrong simulation type", {
  expect_error(
    ReverseDosimetry$new(
      simulation = "not_a_simulation",
      parameters = testRdParameters(),
      outputMappings = testRdOutputMapping()
    )
  )
})

test_that("ReverseDosimetry rejects mapping from a different simulation", {
  otherSim <- loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  )

  expect_error(
    ReverseDosimetry$new(
      simulation = testSimulation(),
      parameters = testRdParameters(),
      outputMappings = testRdOutputMapping(otherSim)
    )
  )
})

test_that("ReverseDosimetry accepts a PIConfiguration", {
  sim <- testSimulation()
  config <- PIConfiguration$new()
  config$algorithm <- "HJKB"

  rd <- ReverseDosimetry$new(
    simulation = sim,
    parameters = testRdParameters(sim),
    outputMappings = testRdOutputMapping(sim),
    configuration = config
  )

  expect_equal(rd$configuration$algorithm, "HJKB")
})

test_that("ReverseDosimetry defaults to BOBYQA if no configuration given", {
  sim <- testSimulation()
  rd <- ReverseDosimetry$new(
    simulation = sim,
    parameters = testRdParameters(sim),
    outputMappings = testRdOutputMapping(sim)
  )

  expect_equal(rd$configuration$algorithm, "BOBYQA")
})

test_that("ReverseDosimetry configuration can be replaced", {
  rd <- testRdTask()

  config <- PIConfiguration$new()
  config$algorithm <- "HJKB"
  rd$configuration <- config

  expect_equal(rd$configuration$algorithm, "HJKB")
})


# ReverseDosimetry: print

test_that("ReverseDosimetry prints without error", {
  expect_no_error(print(testRdTask()))
})


# ReverseDosimetry: run()

test_that("run() returns an RDResult object", {
  suppressMessages(result <- testRdTask(iter = 5)$run())
  expect_s3_class(result, "RDResult")
})

test_that("run() restores simulation output state after completion", {
  sim <- testSimulation()
  rd <- testRdTask(simulation = sim, iter = 3)

  outputsBefore <- sim$outputSelections$allOutputs
  suppressMessages(rd$run())
  outputsAfter <- sim$outputSelections$allOutputs

  expect_equal(
    sapply(outputsAfter, `[[`, "path"),
    sapply(outputsBefore, `[[`, "path")
  )
})


# RDResult: structure

test_that("RDResult$toDataFrame() returns expected columns", {
  suppressMessages(result <- testRdTask(iter = 5)$run())

  df <- result$toDataFrame()
  expect_s3_class(df, "data.frame")
  expect_true(all(
    c(
      "quantityPath",
      "pkParameter",
      "targetValue",
      "targetUnit",
      "achievedValue",
      "estimatedValue",
      "parameterUnit"
    ) %in%
      names(df)
  ))
  expect_equal(nrow(df), 1)
})

test_that("RDResult$toList() contains expected fields", {
  suppressMessages(result <- testRdTask(iter = 3)$run())

  lst <- result$toList()
  expect_true(all(
    c(
      "estimatedValue",
      "unit",
      "objectiveValue",
      "convergence",
      "algorithm",
      "elapsed",
      "achievedPKValues",
      "mappings"
    ) %in%
      names(lst)
  ))
  expect_equal(lst$algorithm, "BOBYQA")
})

test_that("RDResult$estimateCI() raises not-implemented error", {
  suppressMessages(result <- testRdTask(iter = 3)$run())
  expect_error(result$estimateCI(), regexp = "not yet implemented")
})

test_that("RDResult prints without error", {
  suppressMessages(result <- testRdTask(iter = 3)$run())
  expect_no_error(print(result))
})


# ReverseDosimetry: non-dose parameter

test_that("ReverseDosimetry works with a non-dose parameter (Lipophilicity)", {
  sim <- loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  )
  lipoParam <- ospsuite::getParameter(
    "Aciclovir|Lipophilicity",
    container = sim
  )
  piParam <- PIParameters$new(parameters = list(lipoParam))
  piParam$minValue <- -5
  piParam$maxValue <- 5

  mapping <- RDOutputMapping$new(
    quantity = testQuantity(sim),
    pkParameter = "C_max",
    targetValue = 1,
    targetUnit = testQuantity(sim)$unit
  )

  rd <- ReverseDosimetry$new(
    simulation = sim,
    parameters = piParam,
    outputMappings = mapping,
    configuration = lowIterPiConfiguration(5)
  )

  suppressMessages(result <- rd$run())
  expect_s3_class(result, "RDResult")
  expect_true(is.finite(result$toList()$estimatedValue))
})
