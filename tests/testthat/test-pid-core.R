# ParameterIdentification - Core

resetTestFactories()

test_that("ParameterIdentification is created successfully", {
  piConfiguration <- PIConfiguration$new()
  expect_silent(
    piTask <- ParameterIdentification$new(
      simulations = testSimulation(),
      parameters = testParameters(),
      outputMappings = testOutputMapping(),
      configuration = piConfiguration
    )
  )
  expect_s3_class(piTask, class = c("ParameterIdentification", "R6"))
})

test_that("ParameterIdentification read-only fields cannot be modified", {
  piTask <- testPiTask()
  expect_error(piTask$simulations <- testSimulation())
  expect_error(piTask$parameters <- testParameters())
  expect_error(piTask$outputMappings <- testOutputMapping())
})

test_that("ParameterIdentification configuration can be modified without errors", {
  piTask <- testPiTask()
  expect_no_error(piTask$configuration$algorithm <- "HJKB")
  expect_equal(piTask$configuration$algorithm, "HJKB")
  expect_no_error(piTask$configuration$printEvaluationFeedback <- TRUE)
  expect_true(piTask$configuration$printEvaluationFeedback)
  expect_no_error(piTask$configuration$autoEstimateCI <- FALSE)
  expect_false(piTask$configuration$autoEstimateCI)
  expect_no_error(piTask$configuration$algorithmOptions <- list(maxfeval = 3))
  expect_equal(piTask$configuration$algorithmOptions$maxfeval, 3)
  expect_no_error(
    piTask$configuration$objectiveFunctionOptions$robustMethod <- "huber"
  )
  expect_equal(
    piTask$configuration$objectiveFunctionOptions$robustMethod,
    "huber"
  )
  expect_no_error(
    piTask$configuration$objectiveFunctionOptions$linScaleCV <- 0.3
  )
  expect_equal(piTask$configuration$objectiveFunctionOptions$linScaleCV, 0.3)
})

test_that("ParameterIdentification instance prints expected output", {
  piTask <- testPiTask()

  expect_no_error(print(piTask))

  out <- utils::capture.output(print(piTask))
  expect_true(any(grepl("^<ParameterIdentification>$", out)))
  expect_true(any(grepl("Number of parameters:\\s*1", out)))
  expect_true(any(grepl("^Simulations:$", out)))
  expect_true(any(grepl("Aciclovir\\.pkml\\b", out)))
})

test_that("ParameterIdentification errors on missing simulation IDs", {
  simulationMismatch <- loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  )
  expect_error(
    ParameterIdentification$new(
      simulations = simulationMismatch,
      parameters = testParameters(),
      outputMappings = testOutputMapping(),
    ),
    "Mismatch or missing ID detected"
  )
})

test_that("ParameterIdentification errors when PIOutputMapping lacks observed data", {
  expect_error(
    ParameterIdentification$new(
      simulations = testSimulation(),
      parameters = testParameters(),
      outputMappings = testOutputMappingWithoutObsData(),
    ),
    'initialize: No observed data found for quantity path: "Vergin 1995 IV|
    Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
    in simulation: "Vergin 1995 IV"'
  )
})

test_that("ParameterIdentification verifies IDs with multiple simulations and parameters", {
  # no error with multiple simulations and parameter paths
  expect_no_error(
    ParameterIdentification$new(
      simulations = list(sim_250mg, sim_500mg),
      parameters = list(
        piParameterLipo,
        piParameterCl_250mg,
        piParameterCl_500mg
      ),
      outputMappings = list(outputMapping_250mg, outputMapping_500mg),
      configuration = NULL
    )
  )

  # error missing simulation ID
  expect_error(
    ParameterIdentification$new(
      simulations = list(sim_250mg),
      parameters = list(
        piParameterLipo,
        piParameterCl_250mg,
        piParameterCl_500mg
      ),
      outputMappings = list(outputMapping_250mg, outputMapping_500mg)
    ),
    "Mismatch or missing ID detected"
  )
  # error missing parameter ID
  expect_error(
    ParameterIdentification$new(
      simulations = list(sim_250mg, sim_500mg),
      parameters = list(piParameterCl_250mg),
      outputMappings = list(outputMapping_250mg, outputMapping_500mg)
    ),
    "Mismatch or missing ID detected"
  )
  # error missing output mapping ID
  expect_error(
    ParameterIdentification$new(
      simulations = list(sim_250mg, sim_500mg),
      parameters = list(
        piParameterLipo,
        piParameterCl_250mg,
        piParameterCl_500mg
      ),
      outputMappings = list(outputMapping_500mg)
    ),
    "Mismatch or missing ID detected"
  )
})

test_that("ParameterIdentification returns infinite value if simulation fails", {
  testTask <- PISimFailureTester$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = testOutputMapping()
  )

  expect_snapshot(
    suppressMessages(piResult <- testTask$run())
  )

  piResult <- piResult$toList()

  expect_identical(piResult$objectiveValue, Inf)
  expect_false(piResult$convergence)
  expect_true(is.na(piResult$sd))
})

test_that("run() does not warn when few evaluations fail", {
  piTask <- testPiTask()
  piTask$configuration <- lowIterPiConfiguration()
  expect_no_warning(suppressMessages(piTask$run()))
})
