# ParameterIdentification - Core

# Function that allows to replace paths that will differ on the different
# machines by a fixed value to be used in snapshots
transformId <- function(x) {
  gsub(
    pattern = ".*ospsuite/extdata/Aciclovir\\.pkml",
    replacement = "<SimPath>",
    x,
    fixed = FALSE
  )
}

test_that("ParameterIdentification is created successfully", {
  piConfiguration <- PIConfiguration$new()
  expect_silent(piTask <- ParameterIdentification$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = testOutputMapping(),
    configuration = piConfiguration
  ))
  expect_s3_class(piTask, class = c("ParameterIdentification", "R6"))
})

test_that("ParameterIdentification read-only fields cannot be modified", {
  piTask <- createPiTask()
  expect_error(piTask$simulations <- testSimulation())
  expect_error(piTask$parameters <- testParameters())
  expect_error(piTask$outputMappings <- testOutputMapping())
})

test_that("ParameterIdentification configuration can be modified without errors", {
  piTask <- createPiTask()
  expect_no_error(piTask$configuration$algorithm <- "HJKB")
  expect_equal(piTask$configuration$algorithm, "HJKB")
  expect_no_error(piTask$configuration$printEvaluationFeedback <- TRUE)
  expect_true(piTask$configuration$printEvaluationFeedback)
  expect_no_error(piTask$configuration$autoEstimateCI <- FALSE)
  expect_false(piTask$configuration$autoEstimateCI)
  expect_no_error(piTask$configuration$algorithmOptions <- list(maxeval = 3))
  expect_equal(piTask$configuration$algorithmOptions$maxeval, 3)
  expect_no_error(piTask$configuration$objectiveFunctionOptions$robustMethod <- "huber")
  expect_equal(piTask$configuration$objectiveFunctionOptions$robustMethod, "huber")
  expect_no_error(piTask$configuration$objectiveFunctionOptions$linScaleCV <- 0.3)
  expect_equal(piTask$configuration$objectiveFunctionOptions$linScaleCV, 0.3)
})

test_that("ParameterIdentification instance prints expected output", {
  piTask <- createPiTask()
  expect_snapshot(print(piTask), transform = transformId)
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
      parameters = list(piParameterLipo, piParameterCl_250mg, piParameterCl_500mg),
      outputMappings = list(outputMapping_250mg, outputMapping_500mg),
      configuration = NULL
    )
  )

  # error missing simulation ID
  expect_error(
    ParameterIdentification$new(
      simulations = list(sim_250mg),
      parameters = list(piParameterLipo, piParameterCl_250mg, piParameterCl_500mg),
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
      parameters = list(piParameterLipo, piParameterCl_250mg, piParameterCl_500mg),
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

  suppressMessages(
    expect_message(
      piResult <- testTask$run(),
      "Returning infinite cost structure due to simulation failure"
    )
  )

  piResult <- piResult$toList()

  expect_identical(piResult$objectiveValue, Inf)
  expect_false(piResult$convergence)
  expect_true(is.na(piResult$sd))
})


# modelFolder <- file.path(testthat::test_path("../dev/Models/Simulations"))
# sim <- loadSimulation(paste0(modelFolder, "/IR_model_doseResponse.pkml"))
# modelParameter <- ospsuite::getParameter(path = "Organism|IR_I_P_Inter_tHalf", container = sim)
#
# ########### Load observed data########
# # Path to the folder where experimental data files are located
# dataFolder <- file.path(testthat::test_path("../Data"))
# # Name of the excel file with experimental data
# dataFile <- "DataSet.xlsx"
# dataSheets <- c("DoseResponse")
#
# importerConfiguration <- ospsuite::loadDataImporterConfiguration(
#   configurationFilePath = file.path(dataFolder, "dataImporter_configuration.xml")
# )
# importerConfiguration$sheets <- dataSheets
#
# dataSets <- ospsuite::loadDataSetsFromExcel(
#   xlsFilePath = file.path(dataFolder, dataFile),
#   importerConfigurationOrPath = importerConfiguration
# )
#
# # Observed values are given as % from maximal observed value. Model ouput is the
# # absolute value. In order to compare observed and simulated values, simulated
# # results must be normalized first. This is achieved by providing a transformation
# # function.
# resultsTransformationFunction <- function(xVals, yVals) {
#   yVals <- yVals / max(yVals) * 100
#   return(list(xVals = xVals, yVals = yVals))
# }
#
# # Define optimization parameter
# piParameter <- PIParameters$new(parameters = modelParameter)
# # Define data mapping
# piOutputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|IRS1_P",
#   container = sim
# ))
# piOutputMapping$transformResultsFunction <- resultsTransformationFunction
# dataSets$`________IRS_P_rel`$yDimension <- ospDimensions$Amount
# piOutputMapping$addObservedDataSets(data = dataSets$`________IRS_P_rel`)
#
# # test_that("It can initialize ParameterIdentification when the simulateSteadyState
# #           is TRUE and the model does not contain any state variable parameters", {
# #   piConfiguration <- PIConfiguration$new()
# #   piConfiguration$simulateSteadyState <- TRUE
# #
# #   # Create new parameter identification.
# #   expect_no_error(pi <- ParameterIdentification$new(
# #     simulations = sim, parameters = piParameter,
# #     outputMappings = piOutputMapping,
# #     configuration = piConfiguration
# #   ))
# # })
