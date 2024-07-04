# ParameterIdentification

test_that("ParameterIdentification object is correctly created", {
  expect_silent(piTask <- ParameterIdentification$new(
    simulations = testSimulations(),
    parameters = testParameters(),
    outputMappings = testOutputMapping(),
    configuration = piConfiguration
  ))
  testthat::expect_s3_class(piTask, class = c("ParameterIdentification", "R6"))
})

test_that("ParameterIdentification read-only fields can't be modified", {
  piTask <- createPiTask()
  expect_error(piTask$simulations <- testSimulation())
  expect_error(piTask$parameters <- testParameters())
  expect_error(piTask$outputMappings <- testOutputMapping())
})

test_that("ParameterIdentification instance prints without error", {
  piTask <- createPiTask()
  expect_no_error(print(piTask))
})

test_that("ParameterIdentification correctly throws an error upon missing Simulation IDs", {
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

test_that("ParameterIdentification verifies simulation IDs with multiple simulations and parameters correctly", {

  # no error with multiple simulations and parameter paths
  expect_no_error(
    ParameterIdentification$new(
      simulations = list(sim_250mg, sim_500mg),
      parameters = list(piParameterLipo, piParameterCl_250mg, piParameterCl_500mg),
      outputMappings = list(outputMapping_250mg, outputMapping_500mg),
      configuration = piConfiguration
    )
  )

  # error missing simulation ID
  expect_error(
    ParameterIdentification$new(
      simulations = list(sim_250mg),
      parameters = list(piParameterLipo, piParameterCl_250mg, piParameterCl_500mg),
      outputMappings = list(outputMapping_250mg, outputMapping_500mg),
      configuration = piConfiguration
    ),
    "Mismatch or missing ID detected"
  )
  # error missing parameter ID
  expect_error(
    ParameterIdentification$new(
      simulations = list(sim_250mg, sim_500mg),
      parameters = list(piParameterCl_250mg),
      outputMappings = list(outputMapping_250mg, outputMapping_500mg),
      configuration = piConfiguration
    ),
    "Mismatch or missing ID detected"
  )
  # error missing output mapping ID
  expect_error(
    ParameterIdentification$new(
      simulations = list(sim_250mg, sim_500mg),
      parameters = list(piParameterLipo, piParameterCl_250mg, piParameterCl_500mg),
      outputMappings = list(outputMapping_500mg),
      configuration = piConfiguration
    ),
    "Mismatch or missing ID detected"
  )
})

test_that("ParameterIdentification returns an infinite cost structure if the
          simulation is NA", {
  piTask <- createPiTask()
  expect_message(
    modCost <- piTask$.__enclos_env__$private$.objectiveFunction(NA),
    "Simulation was not successful"
  )
  expect_equal(
    modCost,
    .createErrorCostStructure(infinite = TRUE)
  )
})

test_that("plotResults() returns expected plot before running a parameter estimation task", {
  piTask <- createPiTask()
  vdiffr::expect_doppelganger("before_estimation", piTask$plotResults()[[1]])
})

test_that("ParameterIdentification configuration can be changed without error", {
  piTask <- createPiTask()
  expect_no_error(piTask$configuration$algorithm <- "HJKB")
  expect_equal(piTask$configuration$algorithm, "HJKB")
  expect_no_error(piTask$configuration$printEvaluationFeedback <- TRUE)
  expect_true(piTask$configuration$printEvaluationFeedback)
  expect_no_error(piTask$configuration$algorithmOptions <- list(maxeval = 3))
  expect_equal(piTask$configuration$algorithmOptions$maxeval, 3)
  expect_no_error(piTask$configuration$objectiveFunctionOptions$robustMethod <- "huber")
  expect_equal(piTask$configuration$objectiveFunctionOptions$robustMethod, "huber")
  expect_no_error(piTask$configuration$objectiveFunctionOptions$linScaleCV <- 0.3)
  expect_equal(piTask$configuration$objectiveFunctionOptions$linScaleCV, 0.3)
})

test_that("ParameterIdentification$run() errors on invalid objective function option", {
  piTask <- createPiTask()
  piTask$configuration$objectiveFunctionOptions$objectiveFunctionType <- "invalidType"
  expect_error(piTask$run(),
    regexp = "Value\\(s\\) 'invalidType' not included in allowed values: 'lsq, m3'"
  )

  piTask <- createPiTask()
  piTask$configuration$objectiveFunctionOptions$linScaleCV <- 10
  expect_error(piTask$run(),
    regexp = "Value\\(s\\) out of the allowed range: \\[1e-09, 1\\]"
  )
})

# Test BOBYQA Algorithm (Default)

test_that("ParameterIdentification$run() runs without error and produces expected
          results using default BOBYQA algorithm", {
  piTask <- createPiTask()
  expect_no_error(piResults <- piTask$run())
  resultFields <- c("par", "value", "nrOfFnEvaluations", "hessian", "sigma", "lwr", "upr", "cv")
  resultValues <- piResults[names(piResults) %in% resultFields]
  resultValues <- unlist(resultValues)
  referenceValues <- c(1.3189, 156.2574, 22, 730.5977, 0.0523, 1.2163, 1.4214, 3.9671)
  names(referenceValues) <- resultFields
  expect_equal(!!resultValues, referenceValues, tolerance = 1e-03)
})

test_that("ParameterIdentification$run() prints expected evaluation feedback using BOBYQA algorithm", {
  piTask <- createPiTask()
  piTask$configuration$algorithm <- "BOBYQA"
  piTask$configuration$printEvaluationFeedback <- TRUE
  piTask$configuration$algorithmOptions <- list(maxeval = 3)
  evalOutput <- capture_output(piTask$run())
  expect_snapshot_value(evalOutput, style = "serialize")
})

piTask <- createPiTask()
piResults <- piTask$run()
test_that("plotResults() returns expected plot after running a parameter estimation task", {
  vdiffr::expect_doppelganger("after_estimation", piTask$plotResults()[[1]])
})

test_that("plotResults() returns expected plot whith parameter input", {
  vdiffr::expect_doppelganger("custom_parameter", piTask$plotResults(1.2)[[1]])
})


# Test HJBK Algorithm

test_that("ParameterIdentification$run() fails using HJKB algorithm with one parameter", {
  piTask <- createPiTask()
  piTask$configuration$algorithm <- "HJKB"
  expect_error(piTask$run())
})


# Test DEoptim Algorithm

test_that("ParameterIdentification$run() runs without error using HJKB algorithm", {
  piTask <- createPiTask()
  piTask$configuration$algorithm <- "DEoptim"
  piTask$configuration$printEvaluationFeedback <- FALSE
  piTask$configuration$algorithmOptions <- list(itermax = 3, trace = FALSE)
  expect_no_error(piTask$run())
})


# test_that("Grid search produces no error with default parameters", {
#   expect_no_error(gridSearchResults <- piTask$gridSearch())
# })
# gridSearchResults <- piTask$gridSearch(totalEvaluations = 10)
# test_that("Grid search produced correct results", {
#   expect_snapshot_value(gridSearchResults, style = "serialize")
# })


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
