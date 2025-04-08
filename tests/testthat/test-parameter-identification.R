# ParameterIdentification

# Function that allows to replace paths that will differ on the different machines by a fixed value to
# be used in snapshots
transformId <- function(x) {
  # For whatever reason, simulation file path is passed as a character array with first entry being '*'
  # and the second entry the action simulation file path
  isPath <- any(grepl("ospsuite/extdata/Aciclovir.pkml", x, fixed = TRUE))
  ifelse(isPath, "<SimPath>", paste(x, collapse = ""))
}

test_that("ParameterIdentification is created successfully", {
  expect_silent(piTask <- ParameterIdentification$new(
    simulations = testSimulations(),
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

test_that("ParameterIdentification instance prints without errors", {
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

test_that("ParameterIdentification returns infinite value if simulation fails", {
  PITester <- R6::R6Class(
    inherit = ParameterIdentification,
    cloneable = FALSE,
    private = list(
      .evaluate = function(currVals) {
        private$.fnEvaluations <- private$.fnEvaluations + 2
        stop("Simulated failure in evaluation")
      }
    )
  )

  testTask <- PITester$new(
    simulations = testSimulations(),
    parameters = testParameters(),
    outputMappings = testOutputMapping()
  )

  suppressMessages(
    expect_message(
      piResult <- testTask$run(),
      "Returning infinite cost structure due to simulation failure"
    )
  )

  expect_identical(piResult$value, Inf)
})

test_that("ParameterIdentification errors if initial simulation fails", {
  modPiTask <- testModifiedTask()
  suppressMessages(suppressWarnings(
    expect_error(
      modPiTask$run(),
      ".*Stopping optimization: Initial simulation failed.*"
    )
  ))
})

test_that("plotResults() generates expected plot before parameter estimation", {
  piTask <- createPiTask()
  vdiffr::expect_doppelganger("before_estimation", piTask$plotResults()[[1]])
})

test_that("ParameterIdentification configuration can be modified without errors", {
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

test_that("ParameterIdentification$run() errors on invalid objective function options", {
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
test_that("ParameterIdentification$run() runs successfully using default BOBYQA algorithm", {
  piTask <- createPiTask()
  expect_no_error(piResults <- piTask$run())
  resultFields <- c("par", "value", "nrOfFnEvaluations", "hessian", "sigma", "lwr", "upr", "cv")
  resultValues <- piResults[names(piResults) %in% resultFields]
  resultValues <- unlist(resultValues)
  referenceValues <- c(1.3189, 156.2574, 22, 730.5977, 0.0523, 1.2163, 1.4214, 3.9671)
  names(referenceValues) <- resultFields
  expect_equal(!!resultValues, referenceValues, tolerance = 1e-03)
})

test_that("ParameterIdentification$run() outputs expected evaluation feedback using BOBYQA algorithm", {
  piTask <- createPiTask()
  piTask$configuration$algorithm <- "BOBYQA"
  piTask$configuration$printEvaluationFeedback <- TRUE
  piTask$configuration$algorithmOptions <- list(maxeval = 3)
  evalOutput <- capture_output(piTask$run())
  expect_snapshot_value(evalOutput, style = "serialize")
})

piTask <- createPiTask()
piResults <- piTask$run()
test_that("plotResults() generates expected plot after parameter estimation", {
  vdiffr::expect_doppelganger("after_estimation", piTask$plotResults()[[1]])
})

test_that("plotResults() generates expected plot with parameter input", {
  vdiffr::expect_doppelganger("custom_parameter", piTask$plotResults(1.2)[[1]])
})

# Test HJBK Algorithm
test_that("ParameterIdentification$run() fails with HJKB algorithm and one parameter", {
  piTask <- createPiTask()
  piTask$configuration$algorithm <- "HJKB"
  expect_error(piTask$run())
})

# Test DEoptim Algorithm
test_that("ParameterIdentification$run() runs successfully using DEoptim algorithm", {
  piTask <- createPiTask()
  piTask$configuration$algorithm <- "DEoptim"
  piTask$configuration$printEvaluationFeedback <- FALSE
  piTask$configuration$algorithmOptions <- list(itermax = 3, trace = FALSE)
  expect_no_error(piTask$run())
})

# User weights
yLen1 <- length(testObservedDataMultiple()[[1]]$yValues)
yLen2 <- length(testObservedDataMultiple()[[2]]$yValues)

test_that("ParameterIdentification works with one dataset and one scalar weight", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$addObservedDataSets(testObservedDataMultiple()[[1]])
  outputMapping$setDataWeights(list(dataSet1 = 2))

  piTask <- ParameterIdentification$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = outputMapping,
    configuration = lowIterPiConfiguration()
  )

  result <- piTask$run()
  expect_equal(result$value, 3112.516, tolerance = 3)
})

test_that("ParameterIdentification works with two datasets and one weight", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$addObservedDataSets(testObservedDataMultiple())
  outputMapping$setDataWeights(list(dataSet2 = rep(2, yLen2)))

  piTask <- ParameterIdentification$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = outputMapping,
    configuration = lowIterPiConfiguration()
  )

  result <- piTask$run()
  expect_equal(result$value, 163.346, tolerance = 3)
})

test_that("ParameterIdentification works with multiple datasets and individual weights", {
  outputMapping <- PIOutputMapping$new(quantity = testQuantity)
  outputMapping$addObservedDataSets(testObservedDataMultiple())
  outputMapping$setDataWeights(
    list(dataSet1 = rep(2, yLen1), dataSet2 = rep(1.5, yLen2))
  )

  piTask <- ParameterIdentification$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = outputMapping,
    configuration = lowIterPiConfiguration()
  )

  result <- piTask$run()
  expect_equal(result$value, 227.5477, tolerance = 3)
})

# gridSearch
test_that("ParameterIdentification$gridSearch() works without error for single
          parameter", {
  piTask <- createPiTask()
  expect_no_error(gridSearchResults <- piTask$gridSearch())
})

test_that("ParameterIdentification$gridSearch() works with multiple parameters and default settings", {
  piTask <- ParameterIdentification$new(
    simulations = sim_250mg,
    parameters = list(piParameterLipo_250mg, piParameterCl_250mg),
    outputMappings = outputMapping_250mg
  )
  expect_no_error(gridSearchResults <- piTask$gridSearch())
  expect_snapshot(gridSearchResults[1:10, ])
})

test_that("ParameterIdentification$gridSearch() errors for `logScaleFlag = TRUE` with non-positive values", {
  piTask <- ParameterIdentification$new(
    simulations = sim_250mg,
    parameters = list(piParameterLipo_250mg, piParameterCl_250mg),
    outputMappings = outputMapping_250mg
  )
  expect_no_error(
    piTask$gridSearch(logScaleFlag = c(FALSE, TRUE), totalEvaluations = 3)
  )
  expect_error(
    piTask$gridSearch(logScaleFlag = c(TRUE, TRUE), totalEvaluations = 3),
    "Logarithmic scaling is not available for non-positive parameter values"
  )
})

test_that("ParameterIdentification$gridSearch() sets new start values with correct message", {
  piParameterLipo_250mg$startValue <- 0
  piParameterCl_250mg$startValue <- 0
  piTask <- ParameterIdentification$new(
    simulations = sim_250mg,
    parameters = list(piParameterLipo_250mg, piParameterCl_250mg),
    outputMappings = outputMapping_250mg
  )
  startValueMessage <- capture_output(
    piTask$gridSearch(setStartValue = TRUE, totalEvaluations = 10)
  )
  expect_snapshot(startValueMessage)
})

test_that("ParameterIdentification$gridSearch() returns `Inf` upon simulation failure", {
  piTask <- ParameterIdentification$new(
    simulations = sim_250mg,
    parameters = list(piParameterLipo_250mg, piParameterCl_250mg),
    outputMappings = outputMapping_250mg
  )
  suppressMessages(suppressWarnings(
    expect_warning(
      gridSearchResults <- piTask$gridSearch(lower = c(0, -0.5), totalEvaluations = 5)
    )
  ))
  expect_snapshot(gridSearchResults$ofv)
})

# calculateOFVProfiles
test_that("ParameterIdentification$calculateOFVProfiles() works as expected", {
  piTask <- ParameterIdentification$new(
    simulations = sim_250mg,
    parameters = list(piParameterLipo_250mg, piParameterCl_250mg),
    outputMappings = outputMapping_250mg
  )
  ofvProfiles <- piTask$calculateOFVProfiles()
  expect_equal(length(ofvProfiles), length(piTask$parameters))
  expect_snapshot(ofvProfiles[[1]][1:10, ])
  expect_snapshot(ofvProfiles[[2]][1:10, ])
})

test_that("ParameterIdentification$calculateOFVProfiles() returns `Inf` upon simulation failure", {
  piTask <- ParameterIdentification$new(
    simulations = sim_250mg,
    parameters = list(piParameterLipo_250mg, piParameterCl_250mg),
    outputMappings = outputMapping_250mg
  )

  suppressMessages(suppressWarnings(
    expect_warning(
      ofvProfiles <- piTask$calculateOFVProfiles(par = c(0, -0.25), totalEvaluations = 3)
    )
  ))
  expect_snapshot(ofvProfiles[[2]]$ofv)
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
