# modelFolder <- file.path(getwd(), "../dev/Models/Simulations")
# # modelFolder <- file.path(getwd(), "dev/Models/Simulations")
# print(modelFolder)
# sim <- loadSimulation(paste0(modelFolder, "/IR_model_doseResponse.pkml"))
# modelParameter <- ospsuite::getParameter(path = "Organism|IR_I_P_Inter_tHalf", container = sim)
#
# # ########### Load observed data########
# # Path to the folder where experimental data files are located
# dataFolder <- file.path(getwd(), "../Data")
# # Name of the excel file with experimental data
# dataFile <- "DataSet.xlsx"
# dataSheets <- c("DoseResponse")
#
# importerConfiguration <- ospsuite::loadDataImporterConfiguration(
#   configurationFilePath = file.path(getwd(), "../Data", "dataImporter_configuration.xml")
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
# test_that("It can initialize ParameterIdentification when the simulateSteadyState
#           is TRUE and the model does not contain any state variable parameters", {
#   piConfiguration <- PIConfiguration$new()
#   piConfiguration$simulateSteadyState <- TRUE
#
#   # Create new parameter identification.
#   expect_no_error(pi <- ParameterIdentification$new(
#     simulations = sim, parameters = piParameter,
#     outputMappings = piOutputMapping,
#     configuration = piConfiguration
#   ))
# })


# Tests using aciclovir simulation and data -------------------------------

simulations <- c(loadTestSimulation("Aciclovir"))
names(simulations) <- "Aciclovir"

testConfiguration <- PIConfiguration$new()
testConfiguration$printEvaluationFeedback <- FALSE

testQuantity <- ospsuite::getQuantity(
  path = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  container = simulations$Aciclovir
)

testParameters <- ospsuite::getParameter(
  path = "Aciclovir|Lipophilicity",
  container = simulations$Aciclovir
)

testPiParameters <- c(PIParameters$new(parameters = testParameters))

testPiParameters[[1]]$minValue <- -10
testPiParameters[[1]]$maxValue <- 10

filePath <- getTestDataFilePath("AciclovirLaskinData.xlsx")
dataConfiguration <- ospsuite::createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- "Laskin 1982.Group A"
dataConfiguration$namingPattern <- "{Source}.{Sheet}"
observedData <- loadDataSetsFromExcel(
  xlsFilePath = filePath,
  importerConfigurationOrPath = dataConfiguration
)

outputMapping <- PIOutputMapping$new(quantity = testQuantity)
outputMapping$addObservedDataSets(observedData$`AciclovirLaskinData.Laskin 1982.Group A`)
outputMappings <- c(outputMapping)

test_that("ParameterIdentification initializes correctly", {
  expect_no_error(task <- ParameterIdentification$new(
    simulations = simulations,
    parameters = testPiParameters,
    outputMappings = outputMappings,
    configuration = testConfiguration
  ))
  expect_s3_class(task, "ParameterIdentification")
  expect_s3_class(task$configuration, "PIConfiguration")
  expect_length(task$simulations, 1)
  expect_length(task$parameters, 1)
  expect_length(task$outputMappings, 1)
})


task <- ParameterIdentification$new(
  simulations = simulations,
  parameters = testPiParameters,
  outputMappings = outputMappings,
  configuration = testConfiguration
)

test_that("Plotting works before running the parameter estimation task", {
  plots <- suppressWarnings({
    task$plotResults()
  })
  expect_true(is.list(plots))
  vdiffr::expect_doppelganger("before_estimation", plots[[1]])
})

test_that("ParameterIdentification run method works", {
  expect_no_error(taskResults <- task$run())
  expect_true(is.list(taskResults))
})

taskResults <- task$run()

test_that("Results object has expected names", {
  expected_names <- c(
    "par", "value", "iter", "convergence", "message",
    "elapsed", "algorithm", "nrOfFnEvaluations", "hessian",
    "sigma", "lwr", "upr", "cv"
  )
  expect_named(taskResults, expected_names)
})

test_that("Results object contains a parameter estimate", {
  expect_equal(taskResults$par, 1.318853, tolerance = 1e-4)
})

test_that("Results object contains a number of function evaluations", {
  expect_equal(taskResults$nrOfFnEvaluations, 22)
})

test_that("Results object contains a lower bound of the confidence interval", {
  expect_equal(taskResults$lwr, 1.216545, tolerance = 1e-3)
})

test_that("Results object contains an upper bound of the confidence interval", {
  expect_equal(taskResults$upr, 1.42116, tolerance = 1e-3)
})

# task$plotResults generates warning:
# Warning message:
#   In ospsuite::plotObservedVsSimulated(dataCombined[[idx]], plotConfiguration,  :
#                                          Linear scale is inappropriate when `foldDistance` argument is specified.
# Root cause: ospsuite::plotObservedVsSimulated
#                 if (is_any_scale_linear && !is.null(foldDistance)) {
#                   warning(messages$linearScaleWithFoldDistance())
#                   foldDistance <- 0
#                 }
test_that("Plotting works after running the parameter estimation task", {
  plots <- suppressWarnings({
    task$plotResults()
  })
  vdiffr::expect_doppelganger("after_estimation", plots[[1]])
})

test_that("Plotting returns a different plot when supplied with input parameters", {
  plots <- suppressWarnings({
    task$plotResults(1.2)
  })
  vdiffr::expect_doppelganger("custom_parameter", plots[[1]])
})


outputMapping <- PIOutputMapping$new(quantity = testQuantity)
outputMapping$addObservedDataSets(observedData$`AciclovirLaskinData.Laskin 1982.Group A`)
outputMapping$scaling <- "log"
outputMappings <- c(outputMapping)

test_that("Output mappings with log scaling are processed without errors", {
  expect_no_error(task <- ParameterIdentification$new(
    simulations = simulations,
    parameters = testPiParameters,
    outputMappings = outputMapping,
    configuration = testConfiguration
  ))
  expect_no_error(taskResults <- task$run())
})
test_that("Algorithm can be changed in PI configuration", {
  expect_equal(task$configuration$algorithm, "BOBYQA")
  task$configuration$algorithm <- "HJKB"
  expect_equal(task$configuration$algorithm, "HJKB")
})

test_that("Grid search produces no error with default parameters", {
  expect_no_error(gridSearchResults <- task$gridSearch())
  expect_true(is.data.frame(gridSearchResults))
})

gridSearchResults <- task$gridSearch(totalEvaluations = 10)
test_that("Grid search produced correct results", {
  expect_snapshot_value(gridSearchResults, style = "serialize")
})


# test robust and error residual weighting --------------------------------

testConfiguration <- PIConfiguration$new()
testConfiguration$printEvaluationFeedback <- FALSE

test_that("Objective function options can be set to Huber method and
          ParameterIdentification object runs correctly", {
  expect_no_error(testConfiguration$objectiveFunctionOptions <- list(
    robustMethod = "huber"
  ))
  expect_no_error(task <- ParameterIdentification$new(
    simulations = simulations,
    parameters = testPiParameters,
    outputMappings = outputMapping,
    configuration = testConfiguration
  ))
  expect_no_error(taskResults <- task$run())
  expect_equal(taskResults$par, -0.3468597, tolerance = 1e-4)
})

testConfiguration <- PIConfiguration$new()
testConfiguration$printEvaluationFeedback <- FALSE

test_that("Objective function options can be set to error weighting method and
          ParameterIdentification object runs correctly", {
  expect_no_error(testConfiguration$objectiveFunctionOptions <- list(
    residualWeightingMethod = "error"
  ))
  expect_no_error(task <- ParameterIdentification$new(
    simulations = simulations,
    parameters = testPiParameters,
    outputMappings = outputMapping,
    configuration = testConfiguration
  ))
  expect_no_error(taskResults <- task$run())
  expect_equal(taskResults$par, 0.1531188, tolerance = 1e-4)
})


# test censored error calculation -----------------------------------------

observedData$`AciclovirLaskinData.Laskin 1982.Group A`$LLOQ <- 0.5

outputMapping <- PIOutputMapping$new(quantity = testQuantity)
outputMapping$addObservedDataSets(observedData$`AciclovirLaskinData.Laskin 1982.Group A`)
outputMappings <- c(outputMapping)

testConfiguration <- PIConfiguration$new()
testConfiguration$printEvaluationFeedback <- FALSE

test_that("Objective function options can be set to M3 censored error method and
          ParameterIdentification object runs correctly", {
  expect_no_error(testConfiguration$objectiveFunctionOptions <- list(
    objectiveFunctionType = "m3"
  ))
  expect_no_error(task <- ParameterIdentification$new(
    simulations = simulations,
    parameters = testPiParameters,
    outputMappings = outputMapping,
    configuration = testConfiguration
  ))
  expect_no_error(taskResults <- task$run())
  expect_equal(taskResults$par, 1.32004, tolerance = 1e-4)
})
