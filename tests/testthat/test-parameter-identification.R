modelFolder <- file.path(getwd(), "../dev/Models/Simulations")
sim <- loadSimulation(paste0(modelFolder, "/IR_model_doseResponse.pkml"))
modelParameter <- ospsuite::getParameter(path = "Organism|IR_I_P_Inter_tHalf", container = sim)

########### Load observed data########
# Path to the folder where experimental data files are located
dataFolder <- file.path(getwd(), "../Data")
# Name of the excel file with experimental data
dataFile <- "DataSet.xlsx"
dataSheets <- c("DoseResponse")

importerConfiguration <- ospsuite::loadDataImporterConfiguration(
  configurationFilePath = file.path(getwd(), "../Data", "dataImporter_configuration.xml")
)
importerConfiguration$sheets <- dataSheets

dataSets <- ospsuite::loadDataSetsFromExcel(
  xlsFilePath = file.path(dataFolder, dataFile),
  importerConfigurationOrPath = importerConfiguration
)

# Observed values are given as % from maximal observed value. Model ouput is the
# absolute value. In order to compare observed and simulated values, simulated
# results must be normalized first. This is achieved by providing a transformation
# function.
resultsTransformationFunction <- function(xVals, yVals) {
  yVals <- yVals / max(yVals) * 100
  return(list(xVals = xVals, yVals = yVals))
}

# Define optimization parameter
piParameter <- PIParameters$new(parameters = modelParameter)
# Define data mapping
piOutputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|IRS1_P",
  container = sim
))
piOutputMapping$transformResultsFunction <- resultsTransformationFunction
dataSets$`________IRS_P_rel`$yDimension <- ospDimensions$Amount
piOutputMapping$addObservedDataSets(data = dataSets$`________IRS_P_rel`)

test_that("It can initialize ParameterIdentification when the simulateSteadyState
          is TRUE and the model does not contain any state variable parameters", {
  piConfiguration <- PIConfiguration$new()
  piConfiguration$simulateSteadyState <- TRUE

  # Create new parameter identification.
  expect_no_error(pi <- ParameterIdentification$new(
    simulations = sim, parameters = piParameter,
    outputMappings = piOutputMapping,
    configuration = piConfiguration
  ))
})

### Testing the parameter identification package on aciclovir example
simulations <- c(loadSimulation("../dev/Models/Simulations/Aciclovir.pkml"))
names(simulations) <- "Aciclovir"

piConfiguration <- PIConfiguration$new()
piConfiguration$printEvaluationFeedback <- FALSE

parameterPaths <- c("Aciclovir|Lipophilicity")
parameters <- list()
for (parameterPath in parameterPaths) {
  modelParams <- list()
  for (simulation in simulations) {
    modelParams <- c(modelParams, ospsuite::getParameter(path = parameterPath, container = simulation))
  }
  piParameter <- PIParameters$new(parameters = modelParams)
  parameters <- c(parameters, piParameter)
}
parameters[[1]]$minValue <- -10
parameters[[1]]$maxValue <- 10

filePath <- "../data/AciclovirLaskinData.xlsx"
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- "Laskin 1982.Group A"
dataConfiguration$namingPattern <- "{Source}.{Sheet}"
observedData <- loadDataSetsFromExcel(xlsFilePath = filePath, importerConfigurationOrPath = dataConfiguration)

outputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  container = simulations$Aciclovir
))
outputMapping$addObservedDataSets(observedData$`AciclovirLaskinData.Laskin 1982.Group A`)
outputMappings <- c(outputMapping)

test_that("A parameter identification object is correctly created", {
  expect_no_error(task <- ParameterIdentification$new(
    simulations = simulations,
    parameters = parameters,
    outputMappings = outputMapping,
    configuration = piConfiguration
  ))
})
task <- ParameterIdentification$new(
  simulations = simulations,
  parameters = parameters,
  outputMappings = outputMapping,
  configuration = piConfiguration
)
test_that("Plotting works before running the parameter estimation task", {
  vdiffr::expect_doppelganger("before_estimation", task$plotResults()[[1]])
  # plotResults returns a list of plots, one plot for each output mapping
  # in this example, we have a single output mapping
})
taskResults <- task$run()
test_that("Plotting works after running the parameter estimation task", {
  vdiffr::expect_doppelganger("after_estimation", task$plotResults()[[1]])
})
test_that("Plotting returns a different plot when supplied with input parameters", {
  vdiffr::expect_doppelganger("custom_parameter", task$plotResults(1.2)[[1]])
})
test_that("The results object contains a parameter estimate", {
  expect_equal(taskResults$par, 1.318853, tol = 1e-4)
})
test_that("The results object contains a number of function evaluations", {
  expect_equal(taskResults$nrOfFnEvaluations, 22)
})
test_that("The results object contains a lower bound of the confidence interval", {
  expect_equal(taskResults$lwr, 1.216545, tolerance = 1e-3)
})
test_that("The results object contains an upper bound of the confidence interval", {
  expect_equal(taskResults$upr, 1.42116, tolerance = 1e-3)
})
outputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  container = simulations$Aciclovir
))
outputMapping$addObservedDataSets(observedData$`AciclovirLaskinData.Laskin 1982.Group A`)
outputMapping$scaling <- "log"
outputMappings <- c(outputMapping)
test_that("Output mappings with log scaling are processed without errors", {
  expect_no_error(task <- ParameterIdentification$new(
    simulations = simulations,
    parameters = parameters,
    outputMappings = outputMapping,
    configuration = piConfiguration
  ))
  expect_no_error(taskResults <- task$run())
})
test_that("Algorithm can be changed in PI configuration", {
  expect_equal(task$configuration$algorithm, "BOBYQA")
  task$configuration$algorithm <- "HJKB"
  expect_equal(task$configuration$algorithm, "HJKB")
})
test_that("Grid search produces no error with default parameters", {
  expect_no_error(gridSearchResults <- task$calculateGrid())
})
gridSearchResults <- task$calculateGrid(totalEvaluations = 10)
test_that("Grid search produced correct results", {
  expect_snapshot_value(gridSearchResults)
})
