context("ParameterIdentification")

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
  pi <- ParameterIdentification$new(
    simulations = sim, parameters = piParameter,
    outputMappings = piOutputMapping,
    configuration = piConfiguration
  )
  # Plot results before optimization
  expect_error(pi$plotCurrentResults(), regexp = NA)
})

### Testing the parameter identification package on aciclovir example
simulations <- c(loadSimulation("../dev/Models/Simulations/Aciclovir.pkml"))
names(simulations) <- "Aciclovir"

piConfiguration <- PIConfiguration$new()
piConfiguration$printIterationFeedback <- FALSE
piConfiguration$targetFunctionType <- "FME_modCost"

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
task_results <- task$run()
test_that("The results object contains a parameter estimate", {
  expect_equal(task_results$par, -0.009700017)
})
test_that("The results object contains a number of function evaluations", {
  expect_equal(task_results$feval, 15)
})
test_that("The results object contains a lower bound of the confidence interval", {
  expect_equal(task_results$lwr, -5.4220188)
})
test_that("The results object contains an upper bound of the confidence interval", {
  expect_equal(task_results$upr, 5.4026188)
})
