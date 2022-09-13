context("ParameterIdentification")

modelFolder <- file.path(getwd(), "../dev/Models/Simulations")
sim <- loadSimulation(paste0(modelFolder, "/IR_model_doseResponse.pkml"))
modelParameter <- ospsuite::getParameter(path = "Organism|IR_I_P_Inter_tHalf", container = sim)

# Path to the folder where experimental data files are located
dataFolder <- file.path(getwd(), "../dev/Data")
# Name of the excel file with experimental data
dataFile <- "DataSet.xlsx"

dataConfiguration <- esqlabsRLegacy::DataConfiguration$new(
  dataFolder = dataFolder,
  dataFile = dataFile,
  compoundPropertiesFile = NULL,
  dataSheets = c(
    "DoseResponse"
  )
)
observedData <- esqlabsRLegacy::readOSPSTimeValues(dataConfiguration)

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
observedData$DoseResponse$IRS_P_rel$yDimension <- ospDimensions$Amount
piOutputMapping$addObservedData(observedData$DoseResponse$IRS_P_rel)

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
