# library(ospsuite.parameteridentification)
library(esqlabsRLegacy)
##### VARIABLE DEFINITION#####
# Path to the folder where the model file is located.
modelFolder <- file.path(getwd(), "../Models/Simulations")
# Path to the folder where experimental data files are located
dataFolder <- file.path(getwd(), "../../data")
# Name of the excel file with experimental data
dataFile <- "DataSet.xlsx"

########### Load observed data########
dataSheets <- c(
  "Backer_1989",
  "McClain_1988",
  "Marshall_1984",
  "DoseResponse",
  "Cedersund_2008"
)

importerConfiguration <- ospsuite::loadDataImporterConfiguration(
  configurationFilePath = file.path(dataFolder, "dataImporter_configuration.xml")
)
importerConfiguration$sheets <- dataSheets

dataSets <- ospsuite::loadDataSetsFromExcel(
  xlsFilePath = file.path(dataFolder, dataFile),
  importerConfigurationOrPath = importerConfiguration
)

####### LOAD SIMULATIONS and put them in a named list######
simNames <- c(
  "StepIncrease_100nm.pkml",
  "StepIncrease_17.2nm.pkml",
  "IR_model_doseResponse.pkml"
)
simulations <- lapply(simNames, function(x) {
  ospsuite::loadSimulation(file.path(modelFolder, x))
})
names(simulations) <- simNames

########## Create PIConfiguration#############
piConfiguration <- PIConfiguration$new()
piConfiguration$targetFunctionType <- "lsq"
piConfiguration$simulateSteadyState <- TRUE
piConfiguration$steadyStateTime <- 1000

print(piConfiguration)
# If TRUE, the error is printed after each iteration. May be useful for assessing if the algorithm converges.
piConfiguration$printIterationFeedback <- TRUE

######### Define parameters to optimize#######
parameters <- list()
parameterPaths <- c(
  "Organism|IR_I_P_Inter_tHalf",
  "Organism|k_IR_Int_recycle",
  "Organism|k_IRS1_deact",
  "Organism|k_IRS_phosph",
  "Organism|k_IR_I_P_Int_deact",
  "Organism|k_PTP_deact",
  "Organism|k_PTP_phosph"
)
for (parameterPath in parameterPaths) {
  modelParams <- list()
  for (simulation in simulations) {
    modelParams <- c(modelParams, ospsuite::getParameter(path = parameterPath, container = simulation))
  }
  piParameter <- PIParameters$new(parameters = modelParams)
  parameters <- c(parameters, piParameter)
}

######### Define otput mappings#######
piOutputMappings <- list()

# Observed values are given as % from maximal observed value. Model ouput is the
# absolute value. In order to compare observed and simulated values, simulated
# results must be normalized first. This is achieved by providing a transformation
# function.
resultsTransformationFunction <- function(xVals, yVals) {
  yVals <- yVals / max(yVals) * 100
  return(list(xVals = xVals, yVals = yVals))
}

# Create a PIOutputMappings setting the quantity of a model
piOutputMapping <- PIOutputMapping$new(quantity = getQuantity(
  path = "Organism|OUTPUT_PercentInternalized",
  container = simulations$StepIncrease_100nm.pkml
))
# Add observed data
piOutputMapping$addObservedData(observedData$Backer_1989$IR_Int_Percent)
piOutputMappings <- append(piOutputMappings, piOutputMapping)

piOutputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|OUTPUT_Percent_IR_P",
  container = simulations$StepIncrease_100nm.pkml
))
piOutputMapping$addObservedData(observedData$Backer_1989$IR_P_Percent)
piOutputMappings <- append(piOutputMappings, piOutputMapping)

piOutputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|OUTPUT_PercentInternalized",
  container = simulations$StepIncrease_17.2nm.pkml
))
piOutputMapping$addObservedData(list(
  observedData$McClain_1988$IR_Int_Percent,
  observedData$Marshall_1984$IR_Int_Percent
))
piOutputMappings <- append(piOutputMappings, piOutputMapping)

piOutputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|OUTPUT_IR_P_total",
  container = simulations$IR_model_doseResponse.pkml
))
# Applying a user defined function for results post-processing
piOutputMapping$transformResultsFunction <- resultsTransformationFunction
# Changing the dimension of the observed data here as a workaround to match
# the dimensions of observed and simulated data
observedData$DoseResponse$IR_P_rel$yDimension <- ospDimensions$Amount
piOutputMapping$addObservedData(observedData$DoseResponse$IR_P_rel)
piOutputMappings <- append(piOutputMappings, piOutputMapping)

piOutputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|IRS1_P",
  container = simulations$IR_model_doseResponse.pkml
))
piOutputMapping$transformResultsFunction <- resultsTransformationFunction
observedData$DoseResponse$IRS_P_rel$yDimension <- ospDimensions$Amount
piOutputMapping$addObservedData(observedData$DoseResponse$IRS_P_rel)
piOutputMappings <- append(piOutputMappings, piOutputMapping)

piOutputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|OUTPUT_IR_P_total",
  container = simulations$StepIncrease_100nm.pkml
))
piOutputMapping$transformResultsFunction <- resultsTransformationFunction
observedData$Cedersund_2008$IR_P$yDimension <- ospDimensions$Amount
piOutputMapping$addObservedData(observedData$Cedersund_2008$IR_P)
# While we have to apply post-processing (normalization to 100%) of simulated
# results, this data set is the opposite case. The publication reports intensity
# of meausred signal (phosphorylation of insulin receptor) in arbitrary units, with
# values above 100%. We are applying scaling factor to normalize to 100%.
piOutputMapping$setYFactors(observedData$Cedersund_2008$IR_P$label, 1 / max(observedData$Cedersund_2008$IR_P$yValues) * 100)
# The observed data set starts from minute 30. We therefore apply an offset of
# 30 minutes to the x values
piOutputMapping$setXOffset(observedData$Cedersund_2008$IR_P$label, -30)
piOutputMappings <- append(piOutputMappings, piOutputMapping)

piOutputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|IRS1_P",
  container = simulations$StepIncrease_100nm.pkml
))
piOutputMapping$transformResultsFunction <- resultsTransformationFunction
observedData$Cedersund_2008$IRS_P$yDimension <- ospDimensions$Amount
piOutputMapping$addObservedData(observedData$Cedersund_2008$IRS_P)
piOutputMapping$setYFactors(observedData$Cedersund_2008$IRS_P$label, 1 / max(observedData$Cedersund_2008$IRS_P$yValues) * 100)
piOutputMapping$setXOffset(observedData$Cedersund_2008$IRS_P$label, -30)
piOutputMappings <- append(piOutputMappings, piOutputMapping)

# Create new parameter identification.
pi <- ParameterIdentification$new(
  simulations = simulations, parameters = parameters, outputMappings = piOutputMappings,
  configuration = piConfiguration
)
# Plot results before optimization
pi$plotCurrentResults()

# Run optimization
results <- pi$run()

# Plot results after optimization
pi$plotCurrentResults()
