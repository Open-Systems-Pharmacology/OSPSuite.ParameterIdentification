library(ospsuite.parameteridentification)
library(esqlabsRLegacy)
##### VARIABLE DEFINITION#####
# Path to the folder where the model file is located.
modelFolder <- file.path(getwd(), "../Models/Simulations")
# Path to the folder where experimental data files are located
dataFolder <- file.path(getwd(), "../Data")
# Name of the excel file with experimental data
dataFile <- "DataSet.xlsx"

# DataConfiguration is an object that describes how to read observed data from an excel file
dataConfiguration <- DataConfiguration$new(
  dataFolder = dataFolder,
  dataFile = dataFile,
  compoundPropertiesFile = NULL,
  dataSheets = c(
    "Boswell_2012"
  )
)

# To test with error:
# dataConfiguration <- DataConfiguration$new(dataFolder = dataFolder,
#                                            dataFile = dataFile,
#                                            compoundPropertiesFile = NULL,
#                                            dataSheets = c(
#                                              "Boswell_2012_error"
#                                            ))

observedData <- readOSPSTimeValues(dataConfiguration)

####### LOAD SIMULATIONS and put them in a named list######
simNames <- c("Vehicle.pkml", "0.75 mg_kg.pkml", "2.5 mg_kg.pkml")
simulations <- lapply(simNames, function(x) {
  ospsuite::loadSimulation(file.path(modelFolder, x))
})
names(simulations) <- simNames

########## Create PIConfiguration#############
piConfiguration <- PIConfiguration$new()
print(piConfiguration)
# If TRUE, the error is printed after each iteration. May be useful for assessing if the algorithm converges.
piConfiguration$printIterationFeedback <- TRUE

######### Define parameters to optimize#######
parameters <- list()
parameterPaths <- c(
  "Organism|Tumor|Intracellular|k1",
  "Organism|Tumor|Intracellular|k2"
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

# Create a PIOutputMapping setting the quantity of a model
piOutputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|Tumor|Weight (tissue)",
  container = simulations$Vehicle.pkml
))
# Add observed data. Multiple data can be added to the same mapping
piOutputMapping$addObservedData(observedData$Boswell_2012$IV_Vehicle)
# Add the mapping to the list of all mappings
piOutputMappings <- append(piOutputMappings, piOutputMapping)

piOutputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|Tumor|Weight (tissue)",
  container = simulations$`0.75 mg_kg.pkml`
))
piOutputMapping$addObservedData(observedData$Boswell_2012$IV_0.75mgKg_ADC)
piOutputMappings <- append(piOutputMappings, piOutputMapping)

piOutputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|Tumor|Weight (tissue)",
  container = simulations$`2.5 mg_kg.pkml`
))
piOutputMapping$addObservedData(observedData$Boswell_2012$IV_2.50mgKg_ADC)
piOutputMappings <- append(piOutputMappings, piOutputMapping)

# Create new parameter identification. This PI would optimize all three simulations.
# pi <- ParameterIdentification$new(simulations = simulations, parameters = parameters, outputMappings = piOutputMappings,
#                                   configuration = piConfiguration)
# Plot results before optimization
pi$plotCurrentResults()

# OR

# FOR PERFORMANCE REASONS, OPTIMIZE WITH ONE SIMULATION ONLY.
# IF YOU WANT TO USE ALL THREE SIMULATIONS, USE THE `parameters` LIST
# CREATED ABOVE AND PASS ALL THREE SIMULATIONS AND MAPPINGS

parameters <- lapply(parameterPaths, function(x) {
  modelParams <- getParameter(path = x, container = simulations$`2.5 mg_kg.pkml`)
  piParameters <- PIParameters$new(parameters = modelParams)
})

pi <- ParameterIdentification$new(
  simulations = simulations$`2.5 mg_kg.pkml`, parameters = parameters, outputMappings = piOutputMappings[[3]],
  configuration = piConfiguration
)
pi$plotCurrentResults()

results <- pi$run()
print(results)

pi$plotCurrentResults()
