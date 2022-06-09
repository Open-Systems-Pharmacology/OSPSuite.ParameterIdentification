library(ospsuite)
library(ospsuite.parameteridentification)

##### VARIABLE DEFINITION#####
# Path to the folder where the model file is located.
modelFolder <- file.path(getwd(), "../Models/Simulations")
# Path to the folder where experimental data files are located
dataFolder <- file.path(getwd(), "../Data")
# Name of the excel file with experimental data
dataFile <- "DataSet.xlsx"

observedData <- readObservedData(dataFolder, dataFile, groupingColumns = c("PK"), sheets = list("Boswell_2012"))
# should return a list of objects of class ospsuite::DataSet

####### LOAD SIMULATIONS and put them in a named list######
simNames <- c("Vehicle.pkml", "0.75 mg_kg.pkml", "2.5 mg_kg.pkml")
simulations <- lapply(simNames, function(x) {
  ospsuite::loadSimulation(file.path(modelFolder, x))
})
names(simulations) <- simNames

########## Create PIConfiguration#############
piConfiguration <- PIConfiguration$new()
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

# Create a DataCombined object based on observedData and simulations
# and a ParameterIdentification object based on these
mapping <- createDataMapping(observedData$Boswell_2012$IV_0.75mgKg_ADC =
                               getQuantity("Organism|Tumor|Weight (tissue)",
                                           container = simulations$`0.75 mg_kg.pkml`))
pi <- ParameterIdentification$new(mapping = mapping,
                                  parameters = parameters,
                                  configuration = piConfiguration)

# Plot results before optimization
pi$plotCurrentResults()
results <- pi$run()
print(results)
pi$plotCurrentResults()
