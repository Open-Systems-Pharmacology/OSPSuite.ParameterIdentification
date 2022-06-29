library(ospsuite)
library(ospsuite.parameteridentification)

##### VARIABLE DEFINITION#####
rm(list = ls())
# Path to the folder where the model file is located.
modelFolder <- file.path(getwd(), "../Models/Simulations")
# Path to the folder where experimental data files are located
dataFolder <- file.path(getwd(), "../Data")
# Name of the excel file with experimental data
dataFile <- "DataSet.xlsx"
sheets <- "Boswell_2012"
groupingColumns <- c("PK")

observedData <- readObservedData(dataFolder, dataFile, groupingColumns, sheets)

####### LOAD SIMULATIONS and put them in a named list######
simNames <- c("Vehicle.pkml", "0.75 mg_kg.pkml", "2.5 mg_kg.pkml")
simulations <- lapply(simNames, function(x) {
  ospsuite::loadSimulation(file.path(modelFolder, x))
})
names(simulations) <- simNames
# a named list of Simulation class instances

########## Create PIConfiguration#############
piConfiguration <- PIConfiguration$new()
piConfiguration$printIterationFeedback <- TRUE
piConfiguration$targetFunction <- "lsq"

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
# parameters is a list of PIParameters class instances

piTask <- ParameterIdentification$new(data = observedData, models = simulations,
                                      parameters = parameters, configuration = piConfiguration,
                                      mapping = list("IV_Vehicle" = "Vehicle.pkml", "IV_2.50mgKg_ADC" = "2.5 mg_kg.pkml", "IV_0.75mgKg_ADC" = "0.75 mg_kg.pkml"),
                                      quantities = list("IV_Vehicle" = getQuantity("Organism|Tumor|Weight (tissue)", container = simulations[["Vehicle.pkml"]]), "IV_2.50mgKg_ADC" = getQuantity("Organism|Tumor|Weight (tissue)", container = simulations[["2.5 mg_kg.pkml"]]), "IV_0.75mgKg_ADC" = getQuantity("Organism|Tumor|Weight (tissue)", container = simulations[["0.75 mg_kg.pkml"]])))

results <- piTask$run()
print(results)
