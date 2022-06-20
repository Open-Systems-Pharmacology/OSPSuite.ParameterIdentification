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

if (!file.exists(file.path(dataFolder, dataFile))) {
  warning("No file found at ", file.path(dataFolder, dataFile))
}
dataConfiguration <- createImporterConfigurationForFile(filePath = file.path(dataFolder, dataFile))
dataConfiguration$sheets <- sheets
for (columnName in groupingColumns) {
  dataConfiguration$addGroupingColumn(columnName)
}
dataConfiguration$namingPattern <- paste0("{", paste0(groupingColumns, collapse = "}.{"), "}")
if (dataConfiguration$namingPattern == "{}") {
  dataConfiguration$namingPattern <- "{Source}.{Sheet}"
}
observedData <- NULL
try(
  observedData <- loadDataSetsFromExcel(xlsFilePath = file.path(dataFolder, dataFile), importerConfigurationOrPath = dataConfiguration)
)

# returns a list of objects of class ospsuite::DataSet

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

object <- ParameterIdentification$new(data = observedData,
                                      models = simulations,
                                      parameters = parameters,
                                      configuration = piConfiguration)

# Plot results before optimization
pi$plotCurrentResults()
results <- pi$run()
print(results)
pi$plotCurrentResults()
