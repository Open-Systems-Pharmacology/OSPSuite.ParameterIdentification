rm(list = ls())
library(ospsuite.parameteridentification)
simulations <- c(loadSimulation("tests/dev/Models/Simulations/Aciclovir.pkml"))
names(simulations) <- "Aciclovir"

piConfiguration <- PIConfiguration$new()
print(piConfiguration)
# If TRUE, the error is printed after each iteration. May be useful for assessing if the algorithm converges.
piConfiguration$printIterationFeedback <- TRUE

parameterPaths <- c("Aciclovir|Lipophilicity")
parameters = list()
for (parameterPath in parameterPaths) {
  modelParams <- list()
  for (simulation in simulations) {
    modelParams <- c(modelParams, ospsuite::getParameter(path = parameterPath, container = simulation))
  }
  piParameter <- PIParameters$new(parameters = modelParams)
  parameters <- c(parameters, piParameter)
}

filePath = "tests/data/AciclovirLaskinData.xlsx"
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- "Laskin 1982.Group A"
dataConfiguration$namingPattern <- "{Source}.{Sheet}"
observedData <- loadDataSetsFromExcel(xlsFilePath = filePath, importerConfigurationOrPath = dataConfiguration)

outputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
                                                            container = simulations$Aciclovir
))
outputMapping$addObservedData(observedData$`AciclovirLaskinData.Laskin 1982.Group A`)
outputMappings = c(outputMapping)

task <- ParameterIdentification$new(simulations = sim,
                                    parameters = parameterPaths,
                                    outputMappings = outputMapping,
                                    configuration = piConfiguration)
