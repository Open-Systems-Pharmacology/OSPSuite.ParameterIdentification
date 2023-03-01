library(ospsuite.parameteridentification)
simulations <- c(loadSimulation("tests/dev/Models/Simulations/Aciclovir.pkml"))
names(simulations) <- "Aciclovir"

piConfiguration <- PIConfiguration$new()
print(piConfiguration)
# If TRUE, the error is printed after each iteration. May be useful for assessing if the algorithm converges.
piConfiguration$printIterationFeedback <- TRUE
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

filePath <- "tests/data/AciclovirLaskinData.xlsx"
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- "Laskin 1982.Group A"
dataConfiguration$namingPattern <- "{Source}.{Sheet}"
observedData <- loadDataSetsFromExcel(xlsFilePath = filePath, importerConfigurationOrPath = dataConfiguration)

outputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  container = simulations$Aciclovir
))
outputMapping$addObservedDataSets(observedData$`AciclovirLaskinData.Laskin 1982.Group A`)
outputMapping$scaling <- "log"
outputMappings <- c(outputMapping)

task <- ParameterIdentification$new(
  simulations = simulations,
  parameters = parameters,
  outputMappings = outputMapping,
  configuration = piConfiguration
)
task_results <- task$run()
