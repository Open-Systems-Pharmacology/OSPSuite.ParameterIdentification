library(ospsuite.parameteridentification)
simulations <- c(loadSimulation("tests/dev/Models/Simulations/Aciclovir.pkml"))
names(simulations) <- "Aciclovir"

piConfiguration <- PIConfiguration$new()
print(piConfiguration)
# If TRUE, the error is printed after each function evaluation. May be useful for assessing if the algorithm converges.
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

filePath <- "tests/data/AciclovirLaskinData.xlsx"
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- "Laskin 1982.Group A"
dataConfiguration$namingPattern <- "{Source}.{Sheet}"
observedData <- loadDataSetsFromExcel(xlsFilePath = filePath, importerConfigurationOrPath = dataConfiguration)

outputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  container = simulations$Aciclovir
))
outputMapping$addObservedDataSets(observedData$`AciclovirLaskinData.Laskin 1982.Group A`)
outputMapping$scaling <- "lin"
outputMappings <- c(outputMapping)

task <- ParameterIdentification$new(
  simulations = simulations,
  parameters = parameters,
  outputMappings = outputMapping,
  configuration = piConfiguration
)
taskResults <- task$run()
