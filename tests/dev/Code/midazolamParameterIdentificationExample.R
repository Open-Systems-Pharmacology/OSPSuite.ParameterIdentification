library(ospsuite.parameteridentification)
simulations <- c(loadSimulation("tests/dev/Models/Simulations/Smith1981 iv 5mg Midazolam.pkml"))
names(simulations) <- "Midazolam"

piConfiguration <- PIConfiguration$new()
print(piConfiguration)
# If TRUE, the error is printed after each iteration. May be useful for assessing if the algorithm converges.
piConfiguration$printIterationFeedback <- TRUE

parameterPaths <- c("Midazolam|Lipophilicity", "Midazolam-CYP3A4-Patki et al. 2003 rCYP3A4|kcat")
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
parameters[[1]]$startValue <- 3.9
parameters[[2]]$minValue <- 0
parameters[[2]]$maxValue <- 3200
parameters[[2]]$startValue <- 320

filePath <- "tests/data/Midazolam_Smith_1981.xlsx"
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- "Smith1981"
dataConfiguration$namingPattern <- "{Source}.{Sheet}"
observedData <- loadDataSetsFromExcel(xlsFilePath = filePath, importerConfigurationOrPath = dataConfiguration)

outputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Midazolam|Plasma (Peripheral Venous Blood)",
  container = simulations$Midazolam
))
outputMapping$addObservedDataSets(observedData$Midazolam_Smith_1981.Smith1981)
outputMapping$scaling <- "lin"
outputMappings <- c(outputMapping)

task <- ParameterIdentification$new(
  simulations = simulations,
  parameters = parameters,
  outputMappings = outputMapping,
  configuration = piConfiguration
)
task_results <- task$run()
