# PIOutputMapping setup

simulation <- loadSimulation(system.file("extdata", "Aciclovir.pkml", package = "ospsuite"))

testQuantity <- ospsuite::getQuantity(
  path = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  container = simulation
)


# PIParameters setup

testParam <- ospsuite::getParameter("Aciclovir|Permeability", simulation)
refVal <- testParam$value


# ParameterIdentification

simulations <- list(simulation)
names(simulations) <- "Aciclovir"

piConfiguration <- PIConfiguration$new()
piConfiguration$printEvaluationFeedback <- FALSE

parameterPaths <- c("Aciclovir|Lipophilicity")
parameters <- list()
for (parameterPath in parameterPaths) {
  modelParams <- list()
  for (simulation in simulations) {
    modelParams <- c(
      modelParams,
      ospsuite::getParameter(path = parameterPath, container = simulation)
    )
  }
  piParameter <- PIParameters$new(parameters = modelParams)
  parameters <- c(parameters, piParameter)
}
parameters[[1]]$minValue <- -10
parameters[[1]]$maxValue <- 10

filePath <- testthat::test_path("../data/AciclovirLaskinData.xlsx")
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- "Laskin 1982.Group A"
dataConfiguration$namingPattern <- "{Source}.{Sheet}"
observedData <- loadDataSetsFromExcel(
  xlsFilePath = filePath,
  importerConfigurationOrPath = dataConfiguration
)

outputMapping <- PIOutputMapping$new(
  quantity = getQuantity("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    container = simulations$Aciclovir
  )
)
outputMapping$addObservedDataSets(observedData$`AciclovirLaskinData.Laskin 1982.Group A`)
outputMappings <- list(outputMapping)
