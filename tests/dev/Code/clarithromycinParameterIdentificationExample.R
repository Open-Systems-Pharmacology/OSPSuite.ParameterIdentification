library(ospsuite.parameteridentification)
simulations <- c("IV250"   = loadSimulation("tests/dev/Models/Simulations/Chu1992 iv 250mg Clarithromycin.pkml"),
                 "PO250"   = loadSimulation("tests/dev/Models/Simulations/Chu1993 po 250mg Clarithromycin.pkml"),
                 "PO250MD" = loadSimulation("tests/dev/Models/Simulations/Chu1993 po 250mg md Clarithromycin.pkml"),
                 "PO500"   = loadSimulation("tests/dev/Models/Simulations/Chu1993 po 500mg Clarithromycin.pkml"),
                 "PO500MD" = loadSimulation("tests/dev/Models/Simulations/Chu1993 po 500mg md Clarithromycin.pkml"))

piConfiguration <- PIConfiguration$new()
# If TRUE, the error is printed after each iteration. May be useful for assessing if the algorithm converges.
piConfiguration$printIterationFeedback <- TRUE

parameterInputData <- list(list(path = "Clarithromycin-CYP3A4-fit|kcat", min = 0, max = 100, start = 10),
                           list(path = "Neighborhoods|Kidney_pls_Kidney_ur|Clarithromycin|Renal Clearances-fitted|Specific clearance", min = 0, max = 100, start = 10),
                           list(path = "Clarithromycin|Specific intestinal permeability (transcellular)", min = 0, max = 1, start = 0.01))
# The code below assumes that every parameter is present in each simulation
# and parameter values across all simulations should be changed in parallel
parameters <- vector("list", length = length(parameterInputData))
for (idx in seq_along(parameterInputData)) {
  modelParams <- list()
  for (simulation in simulations) {
    modelParams <- c(modelParams, ospsuite::getParameter(path = parameterInputData[[idx]]$path,
                                                         container = simulation))
  }
  parameters[[idx]] <- PIParameters$new(parameters = modelParams)
  parameters[[idx]]$minValue <- parameterInputData[[idx]]$min
  parameters[[idx]]$maxValue <- parameterInputData[[idx]]$max
  parameters[[idx]]$startValue <- parameterInputData[[idx]]$start
}

# Observed data is loaded from two different files
# because IV data is reported in μmol/L, and PO data is reported in µg/ml
filePath <- "tests/data/Clarithromycin_Chu_1992.xlsx"
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- "IV250"
dataConfiguration$namingPattern <- "{Sheet}"
observedData_IV <- loadDataSetsFromExcel(xlsFilePath = filePath, importerConfigurationOrPath = dataConfiguration)
filePath <- "tests/data/Clarithromycin_Chu_1993.xlsx"
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- c("PO250", "PO250MD", "PO500", "PO500MD")
dataConfiguration$namingPattern <- "{Sheet}"
observedData_PO <- loadDataSetsFromExcel(xlsFilePath = filePath, importerConfigurationOrPath = dataConfiguration)
observedData <- c(observedData_IV, observedData_PO)

# The code below assumes each simulation is matched against one sheet in observed data
outputMappings <- vector("list", length = length(simulations))
for (idx in seq_along(simulations)) {
  outputMappings[[idx]] <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Clarithromycin|Plasma (Peripheral Venous Blood)",
                                                                      container = simulations[[idx]]))
  outputMappings[[idx]]$addObservedDataSets(observedData[[names(simulations)[[idx]]]])
  outputMappings[[idx]]$scaling <- "lin"
}

task <- ParameterIdentification$new(
  simulations = simulations,
  parameters = parameters,
  outputMappings = outputMapping,
  configuration = piConfiguration
)
task_results <- task$run()
