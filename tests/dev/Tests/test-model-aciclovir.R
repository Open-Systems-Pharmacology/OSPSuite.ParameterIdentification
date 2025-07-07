# -------------------------------------------------------------------------
# Test setup:
# - Model: Aciclovir (1 parameter)
# - Data: 1 aggregated dataset
# - Scaling: linear
# - Optimizer: BOBYQA
# - CI estimation: Hessian-based
# -------------------------------------------------------------------------

simulation <- loadSimulation(
  system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
)

piConfiguration <- PIConfiguration$new()
piConfiguration$printEvaluationFeedback <- TRUE

parameterPaths <- c("Aciclovir|Lipophilicity")
modelParams <- c(ospsuite::getParameter(
  path = parameterPaths[[1]], container = simulation)
)
parameters <- c(PIParameters$new(parameters = modelParams))
parameters[[1]]$minValue <- -10
parameters[[1]]$maxValue <- 10

filePath <- getTestDataFilePath("AciclovirLaskinData.xlsx")
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- "Laskin 1982.Group A"
dataConfiguration$namingPattern <- "{Source}.{Sheet}"
observedData <- loadDataSetsFromExcel(
  xlsFilePath = filePath,
  importerConfigurationOrPath = dataConfiguration
)

outputMapping <- PIOutputMapping$new(
  quantity = getQuantity(
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    container = simulation
  )
)
outputMapping$addObservedDataSets(
  observedData$`AciclovirLaskinData.Laskin 1982.Group A`
)
outputMapping$scaling <- "lin"
outputMappings <- c(outputMapping)

task <- ParameterIdentification$new(
  simulations = simulation,
  parameters = parameters,
  outputMappings = outputMapping,
  configuration = piConfiguration
)

taskResults <- task$run()
ciResults <- task$estimateCI()

test_that("Estimated lipophilicity parameter equals expected value", {
  expect_equal(taskResults$par, 1.319, tolerance = 0.01)
})

test_that("Hessian-based standard deviation equals expected value", {
  expect_equal(ciResults$sd, 0.207, tolerance = 0.01)
})


gridSearch <- task$gridSearch()
ofvProfiles <- task$calculateOFVProfiles()

test_that("Grid search objective function values equal expected values", {
  expect_equal(gridSearch$ofv[1], 5709.412, tolerance = 0.01)
  expect_equal(gridSearch$ofv[length(gridSearch$ofv)], 870.813, tolerance = 0.01)
})

test_that("Objective function profiles equal expected values", {
  expect_equal(ofvProfiles[[1]]$ofv[1], 162.067, tolerance = 0.01)
  expect_equal(ofvProfiles[[1]]$ofv[nrow(ofvProfiles[[1]])], 163.121, tolerance = 0.01)
})


# -------------------------------------------------------------------------
# Test setup:
# - Model: Aciclovir (1 parameter)
# - Data: 1 aggregated dataset
# - Scaling: logarithmic
# - Optimizer: BOBYQA
# - CI estimation: Hessian-based
# -------------------------------------------------------------------------

simulation <- loadSimulation(
  system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
)

piConfiguration <- PIConfiguration$new()
piConfiguration$printEvaluationFeedback <- TRUE

parameterPaths <- c("Aciclovir|Lipophilicity")
modelParams <- c(ospsuite::getParameter(
  path = parameterPaths[[1]], container = simulation)
)
parameters <- c(PIParameters$new(parameters = modelParams))
parameters[[1]]$minValue <- -10
parameters[[1]]$maxValue <- 10

filePath <- getTestDataFilePath("AciclovirLaskinData.xlsx")
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- "Laskin 1982.Group A"
dataConfiguration$namingPattern <- "{Source}.{Sheet}"
observedData <- loadDataSetsFromExcel(
  xlsFilePath = filePath,
  importerConfigurationOrPath = dataConfiguration
)

outputMapping <- PIOutputMapping$new(
  quantity = getQuantity(
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    container = simulation
  )
)
outputMapping$addObservedDataSets(
  observedData$`AciclovirLaskinData.Laskin 1982.Group A`
)
outputMapping$scaling <- "log"
outputMappings <- c(outputMapping)

task <- ParameterIdentification$new(
  simulations = simulation,
  parameters = parameters,
  outputMappings = outputMapping,
  configuration = piConfiguration
)

taskResults <- task$run()
ciResults <- task$estimateCI()

test_that("Estimated lipophilicity parameter equals expected value", {
  expect_equal(taskResults$par, 0.164, tolerance = 0.01)
})

test_that("Hessian-based standard deviation equals expected value", {
  expect_equal(ciResults$sd, 0.173, tolerance = 0.01)
})


# -------------------------------------------------------------------------
# Test setup:
# - Model: Aciclovir (1 parameter)
# - Data: 1 aggregated dataset
# - Scaling: Linear
# - Optimizer: BOBYQA
# - Residual weighting: Error-based
# - CI method: Hessian-based
# -------------------------------------------------------------------------

simulation <- loadSimulation(
  system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
)

piConfiguration <- PIConfiguration$new()
piConfiguration$objectiveFunctionOptions$residualWeightingMethod <- "error"
piConfiguration$printEvaluationFeedback <- TRUE

parameterPaths <- c("Aciclovir|Lipophilicity")
modelParams <- c(ospsuite::getParameter(
  path = parameterPaths[[1]], container = simulation)
)
parameters <- c(PIParameters$new(parameters = modelParams))
parameters[[1]]$minValue <- -10
parameters[[1]]$maxValue <- 10

filePath <- getTestDataFilePath("AciclovirLaskinData.xlsx")
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- "Laskin 1982.Group A"
dataConfiguration$namingPattern <- "{Source}.{Sheet}"
observedData <- loadDataSetsFromExcel(
  xlsFilePath = filePath,
  importerConfigurationOrPath = dataConfiguration
)

outputMapping <- PIOutputMapping$new(
  quantity = getQuantity(
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    container = simulation
  )
)
outputMapping$addObservedDataSets(
  observedData$`AciclovirLaskinData.Laskin 1982.Group A`
)
outputMapping$scaling <- "log"
outputMappings <- c(outputMapping)

task <- ParameterIdentification$new(
  simulations = simulation,
  parameters = parameters,
  outputMappings = outputMapping,
  configuration = piConfiguration
)

taskResults <- task$run()
ciResults <- task$estimateCI()

test_that("Estimated lipophilicity parameter equals expected value", {
  expect_equal(taskResults$par, 0.153, tolerance = 0.01)
})

test_that("Hessian-based standard deviation equals expected value", {
  expect_equal(ciResults$sd, 0.149, tolerance = 0.01)
})


# -------------------------------------------------------------------------
# Test setup:
# - Model: Aciclovir (1 parameter)
# - Data: 1 aggregated dataset
# - Scaling: logarithmic
# - Optimizer: BOBYQA
# - CI estimation: bootstrap
# -------------------------------------------------------------------------

simulation <- loadSimulation(
  system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
)

piConfiguration <- PIConfiguration$new()
piConfiguration$printEvaluationFeedback <- TRUE
piConfiguration$ciMethod <- "bootstrap"

parameterPaths <- c("Aciclovir|Lipophilicity")
modelParams <- c(ospsuite::getParameter(
  path = parameterPaths[[1]], container = simulation)
)
parameters <- c(PIParameters$new(parameters = modelParams))
parameters[[1]]$minValue <- -10
parameters[[1]]$maxValue <- 10

filePath <- getTestDataFilePath("AciclovirLaskinData.xlsx")
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- "Laskin 1982.Group A"
dataConfiguration$namingPattern <- "{Source}.{Sheet}"
observedData <- loadDataSetsFromExcel(
  xlsFilePath = filePath,
  importerConfigurationOrPath = dataConfiguration
)

outputMapping <- PIOutputMapping$new(
  quantity = getQuantity(
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    container = simulation
  )
)
outputMapping$addObservedDataSets(
  observedData$`AciclovirLaskinData.Laskin 1982.Group A`
)
outputMapping$scaling <- "log"
outputMappings <- c(outputMapping)

task <- ParameterIdentification$new(
  simulations = simulation,
  parameters = parameters,
  outputMappings = outputMapping,
  configuration = piConfiguration
)

taskResults <- task$run()
ciResults <- task$estimateCI()

test_that("Estimated lipophilicity parameter equals expected value", {
  expect_equal(taskResults$par, 0.164, tolerance = 0.01)
})

test_that("Hessian-based standard deviation equals expected value", {
  expect_equal(ciResults$sd, 0.156, tolerance = 0.01)
})


# -------------------------------------------------------------------------
# Test setup:
# - Model: Aciclovir (1 parameter)
# - Data: 1 aggregated dataset
# - Scaling: logarithmic
# - Optimizer: DEoptim
# - CI estimation: bootstrap
# -------------------------------------------------------------------------

simulation <- loadSimulation(
  system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
)

piConfiguration <- PIConfiguration$new()
piConfiguration$printEvaluationFeedback <- TRUE
piConfiguration$algorithm <- "DEoptim"
algorithmOptions <- AlgorithmOptions_DEoptim
algorithmOptions$reltol <- 1e-4
algorithmOptions$steptol <- 10
piConfiguration$algorithmOptions <- algorithmOptions
piConfiguration$ciMethod <- "bootstrap"

parameterPaths <- c("Aciclovir|Lipophilicity")
modelParams <- c(ospsuite::getParameter(
  path = parameterPaths[[1]], container = simulation)
)
parameters <- c(PIParameters$new(parameters = modelParams))
parameters[[1]]$minValue <- -10
parameters[[1]]$maxValue <- 10

filePath <- getTestDataFilePath("AciclovirLaskinData.xlsx")
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- "Laskin 1982.Group A"
dataConfiguration$namingPattern <- "{Source}.{Sheet}"
observedData <- loadDataSetsFromExcel(
  xlsFilePath = filePath,
  importerConfigurationOrPath = dataConfiguration
)

outputMapping <- PIOutputMapping$new(
  quantity = getQuantity(
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    container = simulation
  )
)
outputMapping$addObservedDataSets(
  observedData$`AciclovirLaskinData.Laskin 1982.Group A`
)
outputMapping$scaling <- "log"
outputMappings <- c(outputMapping)

task <- ParameterIdentification$new(
  simulations = simulation,
  parameters = parameters,
  outputMappings = outputMapping,
  configuration = piConfiguration
)

taskResults <- task$run()
ciResults <- task$estimateCI()

test_that("Estimated lipophilicity parameter equals expected value", {
  expect_equal(taskResults$par, 0.164, tolerance = 0.01)
})

test_that("Hessian-based standard deviation equals expected value", {
  expect_equal(ciResults$sd, 0.160, tolerance = 0.01)
})
