# -------------------------------------------------------------------------
# Test setup:
# - Model: Midazolam (2 parameters)
# - Data: 1 individual dataset
# - Scaling: Linear
# - Optimizer: BOBYQA
# - CI method: Hessian-based
# -------------------------------------------------------------------------

simulation <- loadSimulation(
  getTestDataFilePath("Smith1981 iv 5mg Midazolam.pkml")
)

piConfiguration <- PIConfiguration$new()
piConfiguration$printEvaluationFeedback <- TRUE

parameterInputData <- list(
  list(path = "Midazolam|Lipophilicity", min = -10, max = 10, start = 3.9),
  list(
    path = "Midazolam-CYP3A4-Patki et al. 2003 rCYP3A4|kcat",
    min = 0, max = 3200, start = 320
  )
)

parameters <- vector("list", length = length(parameterInputData))
for (idx in seq_along(parameterInputData)) {
  modelParams <- list()
  modelParams <- c(
    modelParams,
    ospsuite::getParameter(
      path = parameterInputData[[idx]]$path,
      container = simulation
    )
  )
  parameters[[idx]] <- PIParameters$new(parameters = modelParams)
  parameters[[idx]]$minValue <- parameterInputData[[idx]]$min
  parameters[[idx]]$maxValue <- parameterInputData[[idx]]$max
  parameters[[idx]]$startValue <- parameterInputData[[idx]]$start
}

filePath <- getTestDataFilePath("Midazolam_Smith_1981.xlsx")
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- "Smith1981"
dataConfiguration$namingPattern <- "{Source}.{Sheet}"

observedData <- loadDataSetsFromExcel(
  xlsFilePath = filePath,
  importerConfigurationOrPath = dataConfiguration
)

outputMapping <- PIOutputMapping$new(
  quantity = getQuantity(
    "Organism|PeripheralVenousBlood|Midazolam|Plasma (Peripheral Venous Blood)",
    container = simulation
  )
)

outputMapping$addObservedDataSets(
  observedData$Midazolam_Smith_1981.Smith1981
)
outputMapping$scaling <- "lin"
outputMappings <- c(outputMapping)

task <- ParameterIdentification$new(
  simulations = simulation,
  parameters = parameters,
  outputMappings = outputMapping,
  configuration = piConfiguration
)

taskResult <- task$run()

test_that("Estimated lipophilicity and kcat parameters equal expected values", {
  expect_equal(taskResult$finalParameters, c(3.366, 0.119), tolerance = 0.01)
})

test_that("Hessian-based standard deviations equal expected values", {
  expect_true(is.null(taskResult$ciError))
  expect_equal(taskResult$sd, c(0.014, 0.113), tolerance = 0.01)
})


# -------------------------------------------------------------------------
# Test setup:
# - Model: Midazolam (2 parameters)
# - Data: 1 individual dataset
# - Scaling: Logarithmic
# - Optimizer: BOBYQA
# - CI method: Hessian-based
# -------------------------------------------------------------------------

simulation <- loadSimulation(
  getTestDataFilePath("Smith1981 iv 5mg Midazolam.pkml")
)

piConfiguration <- PIConfiguration$new()
piConfiguration$printEvaluationFeedback <- TRUE

parameterInputData <- list(
  list(path = "Midazolam|Lipophilicity", min = -10, max = 10, start = 3.9),
  list(
    path = "Midazolam-CYP3A4-Patki et al. 2003 rCYP3A4|kcat",
    min = 0, max = 3200, start = 320
  )
)

parameters <- vector("list", length = length(parameterInputData))
for (idx in seq_along(parameterInputData)) {
  modelParams <- list()
  modelParams <- c(
    modelParams,
    ospsuite::getParameter(
      path = parameterInputData[[idx]]$path,
      container = simulation
    )
  )
  parameters[[idx]] <- PIParameters$new(parameters = modelParams)
  parameters[[idx]]$minValue <- parameterInputData[[idx]]$min
  parameters[[idx]]$maxValue <- parameterInputData[[idx]]$max
  parameters[[idx]]$startValue <- parameterInputData[[idx]]$start
}

filePath <- getTestDataFilePath("Midazolam_Smith_1981.xlsx")
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- "Smith1981"
dataConfiguration$namingPattern <- "{Source}.{Sheet}"

observedData <- loadDataSetsFromExcel(
  xlsFilePath = filePath,
  importerConfigurationOrPath = dataConfiguration
)

outputMapping <- PIOutputMapping$new(
  quantity = getQuantity(
    "Organism|PeripheralVenousBlood|Midazolam|Plasma (Peripheral Venous Blood)",
    container = simulation
  )
)

outputMapping$addObservedDataSets(
  observedData$Midazolam_Smith_1981.Smith1981
)
outputMapping$scaling <- "log"
outputMappings <- c(outputMapping)

task <- ParameterIdentification$new(
  simulations = simulation,
  parameters = parameters,
  outputMappings = outputMapping,
  configuration = piConfiguration
)

taskResult <- task$run()

test_that("Estimated lipophilicity and kcat parameters equal expected values", {
  expect_equal(taskResult$finalParameters, c(3.326, 0.783), tolerance = 0.01)
})

test_that("Hessian-based standard deviations equal expected values", {
  expect_true(is.null(taskResult$ciError))
  expect_equal(taskResult$sd, c(0.074, 0.269), tolerance = 0.01)
})
