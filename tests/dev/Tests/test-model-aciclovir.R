# Load aciclovir 1-parameter model and confirm that the optimal parameter value is as expected

simulation <- loadSimulation(system.file("extdata", "Aciclovir.pkml", package = "ospsuite"))
piConfiguration <- PIConfiguration$new()
piConfiguration$printEvaluationFeedback <- TRUE
# piConfiguration$objectiveFunctionOptions$residualWeightingMethod <- "error"

parameterPaths <- c("Aciclovir|Lipophilicity")
modelParams <- c(ospsuite::getParameter(path = parameterPaths[[1]], container = simulation))
parameters <- c(PIParameters$new(parameters = modelParams))
parameters[[1]]$minValue <- -10
parameters[[1]]$maxValue <- 10

filePath <- getTestDataFilePath("AciclovirLaskinData.xlsx")
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- "Laskin 1982.Group A"
dataConfiguration$namingPattern <- "{Source}.{Sheet}"
observedData <- loadDataSetsFromExcel(xlsFilePath = filePath, importerConfigurationOrPath = dataConfiguration)

outputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
                                                            container = simulation
))
outputMapping$addObservedDataSets(observedData$`AciclovirLaskinData.Laskin 1982.Group A`)
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

test_that("Optimal lipophilicity value in the aciclovir model is close to expected value of 1.318", {
  expect_equal(taskResults$par, 1.318, tolerance = 0.01)
})
test_that("The hessian value in the aciclovir model is calculated without errors", {
  expect_false(is.na(taskResults$hessian))
})
# I only store the `ofv` column in the snapshot file, because the parameter
# names might have nonstandard symbols disrupting the `expect` function

# Commented out until https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/92 is fixed
# or separate tests are implemented
# test_that("The grid calculation (with default parameters) in the aciclovir model returns the expected results", {
#   expect_snapshot_value(task$gridSearch()[["ofv"]], style = "serialize")
# })
# test_that("The profile calculation (with default parameters) in the aciclovir model returns the expected results", {
#   expect_snapshot_value(task$calculateOFVProfiles(), style = "serialize")
# })
# test_that("The profile plot in the aciclovir model returns the expected graphics", {
#   vdiffr::expect_doppelganger("ofv-profile-aciclovir", plotOFVProfiles(task$calculateOFVProfiles())[[1]])
# })

