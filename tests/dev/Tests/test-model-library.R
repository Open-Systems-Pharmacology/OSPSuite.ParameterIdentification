
# Load clarithromycin 3-parameter model and confirm that the optimal parameter values are as expected
simulations <- c(
  "IV250" = loadSimulation(system.file("extdata", "Chu1992 iv 250mg Clarithromycin.pkml", package = "ospsuite.parameteridentification")),
  "PO250" = loadSimulation(system.file("extdata", "Chu1993 po 250mg Clarithromycin.pkml", package = "ospsuite.parameteridentification")),
  "PO250MD" = loadSimulation(system.file("extdata", "Chu1993 po 250mg md Clarithromycin.pkml", package = "ospsuite.parameteridentification")),
  "PO500" = loadSimulation(system.file("extdata", "Chu1993 po 250mg Clarithromycin.pkml", package = "ospsuite.parameteridentification")),
  "PO500MD" = loadSimulation(system.file("extdata", "Chu1993 po 250mg md Clarithromycin.pkml", package = "ospsuite.parameteridentification"))
)
setParameterValuesByPath("Applications|Clarithromycin po, Chu 1993, 250mg|Tablet Clarithromycin|Application_1|ProtocolSchemaItem|Dose",
  simulation = simulations$PO500,
  values = 500, units = "mg"
)

# Get all `Dose` parameters for the MD simulation
doseParams <- getAllParametersMatching("Applications|**|Dose", container = simulations$PO500MD)
setParameterValues(parameters = doseParams, values = rep(500, length(doseParams)), units = rep("mg", length(doseParams)))

piConfiguration <- PIConfiguration$new()
# If TRUE, the error is printed after each iteration. May be useful for assessing if the algorithm converges.
piConfiguration$printEvaluationFeedback <- FALSE

parameterInputData <- list(
  list(path = "Clarithromycin-CYP3A4-fit|kcat", min = 0, max = 100, start = 10),
  list(path = "Neighborhoods|Kidney_pls_Kidney_ur|Clarithromycin|Renal Clearances-fitted|Specific clearance", min = 0, max = 100, start = 10),
  list(path = "Clarithromycin|Specific intestinal permeability (transcellular)", min = 0, max = 1, start = 0.01)
)
# The code below assumes that every parameter is present in each simulation
# and parameter values across all simulations should be changed in parallel
parameters <- vector("list", length = length(parameterInputData))
for (idx in seq_along(parameterInputData)) {
  modelParams <- list()
  for (simulation in simulations) {
    modelParams <- c(modelParams, ospsuite::getParameter(
      path = parameterInputData[[idx]]$path,
      container = simulation
    ))
  }
  parameters[[idx]] <- PIParameters$new(parameters = modelParams)
  parameters[[idx]]$minValue <- parameterInputData[[idx]]$min
  parameters[[idx]]$maxValue <- parameterInputData[[idx]]$max
  parameters[[idx]]$startValue <- parameterInputData[[idx]]$start
}

# Observed data is loaded from two different files
# because IV data is reported in µmol/L, and PO data is reported in µg/ml
filePath <- system.file("extdata", "Clarithromycin_Profiles.xlsx", package = "ospsuite.parameteridentification")
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- c("IV250", "PO250", "PO250MD", "PO500", "PO500MD")
dataConfiguration$namingPattern <- "{Sheet}"
observedData <- loadDataSetsFromExcel(xlsFilePath = filePath, importerConfigurationOrPath = dataConfiguration)

# The code below assumes each simulation is matched against one sheet in observed data
outputMappings <- vector("list", length = length(simulations))
for (idx in seq_along(simulations)) {
  outputMappings[[idx]] <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Clarithromycin|Plasma (Peripheral Venous Blood)",
    container = simulations[[idx]]
  ))
  outputMappings[[idx]]$addObservedDataSets(observedData[[names(simulations)[[idx]]]])
  outputMappings[[idx]]$scaling <- "lin"
}

task <- ParameterIdentification$new(
  simulations = simulations,
  parameters = parameters,
  outputMappings = outputMappings,
  configuration = piConfiguration
)
taskResults <- task$run()

test_that("Optimal kcat value in the clarithromycin model is close to expected value of 14.1", {
  expect_equal(taskResults$par[[1]], 14.10284, tolerance = 0.01)
})
test_that("Optimal specific clearance value in the clarithromycin model is close to expected value of 6.24", {
  expect_equal(taskResults$par[[2]], 6.243346, tolerance = 0.01)
})
test_that("Optimal specific intestinal permeability value in the clarithromycin model is close to expected value of 1.3e-6", {
  expect_equal(taskResults$par[[3]], 1.306399e-6, tolerance = 0.01)
})
test_that("The hessian value in the clarithromycin model is calculated without errors", {
  expect_false(any(is.na(taskResults$hessian)))
})

# test_that("The grid calculation (with the default parameters) in the clarithromycin model returns the expected results", {
#   expect_snapshot_value(task$gridSearch()[["ofv"]], style = "serialize")
# })
# test_that("The profile calculation (with the default parameters) in the clarithromycin model returns the expected results", {
#   expect_snapshot_value(task$calculateOFVProfiles(), style = "serialize")
# })
# test_that("The profile plot in the clarithromycin model returns the expected graphics", {
#   profiles <- task$calculateOFVProfiles()
#   plots <- plotOFVProfiles(profiles)
#   vdiffr::expect_doppelganger("ofv-profile-clarithromycin-1", plots[[1]])
#   vdiffr::expect_doppelganger("ofv-profile-clarithromycin-2", plots[[2]])
#   vdiffr::expect_doppelganger("ofv-profile-clarithromycin-3", plots[[3]])
# })
