# Load aciclovir 1-parameter model and confirm that the optimal parameter value is as expected

simulations <- c(loadSimulation("../dev/Models/Simulations/Aciclovir.pkml"))
names(simulations) <- "Aciclovir"
piConfiguration <- PIConfiguration$new()
piConfiguration$printEvaluationFeedback <- FALSE

parameterPaths <- c("Aciclovir|Lipophilicity")
modelParams <- c(ospsuite::getParameter(path = parameterPaths[[1]], container = simulations$Aciclovir))
parameters <- c(PIParameters$new(parameters = modelParams))
parameters[[1]]$minValue <- -10
parameters[[1]]$maxValue <- 10

filePath <- "../data/AciclovirLaskinData.xlsx"
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- "Laskin 1982.Group A"
dataConfiguration$namingPattern <- "{Source}.{Sheet}"
observedData <- loadDataSetsFromExcel(xlsFilePath = filePath, importerConfigurationOrPath = dataConfiguration)

outputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  container = simulations$Aciclovir
))
outputMapping$addObservedDataSets(observedData$`AciclovirLaskinData.Laskin 1982.Group A`)
outputMappings <- c(outputMapping)

task <- ParameterIdentification$new(
  simulations = simulations,
  parameters = parameters,
  outputMappings = outputMapping,
  configuration = piConfiguration
)
taskResults <- task$run()

test_that("Optimal lipophilicity value in the aciclovir model is close to expected value of 1.318", {
  expect_equal(taskResults$par, 1.318, tolerance = 0.01)
})
test_that("The hessian value in the aciclovir model is calculated without errors", {
  expect_false(is.na(taskResults$hessian))
})
# I only store the `ofv` column in the snapshot file, because the parameter
# names might have nonstandard symbols disrupting the `expect` function
test_that("The grid calculation (with default parameters) in the aciclovir model returns the expected results", {
  expect_snapshot_value(task$gridSearch()[["ofv"]], style = "serialize")
})
test_that("The profile calculation (with default parameters) in the aciclovir model returns the expected results", {
  expect_snapshot_value(task$calculateOFVProfiles(), style = "serialize")
})
test_that("The profile plot in the aciclovir model returns the expected graphics", {
  vdiffr::expect_doppelganger("ofv-profile-aciclovir", task$plotOFVProfiles(task$calculateOFVProfiles())[[1]])
})

# Load midazolam 2-parameter model and confirm that the optimal parameter values are as expected

simulations <- c(loadSimulation("../dev/Models/Simulations/Smith1981 iv 5mg Midazolam.pkml"))
names(simulations) <- "Midazolam"
piConfiguration <- PIConfiguration$new()
piConfiguration$printEvaluationFeedback <- FALSE

parameterInputData <- list(
  list(path = "Midazolam|Lipophilicity", min = -10, max = 10, start = 3.9),
  list(path = "Midazolam-CYP3A4-Patki et al. 2003 rCYP3A4|kcat", min = 0, max = 3200, start = 320)
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

filePath <- "../data/Midazolam_Smith_1981.xlsx"
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
taskResults <- task$run()

test_that("Optimal lipophilicity value in the midazolam model is close to expected value of 3.36", {
  expect_equal(taskResults$par[[1]], 3.36637, tolerance = 0.01)
})
test_that("Optimal kcat value in the midazolam model is close to expected value of 0.117", {
  expect_equal(taskResults$par[[2]], 0.118855, tolerance = 0.01)
})
test_that("The hessian value in the midazolam model is calculated without errors", {
  expect_false(any(is.na(taskResults$hessian)))
})
test_that("The grid calculation (with the default parameters) in the midazolam model returns the expected results", {
  expect_snapshot_value(task$gridSearch()[["ofv"]], style = "serialize")
})
test_that("The profile calculation (with the default parameters) in the midazolam model returns the expected results", {
  expect_snapshot_value(task$calculateOFVProfiles(), style = "serialize")
})
test_that("The profile plot in the midazolam model returns the expected graphics", {
  profiles <- task$calculateOFVProfiles()
  plots <- task$plotOFVProfiles(profiles)
  vdiffr::expect_doppelganger("ofv-profile-midazolam-1", plots[[1]])
  vdiffr::expect_doppelganger("ofv-profile-midazolam-2", plots[[2]])
})
test_that("The grid plot in the midazolam model returns the expected graphics", {
  grid <- task$gridSearch()
  plot <- task$plotGrid(grid)
  vdiffr::expect_doppelganger("ofv-grid-midazolam", plot)
})
test_that("Starting values are correctly changed after a grid search", {
  old_values <- purrr::map_dbl(task$parameters, ~ .x$startValue)
  grid <- task$gridSearch(setStartingPoint = TRUE)
  new_values <- purrr::map_dbl(task$parameters, ~ .x$startValue)
  expect_equal(old_values, c(3.9, 320), tolerance = 1e-3)
  expect_equal(new_values, c(3.333333, 0), tolerance = 1e-3)
})

# Load clarithromycin 3-parameter model and confirm that the optimal parameter values are as expected
simulations <- c(
  "IV250" = loadSimulation("../dev/Models/Simulations/Chu1992 iv 250mg Clarithromycin.pkml"),
  "PO250" = loadSimulation("../dev/Models/Simulations/Chu1993 po 250mg Clarithromycin.pkml"),
  "PO250MD" = loadSimulation("../dev/Models/Simulations/Chu1993 po 250mg md Clarithromycin.pkml"),
  "PO500" = loadSimulation("../dev/Models/Simulations/Chu1993 po 500mg Clarithromycin.pkml"),
  "PO500MD" = loadSimulation("../dev/Models/Simulations/Chu1993 po 500mg md Clarithromycin.pkml")
)

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
filePath <- "../data/Clarithromycin_Chu_1992.xlsx"
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- "IV250"
dataConfiguration$namingPattern <- "{Sheet}"
observedData_IV <- loadDataSetsFromExcel(xlsFilePath = filePath, importerConfigurationOrPath = dataConfiguration)
filePath <- "../data/Clarithromycin_Chu_1993.xlsx"
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- c("PO250", "PO250MD", "PO500", "PO500MD")
dataConfiguration$namingPattern <- "{Sheet}"
observedData_PO <- loadDataSetsFromExcel(xlsFilePath = filePath, importerConfigurationOrPath = dataConfiguration)
observedData <- c(observedData_IV, observedData_PO)

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
test_that("The grid calculation (with the default parameters) in the clarithromycin model returns the expected results", {
  expect_snapshot_value(task$gridSearch()[["ofv"]], style = "serialize")
})
test_that("The profile calculation (with the default parameters) in the clarithromycin model returns the expected results", {
  expect_snapshot_value(task$calculateOFVProfiles(), style = "serialize")
})
test_that("The profile plot in the clarithromycin model returns the expected graphics", {
  profiles <- task$calculateOFVProfiles()
  plots <- task$plotOFVProfiles(profiles)
  vdiffr::expect_doppelganger("ofv-profile-clarithromycin-1", plots[[1]])
  vdiffr::expect_doppelganger("ofv-profile-clarithromycin-2", plots[[2]])
  vdiffr::expect_doppelganger("ofv-profile-clarithromycin-3", plots[[3]])
})
