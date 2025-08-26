# -------------------------------------------------------------------------
# Test setup:
# - Model: Clarithromycin (3 parameters, multiple simulations)
# - Scaling: Linear
# - Optimizer: BOBYQA
# - Residual weighting: Default
# - CI method: Hessian-based
# -------------------------------------------------------------------------

simulations <- c(
  "IV250" = loadSimulation(
    system.file(
      "extdata", "Chu1992 iv 250mg Clarithromycin.pkml",
      package = "ospsuite.parameteridentification"
    )
  ),
  "PO250" = loadSimulation(
    system.file(
      "extdata", "Chu1993 po 250mg Clarithromycin.pkml",
      package = "ospsuite.parameteridentification"
    )
  ),
  "PO250MD" = loadSimulation(
    system.file(
      "extdata", "Chu1993 po 250mg md Clarithromycin.pkml",
      package = "ospsuite.parameteridentification"
    )
  ),
  "PO500" = loadSimulation(
    system.file(
      "extdata", "Chu1993 po 250mg Clarithromycin.pkml",
      package = "ospsuite.parameteridentification"
    )
  ),
  "PO500MD" = loadSimulation(
    system.file(
      "extdata", "Chu1993 po 250mg md Clarithromycin.pkml",
      package = "ospsuite.parameteridentification"
    )
  )
)

setParameterValuesByPath(
  "Applications|Clarithromycin po, Chu 1993, 250mg|Tablet Clarithromycin|Application_1|ProtocolSchemaItem|Dose",
  simulation = simulations$PO500,
  values = 500, units = "mg"
)

# Get all `Dose` parameters for the MD simulation
doseParams <- getAllParametersMatching(
  "Applications|**|Dose",
  container = simulations$PO500MD
)

setParameterValues(
  parameters = doseParams, values = rep(500, length(doseParams)),
  units = rep("mg", length(doseParams))
)

piConfiguration <- PIConfiguration$new()
piConfiguration$printEvaluationFeedback <- TRUE

parameterInputData <- list(
  list(path = "Clarithromycin-CYP3A4-fit|kcat", min = 0, max = 100, start = 10),
  list(
    path = paste0(
      "Neighborhoods|Kidney_pls_Kidney_ur|Clarithromycin|Renal Clearances-",
      "fitted|Specific clearance"
    ),
    min = 0, max = 100, start = 10
  ),
  list(
    path = "Clarithromycin|Specific intestinal permeability (transcellular)",
    min = 0, max = 1, start = 0.01
  )
)

# The code below assumes that every parameter is present in each simulation
# and parameter values across all simulations should be changed in parallel
parameters <- vector("list", length = length(parameterInputData))
for (idx in seq_along(parameterInputData)) {
  modelParams <- list()
  for (simulation in simulations) {
    modelParams <- c(
      modelParams,
      ospsuite::getParameter(
        path = parameterInputData[[idx]]$path,
        container = simulation
      )
    )
  }
  parameters[[idx]] <- PIParameters$new(parameters = modelParams)
  parameters[[idx]]$minValue <- parameterInputData[[idx]]$min
  parameters[[idx]]$maxValue <- parameterInputData[[idx]]$max
  parameters[[idx]]$startValue <- parameterInputData[[idx]]$start
}

# Observed data is loaded from two different files because IV data is reported
# in µmol/L, and PO data is reported in µg/ml
filePath <- system.file(
  "extdata", "Clarithromycin_Profiles.xlsx",
  package = "ospsuite.parameteridentification"
)

dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- c("IV250", "PO250", "PO250MD", "PO500", "PO500MD")
dataConfiguration$namingPattern <- "{Sheet}"

observedData <- loadDataSetsFromExcel(
  xlsFilePath = filePath, importerConfigurationOrPath = dataConfiguration
)

# The code below assumes each simulation is matched against one sheet in
# observed data
outputMappings <- vector("list", length = length(simulations))
for (idx in seq_along(simulations)) {
  outputMappings[[idx]] <- PIOutputMapping$new(
    quantity = getQuantity(
      "Organism|PeripheralVenousBlood|Clarithromycin|Plasma (Peripheral Venous Blood)",
      container = simulations[[idx]]
    )
  )
  outputMappings[[idx]]$addObservedDataSets(
    observedData[[names(simulations)[[idx]]]]
  )
  outputMappings[[idx]]$scaling <- "lin"
}

task <- ParameterIdentification$new(
  simulations = simulations,
  parameters = parameters,
  outputMappings = outputMappings,
  configuration = piConfiguration
)

piResult <- task$run()

test_that("Estimated parameters equal expected values", {
  expect_equal(
    piResult$toList()$finalParameters, c(14.024, 6.244, 1.3e-6),
    tolerance = 0.01
  )
})

test_that("Hessian CI estimation returns expected error message", {
  expect_match(
    piResult$toList()$ciError,
    "Error during CI estimation step 'Covariance matrix validity check'",
    fixed = TRUE
  )
})


# -------------------------------------------------------------------------
# Test setup:
# - Model: Clarithromycin (3 parameters, multiple simulations)
# - Scaling: Logarithmic
# - Optimizer: BOBYQA
# - Residual weighting: Default
# - CI method: Hessian-based
# -------------------------------------------------------------------------

simulations <- c(
  "IV250" = loadSimulation(
    system.file(
      "extdata", "Chu1992 iv 250mg Clarithromycin.pkml",
      package = "ospsuite.parameteridentification"
    )
  ),
  "PO250" = loadSimulation(
    system.file(
      "extdata", "Chu1993 po 250mg Clarithromycin.pkml",
      package = "ospsuite.parameteridentification"
    )
  ),
  "PO250MD" = loadSimulation(
    system.file(
      "extdata", "Chu1993 po 250mg md Clarithromycin.pkml",
      package = "ospsuite.parameteridentification"
    )
  ),
  "PO500" = loadSimulation(
    system.file(
      "extdata", "Chu1993 po 250mg Clarithromycin.pkml",
      package = "ospsuite.parameteridentification"
    )
  ),
  "PO500MD" = loadSimulation(
    system.file(
      "extdata", "Chu1993 po 250mg md Clarithromycin.pkml",
      package = "ospsuite.parameteridentification"
    )
  )
)

setParameterValuesByPath(
  "Applications|Clarithromycin po, Chu 1993, 250mg|Tablet Clarithromycin|Application_1|ProtocolSchemaItem|Dose",
  simulation = simulations$PO500,
  values = 500, units = "mg"
)

# Get all `Dose` parameters for the MD simulation
doseParams <- getAllParametersMatching(
  "Applications|**|Dose",
  container = simulations$PO500MD
)

setParameterValues(
  parameters = doseParams, values = rep(500, length(doseParams)),
  units = rep("mg", length(doseParams))
)

piConfiguration <- PIConfiguration$new()
piConfiguration$printEvaluationFeedback <- TRUE

parameterInputData <- list(
  list(path = "Clarithromycin-CYP3A4-fit|kcat", min = 0, max = 100, start = 10),
  list(
    path = paste0(
      "Neighborhoods|Kidney_pls_Kidney_ur|Clarithromycin|Renal Clearances-",
      "fitted|Specific clearance"
    ),
    min = 0, max = 100, start = 10
  ),
  list(
    path = "Clarithromycin|Specific intestinal permeability (transcellular)",
    min = 0, max = 1, start = 0.01
  )
)

# The code below assumes that every parameter is present in each simulation
# and parameter values across all simulations should be changed in parallel
parameters <- vector("list", length = length(parameterInputData))
for (idx in seq_along(parameterInputData)) {
  modelParams <- list()
  for (simulation in simulations) {
    modelParams <- c(
      modelParams,
      ospsuite::getParameter(
        path = parameterInputData[[idx]]$path,
        container = simulation
      )
    )
  }
  parameters[[idx]] <- PIParameters$new(parameters = modelParams)
  parameters[[idx]]$minValue <- parameterInputData[[idx]]$min
  parameters[[idx]]$maxValue <- parameterInputData[[idx]]$max
  parameters[[idx]]$startValue <- parameterInputData[[idx]]$start
}

# Observed data is loaded from two different files because IV data is reported
# in µmol/L, and PO data is reported in µg/ml
filePath <- system.file(
  "extdata", "Clarithromycin_Profiles.xlsx",
  package = "ospsuite.parameteridentification"
)

dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- c("IV250", "PO250", "PO250MD", "PO500", "PO500MD")
dataConfiguration$namingPattern <- "{Sheet}"

observedData <- loadDataSetsFromExcel(
  xlsFilePath = filePath, importerConfigurationOrPath = dataConfiguration
)

# The code below assumes each simulation is matched against one sheet in
# observed data
outputMappings <- vector("list", length = length(simulations))
for (idx in seq_along(simulations)) {
  outputMappings[[idx]] <- PIOutputMapping$new(
    quantity = getQuantity(
      "Organism|PeripheralVenousBlood|Clarithromycin|Plasma (Peripheral Venous Blood)",
      container = simulations[[idx]]
    )
  )
  outputMappings[[idx]]$addObservedDataSets(
    observedData[[names(simulations)[[idx]]]]
  )
  outputMappings[[idx]]$scaling <- "log"
}

task <- ParameterIdentification$new(
  simulations = simulations,
  parameters = parameters,
  outputMappings = outputMappings,
  configuration = piConfiguration
)

piResult <- task$run()

test_that("Estimated parameters equal expected values", {
  expect_equal(
    piResult$toList()$finalParameters, c(8.420, 5.373, 0.011),
    tolerance = 0.01
  )
})

test_that("Hessian-based standard deviations equal expected values", {
  expect_equal(piResult$toList()$sd, c(2.308, 0.893, 0.002), tolerance = 0.01)
})
