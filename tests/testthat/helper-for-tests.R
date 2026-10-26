# Simulation, Parameter, and Mapping Factories

# Path of the dose parameter in the Aciclovir.pkml shipped with ospsuite 12.4.3
# or later. The file of ospsuite 13 has a "No formulation" container in the
# event path, which the "**" wildcard matches, so the tests run with ospsuite
# 12.4.3 and 13. Before 12.4.3, the dose is below "Applications". A path
# with a wildcard works with getParameter(), but not with
# setParameterValuesByPath().
testDosePath <- "Events|IV 250mg 10min|**|Application_1|ProtocolSchemaItem|Dose"

# Makes the simulation fail when a variable becomes negative. ospsuite 13 has
# this setting in the solver of the simulation. ospsuite 12 has it in
# SimulationRunOptions, where it is on by default.
enableNegativeValuesCheck <- function(simulation) {
  if ("checkForNegativeValues" %in% names(simulation$solver)) {
    simulation$solver$checkForNegativeValues <- TRUE
  }
  invisible(simulation)
}

getTestSimulation <- function() {
  .simulation <- NULL
  function() {
    if (is.null(.simulation)) {
      .simulation <<- loadSimulation(
        system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
      )
    }
    return(.simulation)
  }
}

testSimulation <- getTestSimulation()


getTestParameters <- function() {
  .parameters <- NULL
  .simulation <- NULL
  function(simulation = NULL) {
    if (!is.null(simulation)) {
      parameterPaths <- c("Aciclovir|Lipophilicity")
      parameters <- list()
      for (parameterPath in parameterPaths) {
        param <- ospsuite::getParameter(
          path = parameterPath,
          container = simulation
        )
        piParameter <- PIParameters$new(parameters = list(param))
        piParameter$minValue <- -10
        piParameter$maxValue <- 10
        parameters <- c(parameters, piParameter)
      }
      return(parameters)
    }

    if (is.null(.parameters)) {
      .simulation <<- testSimulation()
      .parameters <<- Recall(simulation = .simulation)
    }

    return(.parameters)
  }
}

testParameters <- getTestParameters()


getTestPKParameters <- function() {
  .parameters <- NULL
  .simulation <- NULL
  function(simulation = NULL) {
    if (!is.null(simulation)) {
      param <- ospsuite::getParameter(
        path = testDosePath,
        container = simulation
      )
      piParameter <- PIParameters$new(parameters = list(param))
      piParameter$minValue <- 0.0001
      piParameter$maxValue <- 0.001
      return(piParameter)
    }

    if (is.null(.parameters)) {
      .simulation <<- testSimulation()
      .parameters <<- Recall(simulation = .simulation)
    }

    return(.parameters)
  }
}

testPKParameters <- getTestPKParameters()


getTestOutputMapping <- function(includeObservedData = TRUE) {
  .outputMapping <- NULL
  .simulation <- NULL
  function(simulation = NULL) {
    if (!is.null(simulation)) {
      quantity <- getQuantity(
        "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
        container = simulation
      )
      mapping <- PIOutputMapping$new(quantity = quantity)
      if (includeObservedData) {
        mapping$addObservedDataSets(
          testObservedData()$`AciclovirLaskinData.Laskin 1982.Group A`
        )
        # Laskin 1982, group A: 13 subjects
        mapping$setSubjectCount(13)
      }
      return(list(mapping))
    }

    if (is.null(.outputMapping)) {
      .simulation <<- testSimulation()
      .outputMapping <<- Recall(simulation = .simulation)
    }

    return(.outputMapping)
  }
}

testOutputMapping <- getTestOutputMapping()
testOutputMappingWithoutObsData <- getTestOutputMapping(
  includeObservedData = FALSE
)


testPiTask <- function() {
  testSimulation <- getTestSimulation()
  sim <- testSimulation()
  testParameters <- getTestParameters()
  testOutputMapping <- getTestOutputMapping()

  ParameterIdentification$new(
    simulations = sim,
    parameters = testParameters(sim),
    outputMappings = testOutputMapping(sim),
    configuration = NULL
  )
}


resetTestFactories <- function() {
  testSimulation <<- getTestSimulation()
  testParameters <<- getTestParameters()
  testPKParameters <<- getTestPKParameters()
  testOutputMapping <<- getTestOutputMapping()
}


# Observed Data Generators

testObservedData <- function() {
  filePath <- testthat::test_path("../data/AciclovirLaskinData.xlsx")
  dataConfig <- createImporterConfigurationForFile(filePath)
  dataConfig$sheets <- "Laskin 1982.Group A"
  dataConfig$namingPattern <- "{Source}.{Sheet}"
  loadDataSetsFromExcel(filePath, dataConfig)
}

syntheticObservedData <- function() {
  filePath <- testthat::test_path("../data/AciclovirDataIndividuals.xlsx")
  dataConfig <- createImporterConfigurationForFile(filePath)
  dataConfig$sheets <- "Aciclovir.Synthetic"
  dataConfig$namingPattern <- "{Source}.{Sheet}.{Subject Id}"
  dataConfig$errorColumn <- NULL
  loadDataSetsFromExcel(
    xlsFilePath = filePath,
    importerConfigurationOrPath = dataConfig
  )
}

# Two aggregated studies with the same geometric means and GSDs on one mapping
aggregatedStudiesMapping <- function() {
  mapping <- PIOutputMapping$new(quantity = testQuantity())
  for (study in c("studyA", "studyB")) {
    dataSet <- ospsuite::DataSet$new(name = study)
    dataSet$setValues(
      xValues = c(1, 2),
      yValues = c(10, 20),
      yErrorValues = c(1.5, 1.5)
    )
    dataSet$yDimension <- ospsuite::ospDimensions$`Concentration (mass)`
    dataSet$yUnit <- "mg/l"
    dataSet$yErrorType <- "GeometricStdDev"
    mapping$addObservedDataSets(dataSet)
  }
  mapping
}

testObservedDataMultiple <- function() {
  filePath <- testthat::test_path("../data/AciclovirLaskinData.xlsx")
  dataConfig <- createImporterConfigurationForFile(filePath)
  dataConfig$sheets <- "Laskin 1982.Group A"
  dataConfig$namingPattern <- "{Source}.{Sheet}"

  dataSet1 <- loadDataSetsFromExcel(filePath, dataConfig)[[1]]
  dataSet1$name <- "dataSet1"

  dataSet2 <- DataSet$new(name = "dataSet2")
  dataSet2$setValues(
    xValues = dataSet1$xValues[-length(dataSet1$xValues)],
    yValues = 1.5 * dataSet1$yValues[-length(dataSet1$yValues)],
    yErrorValues = dataSet1$yErrorValues[-length(dataSet1$yErrorValues)]
  )
  dataSet2$yErrorType <- dataSet1$yErrorType

  list(dataSet1 = dataSet1, dataSet2 = dataSet2)
}


# PIConfiguration Generators

lowIterPiConfiguration <- function(iter = 2) {
  options <- AlgorithmOptions_BOBYQA
  options$maxeval <- iter

  config <- PIConfiguration$new()
  config$algorithmOptions <- options
  config
}

bootstrapPiConfiguration <- function(iter = 2, nBootstrap = 3) {
  options <- AlgorithmOptions_BOBYQA
  options$maxeval <- iter

  ciOptions <- CIOptions_bootstrap
  ciOptions$seed <- 2203
  ciOptions$nBootstrap <- nBootstrap

  config <- PIConfiguration$new()
  config$ciMethod <- "bootstrap"
  config$algorithmOptions <- options
  config$ciOptions <- ciOptions
  config
}


# Specialized Test Classes

PISimFailureTester <- R6::R6Class(
  inherit = ParameterIdentification,
  cloneable = FALSE,
  private = list(
    .simulated = FALSE,
    # The start values simulate, every later simulation fails
    .simulateOutputs = function(currVals, outputMappings = NULL) {
      if (!private$.simulated) {
        private$.simulated <- TRUE
        return(super$.simulateOutputs(currVals))
      }
      stop(.simulationsFailedError(
        "Aciclovir",
        1L,
        "Simulated failure in evaluation"
      ))
    }
  )
)

PIResampleTester <- R6::R6Class(
  inherit = ParameterIdentification,
  cloneable = FALSE,
  private = list(
    # Override to retain modified outputMappings after bootstrap
    .restoreOutputMappingsState = function() {
      return(NULL)
    }
  )
)


# PI Helpers

testQuantity <- function(simulation = testSimulation()) {
  ospsuite::getQuantity(
    path = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    container = simulation
  )
}

testModifiedTask <- function() {
  sim <- loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  )
  sim$solver$mxStep <- 1

  mapping <- PIOutputMapping$new(
    quantity = getQuantity(
      "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      container = sim
    )
  )
  mapping$addObservedDataSets(testObservedData())
  mapping$setSubjectCount(13)

  params <- PIParameters$new(
    parameters = list(
      getParameter("Aciclovir|Lipophilicity", container = sim)
    )
  )

  ParameterIdentification$new(
    simulations = sim,
    parameters = params,
    outputMappings = mapping
  )
}

# A task whose observed data cannot be converted to the unit of the output:
# the molecular weight of the mass concentrations, which are mapped to a molar
# output, is removed after the data set was added to the mapping
testUnconvertibleDataTask <- function() {
  sim <- ospsuite::loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite"),
    loadFromCache = FALSE,
    addToCache = FALSE
  )
  dataSet <- testObservedData()$`AciclovirLaskinData.Laskin 1982.Group A`
  mapping <- PIOutputMapping$new(quantity = testQuantity(sim))
  mapping$addObservedDataSets(dataSet)
  dataSet$molWeight <- NA_real_

  ParameterIdentification$new(
    simulations = sim,
    parameters = testParameters(sim),
    outputMappings = mapping,
    configuration = lowIterPiConfiguration()
  )
}

# A task with two copies of the Aciclovir simulation, which share their name,
# one lipophilicity parameter over both, and the same data mapped to each.
# The simulations at the positions `failing` fail.
testTwoSimulationsTask <- function(failing = integer()) {
  simulations <- lapply(1:2, function(idx) {
    ospsuite::loadSimulation(
      system.file("extdata", "Aciclovir.pkml", package = "ospsuite"),
      loadFromCache = FALSE,
      addToCache = FALSE
    )
  })
  for (idx in failing) {
    simulations[[idx]]$solver$mxStep <- 1
  }

  ParameterIdentification$new(
    simulations = simulations,
    parameters = PIParameters$new(
      parameters = lapply(simulations, function(simulation) {
        ospsuite::getParameter("Aciclovir|Lipophilicity", simulation)
      })
    ),
    outputMappings = lapply(simulations, function(simulation) {
      mapping <- PIOutputMapping$new(quantity = testQuantity(simulation))
      mapping$addObservedDataSets(testObservedData())
      mapping$setSubjectCount(13)
      mapping
    })
  )
}

# A task with the intravenous ("IV250") and the oral ("PO250") Clarithromycin
# simulations of the package, or with one of them, each with its own observed
# data. The output mappings come in the reverse order of the simulations. The
# first parameter group spans both simulations, the second spans them in the
# reverse order, and the third belongs to the oral simulation only. A task
# with one simulation has the groups of that simulation.
testClarithromycinTask <- function(simulationNames = c("IV250", "PO250")) {
  files <- c(
    IV250 = "Clarithromycin_Chu_1992_iv_250mg.pkml",
    PO250 = "Clarithromycin_Chu_1993_po_250mg.pkml"
  )
  simulations <- lapply(files[simulationNames], function(file) {
    ospsuite::loadSimulation(
      system.file(
        "extdata",
        file,
        package = "ospsuite.parameteridentification"
      ),
      loadFromCache = FALSE,
      addToCache = FALSE
    )
  })

  dataFile <- system.file(
    "extdata",
    "Clarithromycin_Profiles.xlsx",
    package = "ospsuite.parameteridentification"
  )
  dataConfig <- ospsuite::createImporterConfigurationForFile(dataFile)
  dataConfig$sheets <- simulationNames
  dataConfig$namingPattern <- "{Sheet}"
  observedData <- ospsuite::loadDataSetsFromExcel(dataFile, dataConfig)

  group <- function(path, groupSimulations) {
    groupSimulations <- intersect(groupSimulations, simulationNames)
    if (length(groupSimulations) == 0) {
      return(NULL)
    }
    PIParameters$new(
      parameters = lapply(simulations[groupSimulations], function(simulation) {
        ospsuite::getParameter(path, simulation)
      })
    )
  }
  parameters <- list(
    group("Clarithromycin-CYP3A4-fit|kcat", c("IV250", "PO250")),
    group(
      paste0(
        "Neighborhoods|Kidney_pls_Kidney_ur|Clarithromycin|",
        "Renal Clearances-fitted|Specific clearance"
      ),
      c("PO250", "IV250")
    ),
    group("Clarithromycin|Lipophilicity", "PO250")
  )

  outputPath <- paste0(
    "Organism|PeripheralVenousBlood|Clarithromycin|",
    "Plasma (Peripheral Venous Blood)"
  )
  ParameterIdentification$new(
    simulations = unname(simulations),
    parameters = Filter(Negate(is.null), parameters),
    outputMappings = lapply(rev(simulationNames), function(name) {
      mapping <- PIOutputMapping$new(
        quantity = ospsuite::getQuantity(outputPath, simulations[[name]])
      )
      mapping$addObservedDataSets(observedData[[name]])
      mapping
    })
  )
}


# General Helpers

# Counts the reads of the observed data of an output mapping, that is the
# calls of `.prepareObservedData()`, until the end of the calling test. The
# count is in `$reads` of the returned environment.
localObservedDataReads <- function(env = parent.frame()) {
  counter <- new.env()
  counter$reads <- 0
  prepareObservedData <- ospsuite.parameteridentification:::.prepareObservedData
  testthat::local_mocked_bindings(
    .prepareObservedData = function(outputMapping) {
      counter$reads <- counter$reads + 1
      prepareObservedData(outputMapping)
    },
    .env = env
  )
  counter
}

parseFnevals <- function(output) {
  text <- paste0(output, collapse = "\n")
  as.integer(regmatches(
    text,
    gregexpr("(?<=fneval: )\\d+", text, perl = TRUE)
  )[[1]])
}

getTestDataFilePath <- function(fileName) {
  dataPath <- testthat::test_path("../data")
  file.path(dataPath, fileName, fsep = .Platform$file.sep)
}

getSimulationFilePath <- function(simulationName) {
  getTestDataFilePath(paste0(simulationName, ".pkml"))
}

loadTestSimulation <- function(
  simulationName,
  loadFromCache = FALSE,
  addToCache = TRUE
) {
  simFile <- getSimulationFilePath(simulationName)
  sim <- ospsuite::loadSimulation(
    simFile,
    loadFromCache = loadFromCache,
    addToCache = addToCache
  )
}

executeWithTestFile <- function(actionWithFile) {
  newFile <- tempfile()
  actionWithFile(newFile)
  file.remove(newFile)
}


# Multi-Simulation Setup

sim_250mg <- loadSimulation(
  system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
)
sim_500mg <- loadSimulation(
  system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
)

piParameterLipo <- PIParameters$new(
  parameters = list(
    getParameter(path = "Aciclovir|Lipophilicity", container = sim_250mg),
    getParameter(path = "Aciclovir|Lipophilicity", container = sim_500mg)
  )
)
piParameterLipo_250mg <- PIParameters$new(
  parameters = list(
    getParameter(path = "Aciclovir|Lipophilicity", container = sim_250mg)
  )
)
piParameterCl_250mg <- PIParameters$new(
  parameters = getParameter(
    path = "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS-Aciclovir|TSspec",
    container = sim_250mg
  )
)
piParameterCl_500mg <- PIParameters$new(
  parameters = getParameter(
    path = "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS-Aciclovir|TSspec",
    container = sim_500mg
  )
)

simOutputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
outputMapping_250mg <- PIOutputMapping$new(
  quantity = getQuantity(path = simOutputPath, container = sim_250mg)
)
outputMapping_500mg <- PIOutputMapping$new(
  quantity = getQuantity(path = simOutputPath, container = sim_500mg)
)
outputMapping_250mg$addObservedDataSets(
  testObservedData()$`AciclovirLaskinData.Laskin 1982.Group A`
)
outputMapping_250mg$setSubjectCount(13)
outputMapping_500mg$addObservedDataSets(
  testObservedData()$`AciclovirLaskinData.Laskin 1982.Group A`
)
outputMapping_500mg$setSubjectCount(13)

# State-variable parameter fixtures

# Aciclovir state-variable (RHS-defined) parameter, dimension Volume (~0.045 L).
stateVariableParameterPath <- "Organism|Lumen|Stomach|Liquid"

# Bounded `PIParameters` wrapping the state-variable parameter, reused by the
# state-variable fixtures and tests.
stateVarPIParameter <- function(sim) {
  param <- PIParameters$new(
    parameters = list(getParameter(stateVariableParameterPath, container = sim))
  )
  param$minValue <- 0.01
  param$maxValue <- 0.1
  param
}

# PI task mixing one state-variable parameter with one constant parameter.
# Two separate PIParameters objects are required because a single group must
# share a dimension (Volume vs dimensionless).
testStateVariableMixedTask <- function() {
  sim <- loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  )

  stateVarParam <- stateVarPIParameter(sim)

  constParam <- PIParameters$new(
    parameters = list(getParameter("Aciclovir|Lipophilicity", container = sim))
  )
  constParam$minValue <- -10
  constParam$maxValue <- 10

  mapping <- PIOutputMapping$new(
    quantity = getQuantity(
      "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      container = sim
    )
  )
  mapping$addObservedDataSets(
    testObservedData()$`AciclovirLaskinData.Laskin 1982.Group A`
  )
  mapping$setSubjectCount(13)

  ParameterIdentification$new(
    simulations = sim,
    parameters = list(stateVarParam, constParam),
    outputMappings = mapping
  )
}

# PI task whose only optimization parameter is state-variable, so that
# .variableParameters stays empty for the simulation (empty-parametersOrPaths
# shadow path).
testStateVariableOnlyTask <- function() {
  sim <- loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  )

  stateVarParam <- stateVarPIParameter(sim)

  mapping <- PIOutputMapping$new(
    quantity = getQuantity(
      "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      container = sim
    )
  )
  mapping$addObservedDataSets(
    testObservedData()$`AciclovirLaskinData.Laskin 1982.Group A`
  )
  mapping$setSubjectCount(13)

  ParameterIdentification$new(
    simulations = sim,
    parameters = stateVarParam,
    outputMappings = mapping
  )
}
