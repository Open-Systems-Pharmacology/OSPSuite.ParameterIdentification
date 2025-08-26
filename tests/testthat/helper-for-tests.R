# Simulation, Parameter, and Mapping Factories

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
          path = parameterPath, container = simulation
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
testOutputMappingWithoutObsData <- getTestOutputMapping(includeObservedData = FALSE)


createPiTask <- function() {
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

  ciOptions <- CIOptions_Bootstrap
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
    .evaluate = function(currVals, bootstrapSeed = NULL) {
      private$.fnEvaluations <- private$.fnEvaluations + 2
      stop("Simulated failure in evaluation")
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

  params <- PIParameters$new(parameters = list(
    getParameter("Aciclovir|Lipophilicity", container = sim)
  ))

  ParameterIdentification$new(
    simulations = sim,
    parameters = params,
    outputMappings = mapping
  )
}


# General Helpers

getTestDataFilePath <- function(fileName) {
  dataPath <- testthat::test_path("../data")
  file.path(dataPath, fileName, fsep = .Platform$file.sep)
}

getSimulationFilePath <- function(simulationName) {
  getTestDataFilePath(paste0(simulationName, ".pkml"))
}

loadTestSimulation <- function(simulationName, loadFromCache = FALSE, addToCache = TRUE) {
  simFile <- getSimulationFilePath(simulationName)
  sim <- ospsuite::loadSimulation(
    simFile,
    loadFromCache = loadFromCache, addToCache = addToCache
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

piParameterLipo <- PIParameters$new(parameters = list(
  getParameter(path = "Aciclovir|Lipophilicity", container = sim_250mg),
  getParameter(path = "Aciclovir|Lipophilicity", container = sim_500mg)
))
piParameterLipo_250mg <- PIParameters$new(parameters = list(
  getParameter(path = "Aciclovir|Lipophilicity", container = sim_250mg)
))
piParameterCl_250mg <- PIParameters$new(
  parameters = getParameter(
    path = "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS|TSspec",
    container = sim_250mg
  )
)
piParameterCl_500mg <- PIParameters$new(
  parameters = getParameter(
    path = "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS|TSspec",
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
outputMapping_500mg$addObservedDataSets(
  testObservedData()$`AciclovirLaskinData.Laskin 1982.Group A`
)
