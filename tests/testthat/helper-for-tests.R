# Simulation function factory
getTestSimulation <- function() {
  .simulation <- NULL
  function() {
    if (is.null(.simulation)) {
      .simulation <<- loadSimulation(system.file("extdata", "Aciclovir.pkml", package = "ospsuite"))
    }
    return(.simulation)
  }
}

testSimulation <- getTestSimulation()

testSimulations <- function() {
  list("Aciclovir" = testSimulation())
}

# Observed data function factory
getTestObservedData <- function() {
  .observedData <- NULL
  function() {
    if (is.null(.observedData)) {
      filePath <- testthat::test_path("../data/AciclovirLaskinData.xlsx")
      dataConfig <- createImporterConfigurationForFile(filePath)
      dataConfig$sheets <- "Laskin 1982.Group A"
      dataConfig$namingPattern <- "{Source}.{Sheet}"
      .observedData <<- loadDataSetsFromExcel(
        xlsFilePath = filePath,
        importerConfigurationOrPath = dataConfig
      )
    }
    return(.observedData)
  }
}

testObservedData <- getTestObservedData()

# Observed data function factory - synthetic individual data
getSyntheticObservedData <- function() {
  .syntheticObservedData <- NULL
  function() {
    if (is.null(.syntheticObservedData)) {
      filePath <- testthat::test_path("../data/AciclovirDataIndividuals.xlsx")
      dataConfig <- createImporterConfigurationForFile(filePath)
      dataConfig$sheets <- "Aciclovir.Synthetic"
      dataConfig$namingPattern <- "{Source}.{Sheet}.{Subject Id}"
      dataConfig$errorColumn <- NULL
      .syntheticObservedData <<- loadDataSetsFromExcel(
        xlsFilePath = filePath,
        importerConfigurationOrPath = dataConfig
      )
    }
    return(.syntheticObservedData)
  }
}

syntheticObservedData <- getSyntheticObservedData()

# Observed data function factory - multiple data sets
getTestObservedDataMultiple <- function() {
  .observedDataMultiple <- NULL
  dataSet1 <- dataSet2 <- NULL
  function() {
    if (is.null(.observedDataMultiple)) {
      filePath <- testthat::test_path("../data/AciclovirLaskinData.xlsx")
      dataConfig <- createImporterConfigurationForFile(filePath)
      dataConfig$sheets <- "Laskin 1982.Group A"
      dataConfig$namingPattern <- "{Source}.{Sheet}"
      dataSet1 <- loadDataSetsFromExcel(
        xlsFilePath = filePath,
        importerConfigurationOrPath = dataConfig
      )[[1]]
      dataSet1$name <- "dataSet1"

      dataSet2 <- DataSet$new(name = "dataSet2")
      dataSet2$setValues(
        xValues = dataSet1$xValues[-length(dataSet1$xValues)],
        yValues = 1.5 * dataSet1$yValues[-length(dataSet1$yValues)],
        yErrorValues = dataSet1$yErrorValues[-length(dataSet1$yErrorValues)]
      )
      dataSet2$yErrorType <- dataSet1$yErrorType

      .observedDataMultiple <- list(dataSet1 = dataSet1, dataSet2 = dataSet2)
    }
    return(.observedDataMultiple)
  }
}

testObservedDataMultiple <- getTestObservedDataMultiple()

# Parameters function factory
getTestParameters <- function() {
  .parameters <- NULL
  function() {
    if (is.null(.parameters)) {
      parameterPaths <- c("Aciclovir|Lipophilicity")
      parameters <- list()
      for (parameterPath in parameterPaths) {
        modelParams <- list()
        for (simulation in testSimulations()) {
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

      .parameters <<- parameters
    }
    return(.parameters)
  }
}

testParameters <- getTestParameters()

# OutputMapping function factory
getTestOutputMapping <- function(includeObservedData = TRUE) {
  .outputMapping <- NULL
  function() {
    if (is.null(.outputMapping)) {
      mapping <- PIOutputMapping$new(
        quantity = getQuantity(
          "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
          container = testSimulations()$Aciclovir
        )
      )
      if (includeObservedData) {
        mapping$addObservedDataSets(
          testObservedData()$`AciclovirLaskinData.Laskin 1982.Group A`
        )
      }
      .outputMapping <<- list(mapping)
    }
    return(.outputMapping)
  }
}

testOutputMapping <- getTestOutputMapping()
testOutputMappingWithoutObsData <- getTestOutputMapping(includeObservedData = FALSE)

# Low iteration PIConfiguration for BOBYQA
getLowIterPiConfiguration <- function(iter = 2) {
  .configuration <- NULL
  function() {
    if (is.null(.configuration)) {
      options <- AlgorithmOptions_BOBYQA
      options$maxeval <- iter
      .configuration <- PIConfiguration$new()
      .configuration$algorithmOptions <- options
    }
    return(.configuration)
  }
}

lowIterPiConfiguration <- getLowIterPiConfiguration()

# Configuration for bootstrap CI estimation
getBootstrapPiConfiguration <- function(iter = 2, nBootstrap = 3) {
  .bsConfiguration <- NULL
  function() {
    if (is.null(.bsConfiguration)) {
      options <- AlgorithmOptions_BOBYQA
      options$maxeval <- iter
      ciOptions <- CIOptions_Bootstrap
      ciOptions$seed <- 2203
      ciOptions$nBootstrap <- nBootstrap

      .bsConfiguration <- PIConfiguration$new()
      .bsConfiguration$ciMethod <- "bootstrap"
      .bsConfiguration$algorithmOptions <- options
      .bsConfiguration$ciOptions <- ciOptions
    }
    return(.bsConfiguration)
  }
}

bootstrapPiConfiguration <- getBootstrapPiConfiguration()

# Function to create a ParameterIdentification task
createPiTask <- function() {
  ParameterIdentification$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = testOutputMapping(),
    configuration = NULL
  )
}

# Function to create a modified ParameterIdentification task (simulation failure)
getTestModifiedTask <- function() {
  function() {
    sim <- loadSimulation(system.file("extdata", "Aciclovir.pkml", package = "ospsuite"))
    sim$solver$mxStep <- 1
    mapping <- PIOutputMapping$new(
      quantity = getQuantity("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)", container = sim)
    )
    mapping$addObservedDataSets(testObservedData())

    params <- PIParameters$new(parameters = list(
      getParameter("Aciclovir|Lipophilicity", container = sim)
    ))

    task <- ParameterIdentification$new(
      simulations = sim,
      parameters = params,
      outputMappings = mapping
    )
    return(task)
  }
}

testModifiedTask <- getTestModifiedTask()

# Multiple simulation and parameter paths
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

# Other variables
testQuantity <- ospsuite::getQuantity(
  path = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  container = testSimulation()
)

# Other variables - weights for bootstrap CI method
weights <- rep(list(rep(2, 11)), 4) |>
  c(list(c(0, 1, 1, 2, 2, 1, 1, 1, 1, 1, 0.5))) |>
  setNames(names(syntheticObservedData()))

# Test classes
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
    # outputMappings are not restored and sampled weights stay accessible
    .restoreOutputMappingsState = function() {
      return(NULL)
    }
  )
)

# Helper functions
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
