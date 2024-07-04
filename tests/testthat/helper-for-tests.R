# Function factories to create object only the first time they are accessed

## Simulation function factory
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

## observedData function factory
getTestObservedData <- function() {
  .observedData <- NULL
  function() {
    if (is.null(.observedData)) {
      filePath <- testthat::test_path("../data/AciclovirLaskinData.xlsx")
      dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
      dataConfiguration$sheets <- "Laskin 1982.Group A"
      dataConfiguration$namingPattern <- "{Source}.{Sheet}"
      .observedData <<- loadDataSetsFromExcel(
        xlsFilePath = filePath,
        importerConfigurationOrPath = dataConfiguration
      )
    }
    return(.observedData)
  }
}

testObservedData <- getTestObservedData()

## Parameters function factory
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


## OutputMapping function factory
getTestOutputMapping <- function() {
  .outputMapping <- NULL
  function() {
    if (is.null(.outputMapping)) {
      outputMapping <- PIOutputMapping$new(
        quantity = getQuantity("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
          container = testSimulations()$Aciclovir
        )
      )
      outputMapping$addObservedDataSets(testObservedData()$`AciclovirLaskinData.Laskin 1982.Group A`)
      .outputMapping <<- list(outputMapping)
    }
    return(.outputMapping)
  }
}

testOutputMapping <- getTestOutputMapping()


# PI multiple simulations and parameter paths

sim_250mg <- loadSimulation(
  system.file("extdata", "Aciclovir.pkml", package = "ospsuite"))
sim_500mg <- loadSimulation(
  system.file("extdata", "Aciclovir.pkml", package = "ospsuite"))

piParameterLipo <- PIParameters$new(parameters = list(
  getParameter(path = "Aciclovir|Lipophilicity", container = sim_250mg),
  getParameter(path = "Aciclovir|Lipophilicity", container = sim_500mg)
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

# outputMapping_250mg <- testOutputMapping()
# outputMapping_500mg <- testOutputMapping()

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

# Variables

testQuantity <- ospsuite::getQuantity(
  path = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  container = testSimulation()
)
testParam <- ospsuite::getParameter("Aciclovir|Permeability", testSimulation())
refVal <- testParam$value

piConfiguration <- PIConfiguration$new()


# Helper functions

getTestDataFilePath <- function(fileName) {
  dataPath <- testthat::test_path("../data")
  file.path(dataPath, fileName, fsep = .Platform$file.sep)
}

getSimulationFilePath <- function(simulationName) {
  getTestDataFilePath(paste0(simulationName, ".pkml"))
}

# Helper function to load a model easily. In the test environment, we do not
# want to load from cache by default. Instead new instances should be created
# unless specifically specified otherwise
loadTestSimulation <- function(simulationName, loadFromCache = FALSE, addToCache = TRUE) {
  simFile <- getSimulationFilePath(simulationName)
  sim <- ospsuite::loadSimulation(simFile, loadFromCache = loadFromCache, addToCache = addToCache)
}

executeWithTestFile <- function(actionWithFile) {
  newFile <- tempfile()
  actionWithFile(newFile)
  file.remove(newFile)
}

# Helper function to create a parameter identification task
# Start with clean configuration state to avoid side effects from shared configurations
createPiTask <- function() {
  piTask <- ParameterIdentification$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = testOutputMapping(),
    configuration = NULL
  )
  return(piTask)
}
