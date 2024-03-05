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
    simulations = simulations,
    parameters = parameters,
    outputMappings = outputMapping,
    configuration = NULL
  )
  return(piTask)
}
