##### Steady-state###
devtools::load_all()
simPath <- "D:\\Documents\\esqlabs GmbH\\Research and Development - Documents\\QSP Platforms\\GHM\\V8_00.24\\Models\\Simulations\\GHM.pkml"

simulation <- loadSimulation(filePath = simPath)

#### Profile getAllStateVariablesPaths
system.time({
  stateVariables <- getAllStateVariablesPaths(simulation = simulation)
})
# time: 0.06

###### Profile getOutputValues
quantitiesPaths <- getAllStateVariablesPaths(simulation)
# Add outputs using direct rClr methods
# Have to get the quantity objects to retrieve their "isFormula"-information
task <- ospsuite:::getContainerTask()
method <- ospsuite:::AllMatchingMethod$Quantity
outputSelections <- simulation$outputSelections
for (path in quantitiesPaths) {
  # A path could produce multiple quantities
  netQuantities <- rClr::clrCall(task, method, simulation$ref, enc2utf8(path))
  for (netQuantity in netQuantities) {
    rClr::clrCall(outputSelections$ref, "AddQuantity", netQuantity)
  }
}
simulationResults <- ospsuite::runSimulation(simulation)

# with metadata
profvis::profvis({
  allOutputs <- ospsuite::getOutputValues(simulationResults, quantitiesOrPaths = quantitiesPaths)
})
# time: 1
system.time({
  allOutputs <- ospsuite::getOutputValues(simulationResults, quantitiesOrPaths = quantitiesPaths)
})
# time: 1.45

# without metadata
profvis::profvis({
  allOutputs <- ospsuite::getOutputValues(simulationResults, quantitiesOrPaths = quantitiesPaths, withMetaData = FALSE)
})
# time: 0.5
system.time({
  allOutputs <- ospsuite::getOutputValues(simulationResults, quantitiesOrPaths = quantitiesPaths, withMetaData = FALSE)
})
# time: 0.75

##### Profile getSteadyState###
profvis::profvis({
  steadyState <- getSteadyState(simulation = simulation, steadyStateTime = 1000)
})
# memory: 148, time: 17
system.time({
  steadyState <- getSteadyState(simulation = simulation, steadyStateTime = 1000)
})
# time 26
