##### Steady-state###
devtools::load_all()
simPath <- "D:\\Documents\\esqlabs GmbH\\Research and Development - Documents\\QSP Platforms\\GHM\\V8_00.24\\Models\\Simulations\\GHM.pkml"

simulation <- loadSimulation(filePath = simPath)
sim2 <- loadSimulation(filePath = simPath)

##### Profile getSteadyState###
profvis::profvis({
  steadyState <- getSteadyState(simulations = simulation, steadyStateTime = 1000)
})
# memory: 148, time: 17
system.time({
  steadyState <- getSteadyState(simulations = c(simulation, sim2), steadyStateTime = 1000)
})
# time 26


#####Profile runSimulation vs runSimulationsConcurrently
system.time({
  res <- runSimulation(simulation = simulation)
})
#31; 28.8; 32.8; 29.9; 30

system.time({
  res <- runSimulationsConcurrently(simulations = simulation)
})
#31.9
