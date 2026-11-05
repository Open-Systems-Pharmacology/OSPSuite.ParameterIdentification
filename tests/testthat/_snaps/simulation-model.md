# the constructor stops when a simulation has no parameter of the task

    Code
      SimulationModel$new(simulations = simulations, parameters = list(
        testPKParameters(simulations[[1]])), pkMappings = list(pkMapping(simulations[[
        1]])))
    Condition
      Error in `.validateSimulationIds()`:
      ! Mismatch or missing ID detected.
      Ensure each Simulation ID matches with corresponding PIParameter IDs.
      Simulation IDs: <id>, <id>
      PIParameter IDs: <id>

# the constructor validates its inputs and the fields are read-only

    Code
      model$simulationBatches <- NULL
    Condition
      Error:
      ! Property $simulationBatches is readonly

---

    Code
      model$loggedFailures <- 1
    Condition
      Error:
      ! Property $loggedFailures is readonly

