# the PK outputs are selected around the analyses and restored

    Code
      withPKOutputs(task, objective$pkValues(startValues(task)))
    Condition
      Error in `ospsuite::calculatePKAnalyses()`:
      ! Injected PK failure

# PKObjective validates the model and the mappings

    Code
      PKObjective$new(model = list(1), pkMappings = task$pkOutputMappings)
    Condition
      Error in `ospsuite.utils::validateIsOfType()`:
      ! `<caller>()`: argument "model" is of type <list>, but expected <SimulationModel>!

---

    Code
      PKObjective$new(model = priv$.model, pkMappings = list(1))
    Condition
      Error in `ospsuite.utils::validateIsOfType()`:
      ! `<caller>()`: argument "pkMappings" is of type <list>, but expected <PKOutputMapping>!

# estimateCI() gives the PK mode error before building the model

    Code
      task$estimateCI()
    Condition
      Error in `private$.assertNotPKMode()`:
      ! `estimateCI()` is not applicable for PK metric optimization.

# ParameterIdentification stops when a simulation has no parameter of the task in PK mode

    Code
      ParameterIdentification$new(simulations = simulations, parameters = testPKParameters(
        simulations[[1]]), pkOutputMappings = list(pkMapping(simulations[[1]]),
      pkMapping(simulations[[2]])))
    Condition
      Error in `.validateSimulationIds()`:
      ! Mismatch or missing ID detected.
      Ensure each Simulation ID matches with corresponding PIParameter IDs.
      Simulation IDs: <id>, <id>
      PIParameter IDs: <id>

