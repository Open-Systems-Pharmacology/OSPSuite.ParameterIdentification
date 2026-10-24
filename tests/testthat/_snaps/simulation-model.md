# simulatedOutputs() fails when a used output is NA at an observed time

    Code
      model$simulatedOutputs(startValues(task), task$outputMappings)
    Condition
      Error in `model$simulatedOutputs()`:
      ! Simulation 'Vergin 1995 IV' failed: The simulated values of "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)" are "NA" at an observed time.

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

