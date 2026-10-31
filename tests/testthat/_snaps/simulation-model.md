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

