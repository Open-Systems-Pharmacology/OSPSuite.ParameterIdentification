# PIObservations validates its input and is read-only

    Code
      data$observedData <- NULL
    Condition
      Error:
      ! Property $observedData is readonly

---

    Code
      data$outputMappings <- NULL
    Condition
      Error:
      ! Property $outputMappings is readonly

