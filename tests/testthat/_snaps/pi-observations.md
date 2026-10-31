# PIObservations validates its input and is read-only

    Code
      PIObservations$new(list(1))
    Condition
      Error in `ospsuite.utils::validateIsOfType()`:
      ! `<caller>()`: argument "outputMappings" is of type <list>, but expected <PIOutputMapping>!

---

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

