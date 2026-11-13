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

# blqRemove that removes every observation of a mapping stops

    Code
      PIObservations$new(list(blqMapping(c(0.1, 0.2))), "always")
    Condition
      Error:
      ! `blqRemove = "always"` removes every observed value of the output mapping for "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)", because all are below the LLOQ. Choose another `blqRemove`.

# run() stops on a mapping that blqRemove empties

    Code
      task$run()
    Condition
      Error:
      ! `blqRemove = "always"` removes every observed value of the output mapping for "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)", because all are below the LLOQ. Choose another `blqRemove`.

