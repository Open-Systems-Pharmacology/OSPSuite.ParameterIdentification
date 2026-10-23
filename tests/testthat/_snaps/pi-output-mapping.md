# PIOutputMapping instance prints without errors

    Code
      print(outputMapping)
    Output
      <PIOutputMapping>
        * Output path: Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral
        Venous Blood)
        * Observed data labels: NULL
        * Data weight labels: NULL
        * Scaling: lin

# setSubjectCount() rejects invalid counts and the binding is read-only

    Code
      mapping$setSubjectCount(0)
    Condition
      Error in `mapping$setSubjectCount()`:
      ! The subject count must be one positive whole number, not 0.

---

    Code
      mapping$setSubjectCount(2.5)
    Condition
      Error in `mapping$setSubjectCount()`:
      ! The subject count must be one positive whole number, not 2.5.

---

    Code
      mapping$setSubjectCount(c(3, 4))
    Condition
      Error in `mapping$setSubjectCount()`:
      ! The subject count must be one positive whole number, not 3 and 4.

---

    Code
      mapping$subjectCount <- 5
    Condition
      Error:
      ! Property $subjectCount is readonly

