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
      mapping$setSubjectCount(13)
    Condition
      Error in `mapping$setSubjectCount()`:
      ! Cannot set subject counts: no observed data sets defined. Add the data sets with `PIOutputMapping$addObservedDataSets()` first.

---

    Code
      mapping$setSubjectCount(0)
    Condition
      Error in `mapping$setSubjectCount()`:
      ! Each subject count must be a positive whole number up to 2147483647, not 0.

---

    Code
      mapping$setSubjectCount(2.5)
    Condition
      Error in `mapping$setSubjectCount()`:
      ! Each subject count must be a positive whole number up to 2147483647, not 2.5.

---

    Code
      mapping$setSubjectCount(NA)
    Condition
      Error in `mapping$setSubjectCount()`:
      ! Each subject count must be a positive whole number up to 2147483647, not NA.

---

    Code
      mapping$setSubjectCount(numeric())
    Condition
      Error in `mapping$setSubjectCount()`:
      ! Subject counts must be one unnamed number for all data sets, or a vector named by the observed data sets "AciclovirLaskinData.Laskin 1982.Group A".

---

    Code
      mapping$setSubjectCount(c(3, 4))
    Condition
      Error in `mapping$setSubjectCount()`:
      ! Subject counts must be one unnamed number for all data sets, or a vector named by the observed data sets "AciclovirLaskinData.Laskin 1982.Group A".

---

    Code
      mapping$setSubjectCount(c(unknownStudy = 12))
    Condition
      Error in `mapping$setSubjectCount()`:
      ! Subject counts must be one unnamed number for all data sets, or a vector named by the observed data sets "AciclovirLaskinData.Laskin 1982.Group A".

---

    Code
      mapping$setSubjectCount(1e+10)
    Condition
      Error in `mapping$setSubjectCount()`:
      ! Each subject count must be a positive whole number up to 2147483647, not 1e+10.

---

    Code
      mapping$subjectCount <- 5
    Condition
      Error:
      ! Property $subjectCount is readonly

