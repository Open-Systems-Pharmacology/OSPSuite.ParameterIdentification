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

# setDataTransformations() rejects invalid values and changes nothing

    Code
      PIOutputMapping$new(quantity = testQuantity)$setDataTransformations(xOffsets = 1)
    Condition
      Error:
      ! Cannot set data transformations: no observed data sets defined. Add the data sets with `PIOutputMapping$addObservedDataSets()` first.

---

    Code
      mapping$setDataTransformations(xOffsets = c(studyC = 1))
    Condition
      Error in `mapping$setDataTransformations()`:
      ! `xOffsets` must be one unnamed number for all data sets, or a vector named by the observed data sets "studyA" and "studyB".

---

    Code
      mapping$setDataTransformations(xOffsets = c(1, 2))
    Condition
      Error in `mapping$setDataTransformations()`:
      ! `xOffsets` must be one unnamed number for all data sets, or a vector named by the observed data sets "studyA" and "studyB".

---

    Code
      mapping$setDataTransformations(yFactors = NA_real_)
    Condition
      Error in `mapping$setDataTransformations()`:
      ! `yFactors` must contain finite numbers.

---

    Code
      mapping$setDataTransformations(yFactors = Inf)
    Condition
      Error in `mapping$setDataTransformations()`:
      ! `yFactors` must contain finite numbers.

---

    Code
      mapping$setDataTransformations(yFactors = numeric())
    Condition
      Error in `mapping$setDataTransformations()`:
      ! `yFactors` must contain finite numbers.

---

    Code
      mapping$setDataTransformations(xOffsets = 1, yFactors = c(studyC = 1))
    Condition
      Error in `mapping$setDataTransformations()`:
      ! `yFactors` must be one unnamed number for all data sets, or a vector named by the observed data sets "studyA" and "studyB".

# removing a data set drops its transformations and weights

    Code
      mapping$addObservedDataSets(studyB)
    Condition
      Warning in `mapping$addObservedDataSets()`:
      Data weights have already been set. Check if they are still valid after adding new datasets.

# observedDataSets stops when a data set was renamed after adding it

    Code
      mapping$observedDataSets
    Condition
      Error:
      ! The observed data set(s) "studyB" were renamed to "studyB revised" after they were added to the output mapping. Remove them with `PIOutputMapping$removeObservedDataSet()` under the old name and add them again.

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

