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
      ! `xOffsets` must be one unnamed number for all data sets, or a vector named by the observed data sets "studyA" and "studyB", with each name used once.

---

    Code
      mapping$setDataTransformations(xOffsets = c(1, 2))
    Condition
      Error in `mapping$setDataTransformations()`:
      ! `xOffsets` must be one unnamed number for all data sets, or a vector named by the observed data sets "studyA" and "studyB", with each name used once.

---

    Code
      mapping$setDataTransformations(yFactors = NA_real_)
    Condition
      Error in `mapping$setDataTransformations()`:
      ! `yFactors` must be a numeric vector of finite numbers.

---

    Code
      mapping$setDataTransformations(yFactors = Inf)
    Condition
      Error in `mapping$setDataTransformations()`:
      ! `yFactors` must be a numeric vector of finite numbers.

---

    Code
      mapping$setDataTransformations(yFactors = numeric())
    Condition
      Error in `mapping$setDataTransformations()`:
      ! `yFactors` must be a numeric vector of finite numbers.

---

    Code
      mapping$setDataTransformations(xOffsets = 1, yFactors = c(studyC = 1))
    Condition
      Error in `mapping$setDataTransformations()`:
      ! `yFactors` must be one unnamed number for all data sets, or a vector named by the observed data sets "studyA" and "studyB", with each name used once.

---

    Code
      mapping$setDataTransformations(xOffsets = list(studyB = 0.2))
    Condition
      Error in `mapping$setDataTransformations()`:
      ! `xOffsets` must be a numeric vector of finite numbers.

---

    Code
      mapping$setDataTransformations(xOffsets = c(studyA = 1, studyA = 2))
    Condition
      Error in `mapping$setDataTransformations()`:
      ! `xOffsets` must be one unnamed number for all data sets, or a vector named by the observed data sets "studyA" and "studyB", with each name used once.

# adding a data set warns only when transformations were set before

    Code
      mapping$addObservedDataSets(studyC)
    Condition
      Warning in `mapping$addObservedDataSets()`:
      Data transformations have already been set. The added data set(s) "studyC" have none, so set them with `PIOutputMapping$setDataTransformations()` if they need any.

# removeObservedDataSet() rejects anything but one string and changes nothing

    Code
      mapping$removeObservedDataSet(1)
    Condition
      Error in `mapping$removeObservedDataSet()`:
      ! `label` must be the name of one observed data set, given as a single string.

---

    Code
      mapping$removeObservedDataSet(character())
    Condition
      Error in `mapping$removeObservedDataSet()`:
      ! `label` must be the name of one observed data set, given as a single string.

---

    Code
      mapping$removeObservedDataSet(NA_character_)
    Condition
      Error in `mapping$removeObservedDataSet()`:
      ! `label` must be the name of one observed data set, given as a single string.

# removing a data set drops its transformations and weights

    Code
      mapping$addObservedDataSets(studyB)
    Condition
      Warning in `mapping$addObservedDataSets()`:
      Data weights have already been set. Check if they are still valid after adding new datasets.
      Warning in `mapping$addObservedDataSets()`:
      Data transformations have already been set. The added data set(s) "studyB" have none, so set them with `PIOutputMapping$setDataTransformations()` if they need any.

# observedDataSets stops when a data set was renamed after adding it

    Code
      mapping$observedDataSets
    Condition
      Error:
      ! The observed data set(s) "studyB" were renamed to "studyB revised" after they were added to the output mapping. Remove them with `PIOutputMapping$removeObservedDataSet()` under the old name and add them again, then set their weights, transformations and subject count again.

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
      ! Subject counts must be one unnamed number for all data sets, or a vector named by the observed data sets "AciclovirLaskinData.Laskin 1982.Group A", with each name used once.

---

    Code
      mapping$setSubjectCount(c(3, 4))
    Condition
      Error in `mapping$setSubjectCount()`:
      ! Subject counts must be one unnamed number for all data sets, or a vector named by the observed data sets "AciclovirLaskinData.Laskin 1982.Group A", with each name used once.

---

    Code
      mapping$setSubjectCount(c(unknownStudy = 12))
    Condition
      Error in `mapping$setSubjectCount()`:
      ! Subject counts must be one unnamed number for all data sets, or a vector named by the observed data sets "AciclovirLaskinData.Laskin 1982.Group A", with each name used once.

---

    Code
      mapping$setSubjectCount(setNames(c(12, 13), c(label, label)))
    Condition
      Error in `mapping$setSubjectCount()`:
      ! Subject counts must be one unnamed number for all data sets, or a vector named by the observed data sets "AciclovirLaskinData.Laskin 1982.Group A", with each name used once.

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

