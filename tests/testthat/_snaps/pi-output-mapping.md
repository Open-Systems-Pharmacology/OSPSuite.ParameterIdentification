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

# replacing a data set with another number of y-values drops its weights

    Code
      mapping$addObservedDataSets(shortStudyB())
    Condition
      Warning in `mapping$addObservedDataSets()`:
      The weights of "studyB" were removed, because its number of y-values changed. Set them again with `PIOutputMapping$setDataWeights()`.

# replacing a data set warns when the call passes weights for another data set only

    Code
      mapping$addObservedDataSets(shortStudyB(), weights = c(studyA = 3))
    Condition
      Warning in `mapping$addObservedDataSets()`:
      The weights of "studyB" were removed, because its number of y-values changed. Set them again with `PIOutputMapping$setDataWeights()`.

# replacing a data set with invalid weights warns about the dropped weights

    Code
      try(mapping$addObservedDataSets(shortStudyB(), weights = list(studyB = c(1, 0,
        2))), silent = TRUE)
    Condition
      Warning in `mapping$addObservedDataSets()`:
      The weights of "studyB" were removed, because its number of y-values changed. Set them again with `PIOutputMapping$setDataWeights()`.

# one number in addObservedDataSets() replaces the weights of all data sets

    Code
      mapping$addObservedDataSets(studyC, weights = 0.5)
    Condition
      Warning in `mapping$addObservedDataSets()`:
      Data weights have already been set. Check if they are still valid after adding new datasets.

# a data set without values takes empty weights and loses them when it gets values

    Code
      mapping$addObservedDataSets(studyC)
    Condition
      Warning in `mapping$addObservedDataSets()`:
      The weights of "studyC" were removed, because its number of y-values changed. Set them again with `PIOutputMapping$setDataWeights()`.

# observedDataSets stops when a data set was renamed after adding it

    Code
      mapping$observedDataSets
    Condition
      Error:
      ! The observed data set(s) "studyB" were renamed to "studyB revised" after they were added to the output mapping. Remove them with `PIOutputMapping$removeObservedDataSet()` under the old name and add them again, then set their weights, transformations and subject count again.

# PIOutputMapping cannot set weights with invalid input

    Code
      outputMapping$setDataWeights(weights)
    Condition
      Error in `outputMapping$setDataWeights()`:
      ! Weights must be one unnamed number, or be named by the observed data sets "AciclovirLaskinData.Laskin 1982.Group A", each used once.

# PIOutputMapping cannot set weights with invalid dataset label

    Code
      outputMapping$setDataWeights(weights)
    Condition
      Error in `outputMapping$setDataWeights()`:
      ! Weights must be one unnamed number, or be named by the observed data sets "AciclovirLaskinData.Laskin 1982.Group A", each used once.

---

    Code
      outputMapping$setDataWeights(weights)
    Condition
      Error in `outputMapping$setDataWeights()`:
      ! Weights must be one unnamed number, or be named by the observed data sets "AciclovirLaskinData.Laskin 1982.Group A", each used once.

# PIOutputMapping fails with unknown label in weights for multiple datasets

    Code
      outputMapping$setDataWeights(weights)
    Condition
      Error in `outputMapping$setDataWeights()`:
      ! Weights must be one unnamed number, or be named by the observed data sets "dataSet1" and "dataSet2", each used once.

# setDataWeights() rejects invalid names and changes nothing

    Code
      mapping$setDataWeights(list(2))
    Condition
      Error in `mapping$setDataWeights()`:
      ! Weights must be one unnamed number, or be named by the observed data sets "studyA" and "studyB", each used once.
    Code
      mapping$setDataWeights(list(1, 2))
    Condition
      Error in `mapping$setDataWeights()`:
      ! Weights must be one unnamed number, or be named by the observed data sets "studyA" and "studyB", each used once.
    Code
      mapping$setDataWeights(c(1, 2))
    Condition
      Error in `mapping$setDataWeights()`:
      ! Weights must be one unnamed number, or be named by the observed data sets "studyA" and "studyB", each used once.
    Code
      mapping$setDataWeights(c(studyC = 1))
    Condition
      Error in `mapping$setDataWeights()`:
      ! Weights must be one unnamed number, or be named by the observed data sets "studyA" and "studyB", each used once.
    Code
      mapping$setDataWeights(c(studyA = 1, studyA = 2))
    Condition
      Error in `mapping$setDataWeights()`:
      ! Weights must be one unnamed number, or be named by the observed data sets "studyA" and "studyB", each used once.
    Code
      mapping$setDataWeights(stats::setNames(c(1, 2), c("studyA", "")))
    Condition
      Error in `mapping$setDataWeights()`:
      ! Weights must be one unnamed number, or be named by the observed data sets "studyA" and "studyB", each used once.
    Code
      mapping$setDataWeights(list(studyB = 3, studyC = 1))
    Condition
      Error in `mapping$setDataWeights()`:
      ! Weights must be one unnamed number, or be named by the observed data sets "studyA" and "studyB", each used once.

# setDataWeights() rejects invalid values and changes nothing

    Code
      mapping$setDataWeights("2")
    Condition
      Error in `mapping$setDataWeights()`:
      ! Weights must be finite numbers that are not negative.
    Code
      mapping$setDataWeights(NULL)
    Condition
      Error in `mapping$setDataWeights()`:
      ! Weights must be finite numbers that are not negative.
    Code
      mapping$setDataWeights(NA)
    Condition
      Error in `mapping$setDataWeights()`:
      ! Weights must be finite numbers that are not negative.
    Code
      mapping$setDataWeights(NA_real_)
    Condition
      Error in `mapping$setDataWeights()`:
      ! Weights must be finite numbers that are not negative.
    Code
      mapping$setDataWeights(Inf)
    Condition
      Error in `mapping$setDataWeights()`:
      ! Weights must be finite numbers that are not negative.
    Code
      mapping$setDataWeights(-1)
    Condition
      Error in `mapping$setDataWeights()`:
      ! Weights must be finite numbers that are not negative.
    Code
      mapping$setDataWeights(numeric())
    Condition
      Error in `mapping$setDataWeights()`:
      ! Weights must be finite numbers that are not negative.
    Code
      mapping$setDataWeights(list())
    Condition
      Error in `mapping$setDataWeights()`:
      ! Weights must be finite numbers that are not negative.
    Code
      mapping$setDataWeights(list(studyA = "2"))
    Condition
      Error in `mapping$setDataWeights()`:
      ! Weights must be finite numbers that are not negative.
    Code
      mapping$setDataWeights(list(studyA = list(1)))
    Condition
      Error in `mapping$setDataWeights()`:
      ! Weights must be finite numbers that are not negative.
    Code
      mapping$setDataWeights(list(studyA = numeric()))
    Condition
      Error in `mapping$setDataWeights()`:
      ! Weights must be finite numbers that are not negative.
    Code
      mapping$setDataWeights(c(studyA = NaN))
    Condition
      Error in `mapping$setDataWeights()`:
      ! Weights must be finite numbers that are not negative.
    Code
      mapping$setDataWeights(c(studyB = -0.5))
    Condition
      Error in `mapping$setDataWeights()`:
      ! Weights must be finite numbers that are not negative.
    Code
      mapping$setDataWeights(list(studyB = 3, studyA = c(1, NA, 1, 1)))
    Condition
      Error in `mapping$setDataWeights()`:
      ! Weights must be finite numbers that are not negative.
    Code
      mapping$setDataWeights(list(studyB = 3, studyA = c(1, 2)))
    Condition
      Error in `mapping$setDataWeights()`:
      ! Weights for 'studyA' must have length 4 matching y-values, but got 2.

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

