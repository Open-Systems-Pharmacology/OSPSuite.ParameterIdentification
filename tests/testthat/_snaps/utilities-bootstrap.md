# .validateSubjectCounts() stops for aggregated data without a count

    Code
      .validateSubjectCounts(list(mapping))
    Condition
      Error in `.validateSubjectCounts()`:
      ! The aggregated data set(s) "AciclovirLaskinData.Laskin 1982.Group A" have no subject count. Bootstrap confidence intervals redraw the means of aggregated data from their mean and SD divided by the square root of the number of subjects, so call `PIOutputMapping$setSubjectCount()` on the output mapping with that number. Profile likelihood (`ciMethod = "PL"`) is the recommended interval for aggregated data.

# .classifyObservedData() reports aggregated data without error values as drawn whole

    Code
      .classifyObservedData(list(mapping))
    Message
      Data set "AciclovirLaskinData.Laskin 1982.Group A" has a yErrorType but no error values and is drawn as a whole data set.
      Classified observed data: 1 individual, 0 aggregated dataset(s).
    Condition
      Warning in `.classifyObservedData()`:
      Less than 3 individual datasets detected - bootstrap CI may be unreliable.

