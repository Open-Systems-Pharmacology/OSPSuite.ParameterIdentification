# .validateSubjectCounts() stops for aggregated data without a count

    Code
      .validateSubjectCounts(list(mapping))
    Condition
      Error in `.validateSubjectCounts()`:
      ! The aggregated data set(s) "AciclovirLaskinData.Laskin 1982.Group A" have no subject count. Bootstrap confidence intervals redraw the means of aggregated data from their mean and SD divided by the square root of the number of subjects, so call `PIOutputMapping$setSubjectCount()` on the output mapping with that number. Profile likelihood (`ciMethod = "PL"`) is the recommended interval for aggregated data.

