# PIParameters can print PIOutputMapping

    Code
      print(piParam)
    Output
      <PIParameters>
        * Number of parameters: 1
        * Value: 1.764e-05
        * Start value: 1.764e-05
        * Min value: 1.764e-06
        * Max value: 0.0001764
        * Unit: dm/min

# PIParameters can export to data.frame

    Code
      piParam$toDataFrame()
    Output
        group         name                                  path   unit    currValue
      1    NA Permeability Vergin 1995 IV|Aciclovir|Permeability dm/min 1.764167e-05
          startValue     minValue     maxValue
      1 1.764167e-05 1.764167e-06 0.0001764167

# Start, min, and max values are set correctly (single parameter)

    Code
      piParam$minValue <- (newStartValue * 2)
    Condition
      Error:
      ! `minValue` and `maxValue` must bracket the start value (3.52833353250001e-05) with `minValue` < `maxValue`. Provided bound: 7.05666706500003e-05.

---

    Code
      piParam$maxValue <- (newStartValue / 2)
    Condition
      Error:
      ! `minValue` and `maxValue` must bracket the start value (3.52833353250001e-05) with `minValue` < `maxValue`. Provided bound: 1.76416676625001e-05.

# PIParameters with multiple parameters can export to data.frame

    Code
      piParam$toDataFrame()
    Output
        group         name                                  path   unit    currValue
      1    NA Permeability Vergin 1995 IV|Aciclovir|Permeability dm/min 1.764167e-05
          startValue     minValue     maxValue
      1 1.764167e-05 1.764167e-06 0.0001764167

# Start, min, and max values are set correctly (multiple parameters)

    Code
      piParam$minValue <- (newStartValue * 2)
    Condition
      Error:
      ! `minValue` and `maxValue` must bracket the start value (4.33497537841081) with `minValue` < `maxValue`. Provided bound: 8.66995075682162.

---

    Code
      piParam$maxValue <- (newStartValue / 2)
    Condition
      Error:
      ! `minValue` and `maxValue` must bracket the start value (4.33497537841081) with `minValue` < `maxValue`. Provided bound: 2.16748768920541.

# Zero start value without explicit bounds errors

    Code
      PIParameters$new(zeroParam)
    Condition
      Error in `initialize()`:
      ! Cannot derive optimization bounds from a start value of 0. Provide explicit `minValue` and `maxValue` when creating the <PIParameters>.

# Zero-width bounds are rejected at a zero start value

    Code
      PIParameters$new(zeroParam, minValue = 0, maxValue = 0)
    Condition
      Error:
      ! `minValue` and `maxValue` must bracket the start value (0) with `minValue` < `maxValue`. Provided bound: 0.

# Zero-width bounds are rejected at a non-zero start value

    Code
      PIParameters$new(param, minValue = 5, maxValue = 5)
    Condition
      Error:
      ! `minValue` and `maxValue` must bracket the start value (5) with `minValue` < `maxValue`. Provided bound: 5.

# Setters cannot collapse the range to zero width

    Code
      piParam$maxValue <- 5
    Condition
      Error:
      ! `minValue` and `maxValue` must bracket the start value (5) with `minValue` < `maxValue`. Provided bound: 5.

