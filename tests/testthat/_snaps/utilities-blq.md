# an unrecognized mode errors at the default switch arm

    Code
      .applyBlqRemove(blqSingleDataset(), "sometimes")
    Condition
      Error in `ospsuite.utils::validateEnumValue()`:
      ! sometimes is not a valid value in `BLQRemoveModes`.
      All valid values can be found using `BLQRemoveModes`

# an unrecognized method errors at the default switch arm

    Code
      .applyBlqSubstitution(1, lloq = 0.5, "sometimes", "lin")
    Condition
      Error in `ospsuite.utils::validateEnumValue()`:
      ! sometimes is not a valid value in `BLQMethods`.
      All valid values can be found using `BLQMethods`

# a length-mismatched lloq errors instead of silently corrupting observed values

    Code
      .applyBlqSubstitution(c(0.3, 0.2), lloq = 0.5, "lloqHalf", "lin")
    Condition
      Error in `ospsuite.utils::validateIsSameLength()`:
      ! Arguments "observedValues, lloq" must have the same length, but they don't!

