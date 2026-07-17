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

