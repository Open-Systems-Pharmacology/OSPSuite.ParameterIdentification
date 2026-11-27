# observedData of PIObjective is read-only

    Code
      objective$observedData <- NULL
    Condition
      Error:
      ! Property $observedData is readonly

# PIObjective validates its options

    Code
      PIObjective$new(priv$.model, data, options, "censored", BLQOptions)
    Condition
      Error in `ospsuite.utils::validateEnumValue()`:
      ! censored is not a valid value in `BLQMethods`.
      All valid values can be found using `BLQMethods`

---

    Code
      PIObjective$new(priv$.model, data, options, "m3", list(cv = 0))
    Condition
      Error in `ospsuite.utils::validateIsOption()`:
      ! Option validation failed:
      
      cv : `<caller>()`: Value(s) out of the allowed range [2.22044604925031e-16, 1].

---

    Code
      PIObjective$new(priv$.model, data, options, "m3", BLQOptions)
    Condition
      Error in `ospsuite.utils::validateIsOption()`:
      ! Option validation failed:
      
      robustMethod : "tukey" not allowed.
      Allowed values: "none", "huber", and "bisquare".

