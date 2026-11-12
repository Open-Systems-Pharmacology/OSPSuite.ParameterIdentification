# PIConfiguration instance prints without errors

    Code
      print(piConfiguration)
    Output
      <PIConfiguration>
        * Optimization algorithm: BOBYQA
        * Confidence interval method: hessian
        * Objective function type: lsq
        * Residual weighting method: none
        * Robust residual calculation method: none
        * BLQ removal mode: none
        * BLQ handling method: lloqHalf
        * Print feedback after each function evaluation: FALSE

# objectiveFunctionType m3 stops with an error naming blqMethod

    Code
      piConfiguration$objectiveFunctionOptions$objectiveFunctionType <- "m3"
    Condition
      Error:
      ! "m3" is no longer an `objectiveFunctionType`. Set `blqMethod = "m3"` instead.

---

    Code
      piConfiguration$objectiveFunctionOptions <- list(objectiveFunctionType = "m3")
    Condition
      Error:
      ! "m3" is no longer an `objectiveFunctionType`. Set `blqMethod = "m3"` instead.

# blqRemove accepts valid modes and rejects invalid ones

    Code
      piConfiguration$blqRemove <- "sometimes"
    Condition
      Error in `ospsuite.utils::validateEnumValue()`:
      ! sometimes is not a valid value in `BLQRemoveModes`.
      All valid values can be found using `BLQRemoveModes`

# blqRemove and blqMethod reject more than one value

    Code
      piConfiguration$blqRemove <- c("none", "always")
    Condition
      Error in `ospsuite.utils::validateIsOfLength()`:
      ! `<caller>()`: Object should be of length 1, but is of length 2 instead.

---

    Code
      piConfiguration$blqMethod <- c("m3", "none")
    Condition
      Error in `ospsuite.utils::validateIsOfLength()`:
      ! `<caller>()`: Object should be of length 1, but is of length 2 instead.

# blqMethod accepts all four methods and rejects invalid ones

    Code
      piConfiguration$blqMethod <- "censored"
    Condition
      Error in `ospsuite.utils::validateEnumValue()`:
      ! censored is not a valid value in `BLQMethods`.
      All valid values can be found using `BLQMethods`

# blqOptions rejects a cv outside (0, 1]

    Code
      piConfiguration$blqOptions$cv <- 0
    Condition
      Error in `ospsuite.utils::validateIsOption()`:
      ! Option validation failed:
      
      cv : `<caller>()`: Value(s) out of the allowed range [2.22044604925031e-16, 1].

---

    Code
      piConfiguration$blqOptions$cv <- 1.5
    Condition
      Error in `ospsuite.utils::validateIsOption()`:
      ! Option validation failed:
      
      cv : `<caller>()`: Value(s) out of the allowed range [2.22044604925031e-16, 1].

# blqOptions stops on the removed keys linScaleCV and logScaleSD

    Code
      piConfiguration$blqOptions <- list(linScaleCV = 0.3)
    Condition
      Error:
      ! Option `linScaleCV` was removed. Set the coefficient of variation of BLQ observations with `blqOptions$cv` instead.

---

    Code
      piConfiguration$blqOptions <- list(linScaleCV = 0.3, logScaleSD = 0.1)
    Condition
      Error:
      ! Options `linScaleCV` and `logScaleSD` were removed. Set the coefficient of variation of BLQ observations with `blqOptions$cv` instead.

# objectiveFunctionOptions stops on the removed keys linScaleCV and logScaleSD

    Code
      piConfiguration$objectiveFunctionOptions$logScaleSD <- 0.1
    Condition
      Error:
      ! Option `logScaleSD` was removed. Set the coefficient of variation of BLQ observations with `blqOptions$cv` instead.

---

    Code
      piConfiguration$objectiveFunctionOptions <- list(linScaleCV = 0.2)
    Condition
      Error:
      ! Option `linScaleCV` was removed. Set the coefficient of variation of BLQ observations with `blqOptions$cv` instead.

# blqOptions warns and ignores unknown keys

    Code
      piConfiguration$blqOptions <- list(unknownKey = "x")
    Condition
      Warning:
      Unknown option for blqOptions: "unknownKey". Ignored.

# blqRemove always and blqMethod m3 are rejected in either order

    Code
      piConfiguration$blqMethod <- "m3"
    Condition
      Error:
      ! `blqRemove = "always"` removes every BLQ observation, so `blqMethod = "m3"` has nothing to censor. Choose another `blqRemove` or `blqMethod`.

---

    Code
      piConfiguration$blqRemove <- "always"
    Condition
      Error:
      ! `blqRemove = "always"` removes every BLQ observation, so `blqMethod = "m3"` has nothing to censor. Choose another `blqRemove` or `blqMethod`.

