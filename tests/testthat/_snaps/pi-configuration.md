# PIConfiguration instance prints without errors

    Code
      print(piConfiguration)
    Output
      <PIConfiguration>
        * Optimization algorithm: BOBYQA
        * Confidence interval method: hessian
        * Objective type: lsq
        * Residual weighting method: none
        * Robust residual calculation method: none
        * BLQ removal mode: none
        * BLQ handling method: lloqHalf
        * Print feedback after each function evaluation: FALSE

# objectiveType rejects an unknown value

    Code
      piConfiguration$objectiveType <- "map"
    Condition
      Error in `ospsuite.utils::validateEnumValue()`:
      ! map is not a valid value in `ObjectiveTypes`.
      All valid values can be found using `ObjectiveTypes`
      Did you mean one of these: mle ?

# blqMethod m3 requires objectiveType mle

    Code
      piConfiguration$blqMethod <- "m3"
    Condition
      Error:
      ! `blqMethod = "m3"` requires `objectiveType = "mle"`, but `objectiveType` is "lsq".
      Censoring is a likelihood operation, so it cannot be scored by least squares.

# objectiveType cannot leave m3 stranded on the lsq path

    Code
      piConfiguration$objectiveType <- "lsq"
    Condition
      Error:
      ! `objectiveType = "lsq"` is not allowed while `blqMethod = "m3"`.
      Set `blqMethod` to "none", "lloq", or "lloqHalf" first.

# mle and robust residual weighting are mutually exclusive

    Code
      piConfiguration$objectiveType <- "mle"
    Condition
      Error:
      ! `objectiveType = "mle"` cannot be combined with `robustMethod = "bisquare"`.
      Robust weights can be exactly zero, which makes the likelihood infinite at every parameter value.
      Set `robustMethod = "none"`, or use `objectiveType = "lsq"`.

---

    Code
      piConfigurationMle$objectiveFunctionOptions <- list(robustMethod = "huber")
    Condition
      Error:
      ! `objectiveType = "mle"` cannot be combined with `robustMethod = "huber"`.
      Robust weights can be exactly zero, which makes the likelihood infinite at every parameter value.
      Set `robustMethod = "none"`, or use `objectiveType = "lsq"`.

# mle and scaleVar are mutually exclusive under data-error weighting

    Code
      piConfiguration$objectiveType <- "mle"
    Condition
      Error:
      ! `objectiveType = "mle"` cannot be combined with `scaleVar = TRUE` while `residualWeightingMethod = "error"`.
      There is no concentrated scale for the scale factor to cancel against under a measured standard deviation, so it would inflate the measured standard deviation by the observation count.
      Set `scaleVar = FALSE`, or use `residualWeightingMethod = "none"`.

---

    Code
      piConfigurationMle$objectiveFunctionOptions <- list(scaleVar = TRUE)
    Condition
      Error:
      ! `objectiveType = "mle"` cannot be combined with `scaleVar = TRUE` while `residualWeightingMethod = "error"`.
      There is no concentrated scale for the scale factor to cancel against under a measured standard deviation, so it would inflate the measured standard deviation by the observation count.
      Set `scaleVar = FALSE`, or use `residualWeightingMethod = "none"`.

# objectiveFunctionOptions validates values before the mle cross-field guards

    Code
      piConfiguration$objectiveFunctionOptions <- list(residualWeightingMethod = NA)
    Condition
      Error in `ospsuite.utils::validateIsOption()`:
      ! Option validation failed:
      
      residualWeightingMethod : `<caller>`: NA values are not allowed.

# objectiveFunctionType is no longer an objectiveFunctionOptions key

    Code
      piConfiguration$objectiveFunctionOptions <- list(objectiveFunctionType = "mle")
    Condition
      Error:
      ! `objectiveFunctionType` has been removed from objectiveFunctionOptions.
      Use `objectiveType` to select "lsq" or "mle" scoring.
      For censored (M3) handling set `objectiveType = "mle"` together with `blqMethod = "m3"`.

