# mle with data-error weighting rejects observations lacking an error value

    Code
      task$run()
    Message
      Starting optimization using 'BOBYQA' with initial value(s):
        -0.09700
    Condition
      Error in `private$.objectiveFunction()`:
      ! `objectiveType = "mle"` with `residualWeightingMethod = "error"` needs a usable standard deviation on every scored observation, and "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)" does not provide one everywhere.
      8 observations without a usable error value. Supply an error value for every observation, or set `residualWeightingMethod = "none"` to estimate a single residual standard deviation instead.

# mle names a non-positive observation rather than blaming its error value

    Code
      priv$.objectiveFunction(startValues)
    Condition
      Error in `priv$.objectiveFunction()`:
      ! `objectiveType = "mle"` with `residualWeightingMethod = "error"` needs a usable standard deviation on every scored observation, and "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)" does not provide one everywhere.
      1 observation with a value of zero or less and a usable error value. The data-error model turns that error value into a weight through the coefficient of variation, which is undefined at a non-positive value, so such an observation cannot be scored by this model at all. Remove it from the data set, or set `residualWeightingMethod = "none"`.

# mle rejects a dataset weight the user set to zero

    Code
      priv$.objectiveFunction(startValues)
    Condition
      Error in `priv$.objectiveFunction()`:
      ! "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)" carries a dataset weight of zero or less, which `objectiveType = "mle"` cannot represent.
      A zero weight means the residual standard deviation is infinite rather than that the observation is excluded.
      Remove the observation from the data set instead, or use `blqRemove` if it is below the quantification limit.

# the mle preconditions run on every entry point, not only run()

    Code
      task$calculateOFVProfiles(totalEvaluations = 2L)
    Condition
      Error:
      ! `objectiveType = "mle"` with `residualWeightingMethod = "error"` needs a usable standard deviation on every scored observation, and "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)" does not provide one everywhere.
      8 observations without a usable error value. Supply an error value for every observation, or set `residualWeightingMethod = "none"` to estimate a single residual standard deviation instead.

---

    Code
      task$gridSearch(totalEvaluations = 2)
    Condition
      Error:
      ! `objectiveType = "mle"` with `residualWeightingMethod = "error"` needs a usable standard deviation on every scored observation, and "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)" does not provide one everywhere.
      8 observations without a usable error value. Supply an error value for every observation, or set `residualWeightingMethod = "none"` to estimate a single residual standard deviation instead.

