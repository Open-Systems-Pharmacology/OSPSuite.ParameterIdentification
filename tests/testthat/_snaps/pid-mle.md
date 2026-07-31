# mle with data-error weighting rejects observations lacking an error value

    Code
      task$run()
    Message
      Starting optimization using 'BOBYQA' with initial value(s):
        -0.09700
    Condition
      Error in `private$.objectiveFunction()`:
      ! "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)" has 8 observations without a usable error value, and `objectiveType = "mle"` with `residualWeightingMethod = "error"` requires one on every scored observation.
      Supply an error value for every observation, or set `residualWeightingMethod = "none"` to estimate a single residual standard deviation instead.

