# .errorModelFor rejects an unrecognized weighting method

    Code
      .errorModelFor("geometric")
    Condition
      Error in `.errorModelFor()`:
      ! No error model is defined for `residualWeightingMethod = "geometric"`.

# .negLogLikelihood rejects an unrecognized error model

    Code
      .negLogLikelihood(weightedSSR = 8, nObservations = 4, sumLogSigma = 0,
        errorModel = "proportional")
    Condition
      Error in `ospsuite.utils::validateEnumValue()`:
      ! proportional is not a valid value in `ErrorModels`.
      All valid values can be found using `ErrorModels`

