# PIResult

Structured output of a parameter identification task, including
optimization results, confidence intervals and parameter metadata.

## Methods

### Public methods

- [`PIResult$new()`](#method-PIResult-initialize)

- [`PIResult$toDataFrame()`](#method-PIResult-toDataFrame)

- [`PIResult$toList()`](#method-PIResult-toList)

- [`PIResult$print()`](#method-PIResult-print)

- [`PIResult$clone()`](#method-PIResult-clone)

------------------------------------------------------------------------

### `PIResult$new()`

Initializes a `PIResult` instance. For internal use only.

This constructor is used internally by the parameter identification
workflow to store and standardize results from optimization and
confidence interval estimation.

#### Usage

    PIResult$new(
      optimResult,
      ciResult = NULL,
      costDetails = NULL,
      configuration = NULL,
      piParameters = NULL
    )

#### Arguments

- `optimResult`:

  A named list containing optimization results. Typically produced
  internally by `Optimizer$run()` and includes fields such as `par`,
  `value`, `startValues`, `elapsed`, `convergence`, etc.

- `ciResult`:

  (Optional) A named list of confidence interval results, returned by
  `Optimizer$estimateCI()`. Contains fields like `sd`, `cv`, `lowerCI`,
  `upperCI`, `ciType`, `elapsed`, and `details`.

- `costDetails`:

  (Optional) The cost details of the best evaluation, a `modelCost`
  object as returned by the objective function. See `toList()`.

- `configuration`:

  (Optional) The `PIConfiguration` object used during parameter
  identification, read for the CI method when `ciResult` is `NULL`.

- `piParameters`:

  (Optional) A list of `PIParameter` objects used in the optimization.
  If not provided, default parameter names will be generated
  automatically.

#### Returns

A `PIResult` object containing optimization results, confidence interval
estimates (if available) and parameter metadata (if available).

------------------------------------------------------------------------

### `PIResult$toDataFrame()`

Export PIResult to a `data.frame`.

#### Usage

    PIResult$toDataFrame()

#### Returns

A data frame with the following columns:

- `group`: Parameter group identifier

- `name`: Parameter name

- `path`: Full parameter path

- `unit`: Unit of the parameter

- `estimate`: Estimated parameter value after optimization

- `sd` and `cv`: Standard deviation and coefficient of variation of the
  estimate, for the hessian and bootstrap methods

- `se` and `rse`: Standard error, the half-width of the interval divided
  by the z-score of the confidence level, and relative standard error,
  for the profile likelihood method

- `lowerCI`: Lower confidence bound

- `upperCI`: Upper confidence bound

- `ciType`: Type of confidence interval ("two-sided", "one-sided", or
  "failed")

- `initialValue`: Initial parameter value used for optimization

------------------------------------------------------------------------

### `PIResult$toList()`

Returns the full internal result list.

#### Usage

    PIResult$toList()

#### Returns

A named list containing all result values and `costDetails`, a
`modelCost` object: `modelCost` (the total cost), `minLogProbability`,
`costVariables` (one row with `nObservations`, `M3Contribution`,
`rawSSR` and `weightedSSR`) and `residualDetails` (one row per
observation with `index`, `x`, `yObserved`, `ySimulated`, `scaleFactor`,
`errorWeights`, `robustWeights`, `userWeights`, `totalWeights`,
`rawResiduals` and `weightedResiduals`).

------------------------------------------------------------------------

### `PIResult$print()`

Prints a summary of `PIResult`

#### Usage

    PIResult$print()

------------------------------------------------------------------------

### `PIResult$clone()`

The objects of this class are cloneable with this method.

#### Usage

    PIResult$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
