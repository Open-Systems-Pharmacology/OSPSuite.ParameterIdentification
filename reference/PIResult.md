# PIResult

Structured output of a parameter identification task, including
optimization results, confidence intervals, parameter metadata, and
configuration.

## Methods

### Public methods

- [`PIResult$new()`](#method-PIResult-new)

- [`PIResult$toDataFrame()`](#method-PIResult-toDataFrame)

- [`PIResult$toList()`](#method-PIResult-toList)

- [`PIResult$print()`](#method-PIResult-print)

- [`PIResult$clone()`](#method-PIResult-clone)

------------------------------------------------------------------------

### Method `new()`

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

  (Optional) A named list with detailed cost metrics computed during
  optimization.

- `configuration`:

  (Optional) The `PIConfiguration` object used during parameter
  identification.

- `piParameters`:

  (Optional) A list of `PIParameter` objects used in the optimization.
  If not provided, default parameter names will be generated
  automatically.

#### Returns

A `PIResult` object containing optimization results, confidence interval
estimates (if available), parameter metadata (if available), and
configuration.

------------------------------------------------------------------------

### Method `toDataFrame()`

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

- `sd`: Standard deviation from CI estimation

- `cv`: Coefficient of variation

- `lowerCI`: Lower confidence bound

- `upperCI`: Upper confidence bound

- `ciType`: Type of confidence interval ("two-sided", "one-sided", or
  "failed")

- `initialValue`: Initial parameter value used for optimization

------------------------------------------------------------------------

### Method `toList()`

Returns the full internal result list.

#### Usage

    PIResult$toList()

#### Returns

A named list containing all result values.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints a summary of `PIResult`

#### Usage

    PIResult$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PIResult$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
