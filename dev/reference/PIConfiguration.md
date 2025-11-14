# PIConfiguration

Encapsulates configurations such as optimization algorithm choice, and
evaluation settings for parameter identification.

## Active bindings

- `printEvaluationFeedback`:

  Boolean. If `TRUE`, prints objective function value after each
  evaluation. Default is `FALSE`.

- `simulationRunOptions`:

  Object of `SimulationRunOptions` for simulation runs. If `NULL`,
  default options are used.

- `objectiveFunctionOptions`:

  Settings for model fit evaluation, affecting error metrics and cost
  calculation. See
  [`ObjectiveFunctionSpecs`](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/dev/reference/ObjectiveFunctionSpecs.md)
  for details. Defaults in
  [`ObjectiveFunctionOptions`](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/dev/reference/ObjectiveFunctionOptions.md).

- `algorithm`:

  Optimization algorithm name. See
  [`Algorithms`](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/dev/reference/Algorithms.md)
  for a list of supported algorithms. Defaults to `BOBYQA`.

- `ciMethod`:

  Confidence interval estimation method. See
  [`CIMethods`](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/dev/reference/CIMethods.md)
  for available options. Defaults to `hessian`.

- `algorithmOptions`:

  Named list of settings specific to the selected algorithm.. Refer to
  [`AlgorithmOptions`](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/dev/reference/AlgorithmOptions.md)
  for default settings per algorithm (e.g., `AlgorithmOptions_XYZ` where
  `XYZ` denotes the algorithm name). If `NULL`, algorithm's default
  settings are applied.

- `ciOptions`:

  Named list of settings for the selected CI method. Refer to
  [`CIOptions`](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/dev/reference/CIOptions.md)
  for default settings per method (e.g., `CIOptions_XYZ` where `XYZ`
  corresponds to the method name). If `NULL`, CI method's default
  settings are applied.

- `autoEstimateCI`:

  Logical. If `TRUE`, confidence intervals are automatically estimated
  after optimization. If `FALSE`, the step is skipped and can be
  triggered manually by calling the `estimateCI()` method on the
  `ParameterIdentification` object.

- `modelCostField`:

  Read-only field name in the cost object used as the optimization
  target. Currently, only `modelCost` is supported.

## Methods

### Public methods

- [`PIConfiguration$new()`](#method-PIConfiguration-new)

- [`PIConfiguration$print()`](#method-PIConfiguration-print)

- [`PIConfiguration$clone()`](#method-PIConfiguration-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new instance of the class.

#### Usage

    PIConfiguration$new()

#### Returns

A new `PIConfiguration` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints a summary of the `PIConfiguration`.

#### Usage

    PIConfiguration$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PIConfiguration$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
