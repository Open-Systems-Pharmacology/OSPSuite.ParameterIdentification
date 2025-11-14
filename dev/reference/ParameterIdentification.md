# ParameterIdentification

Performs parameter estimation by fitting model simulations to observed
data. Supports customizable optimization and confidence interval
methods.

## Active bindings

- `simulations`:

  A named list of `Simulation` objects, keyed by the IDs of their root
  containers.

- `parameters`:

  A list of `PIParameters`, each representing a grouped set of model
  parameters to be optimized (read-only).

- `configuration`:

  A `PIConfiguration` object controlling algorithm, CI estimation, and
  objective function options.

- `outputMappings`:

  A list of `PIOutputMapping` objects mapping observed datasets to
  simulated outputs.

## Methods

### Public methods

- [`ParameterIdentification$new()`](#method-ParameterIdentification-new)

- [`ParameterIdentification$run()`](#method-ParameterIdentification-run)

- [`ParameterIdentification$estimateCI()`](#method-ParameterIdentification-estimateCI)

- [`ParameterIdentification$plotResults()`](#method-ParameterIdentification-plotResults)

- [`ParameterIdentification$gridSearch()`](#method-ParameterIdentification-gridSearch)

- [`ParameterIdentification$calculateOFVProfiles()`](#method-ParameterIdentification-calculateOFVProfiles)

- [`ParameterIdentification$print()`](#method-ParameterIdentification-print)

------------------------------------------------------------------------

### Method `new()`

Initializes a `ParameterIdentification` instance.

#### Usage

    ParameterIdentification$new(
      simulations,
      parameters,
      outputMappings,
      configuration = NULL
    )

#### Arguments

- `simulations`:

  A `Simulation` or list of `Simulation` objects to be used for
  parameter estimation. Each simulation must contain the model
  parameters specified in `parameters`. Use
  [`ospsuite::loadSimulation()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/loadSimulation.html)
  to load simulation files.

- `parameters`:

  A `PIParameters` or list of `PIParameters` objects specifying the
  model parameters to optimize. Each `PIParameters` object may group one
  or more underlying model parameters. See
  [`PIParameters`](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/dev/reference/PIParameters.md)
  for details.

- `outputMappings`:

  A `PIOutputMapping` or list of `PIOutputMapping` objects mapping model
  outputs (represented by `Quantity` objects) to observed data. \#' See
  [`PIOutputMapping`](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/dev/reference/PIOutputMapping.md)
  for details.

- `configuration`:

  (Optional) A `PIConfiguration` object specifying algorithm, CI method,
  and objective function settings. Defaults to a new configuration if
  omitted. See
  [`PIConfiguration`](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/dev/reference/PIConfiguration.md)
  for configuration options.

#### Returns

A `ParameterIdentification` object ready to run parameter estimation.
Executes Parameter Identification

------------------------------------------------------------------------

### Method `run()`

Runs parameter identification using the configured optimization
algorithm. Returns a structured `piResults`object containing estimated
parameters, diagnostics, and (optionally) confidence intervals.

#### Usage

    ParameterIdentification$run()

#### Returns

A
[`PIResult`](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/dev/reference/PIResult.md)
object containing the optimization results. Estimate Confidence
Intervals

------------------------------------------------------------------------

### Method `estimateCI()`

Computes confidence intervals for the optimized parameters using the
method defined in the associated `PIConfiguration`. Intended for
advanced use when `autoEstimateCI` was set to `FALSE` during the initial
run.

#### Usage

    ParameterIdentification$estimateCI()

#### Returns

The same
[`PIResult`](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/dev/reference/PIResult.md)
object returned by the `run()` method, updated to include confidence
interval estimates. Plot Parameter Estimation Results

------------------------------------------------------------------------

### Method `plotResults()`

Re-runs model simulations using the current or specified parameter
values and generates plots comparing predictions to observed data.

#### Usage

    ParameterIdentification$plotResults(par = NULL)

#### Arguments

- `par`:

  Optional parameter values for simulations, in the order of
  `ParameterIdentification$parameters`. Use current values if `NULL`.

#### Returns

A list of `ggplot2` plots (one per output mapping), showing:

- Individual time profiles

- Observed vs. simulated values

- Residuals vs. time Perform a Parameter Grid Search

Generates a grid of parameter combinations, computes the OFV for each,
and optionally sets the best result as the starting point for s
subsequent optimization.

Note: The resulting grid can be used to explore the parameter space or
initialize better starting values.

------------------------------------------------------------------------

### Method `gridSearch()`

#### Usage

    ParameterIdentification$gridSearch(
      lower = NULL,
      upper = NULL,
      logScaleFlag = FALSE,
      totalEvaluations = 50,
      setStartValue = FALSE
    )

#### Arguments

- `lower`:

  Numeric vector of parameter lower bounds, defaulting to `PIParameter`
  minimum values.

- `upper`:

  Numeric vector of parameter upper bounds, defaulting to `PIParameter`
  maximum values.

- `logScaleFlag`:

  Logical scalar or vector; determines if grid points are spaced
  logarithmically. Default is `FALSE`.

- `totalEvaluations`:

  Integer specifying the total grid points. Default is 50.

- `setStartValue`:

  Logical. If `TRUE`, updates `PIParameter` starting values to the best
  grid point. Default is `FALSE`.

#### Returns

A tibble where each row is a parameter combination and the corresponding
objective function value (`ofv`). Calculate Objective Function Value
(OFV) Profiles

Generates OFV profiles by varying each parameter independently while
holding others constant.

------------------------------------------------------------------------

### Method `calculateOFVProfiles()`

#### Usage

    ParameterIdentification$calculateOFVProfiles(
      par = NULL,
      boundFactor = 0.1,
      totalEvaluations = 20
    )

#### Arguments

- `par`:

  Numeric vector of parameter values, one for each parameter. Defaults
  to current parameter values if `NULL`, invalid or mismatched.

- `boundFactor`:

  Numeric value. A value of 0.1 means `lower` is 10% below `par` and
  `upper` is 10% above `par`. Default is `0.1`.

- `totalEvaluations`:

  Integer specifying the total number of grid points across each
  parameter profile. Default is 20.

#### Returns

A list of tibbles, one for each parameter, showing how the objective
function value (OFV) changes when varying that parameter while keeping
the others fixed.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints a summary of `ParameterIdentification` instance.

#### Usage

    ParameterIdentification$print()
