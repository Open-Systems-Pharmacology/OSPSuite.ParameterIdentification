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

- `pkOutputMappings`:

  A list of `PKOutputMapping` objects for PK metric optimization. `NULL`
  in standard PI mode. Read-only.

## Methods

### Public methods

- [`ParameterIdentification$new()`](#method-ParameterIdentification-initialize)

- [`ParameterIdentification$run()`](#method-ParameterIdentification-run)

- [`ParameterIdentification$estimateCI()`](#method-ParameterIdentification-estimateCI)

- [`ParameterIdentification$plotResults()`](#method-ParameterIdentification-plotResults)

- [`ParameterIdentification$gridSearch()`](#method-ParameterIdentification-gridSearch)

- [`ParameterIdentification$calculateOFVProfiles()`](#method-ParameterIdentification-calculateOFVProfiles)

- [`ParameterIdentification$print()`](#method-ParameterIdentification-print)

------------------------------------------------------------------------

### `ParameterIdentification$new()`

Initializes a `ParameterIdentification` instance.

#### Usage

    ParameterIdentification$new(
      simulations,
      parameters,
      outputMappings = NULL,
      pkOutputMappings = NULL,
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
  [`PIParameters`](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/reference/PIParameters.md)
  for details.

- `outputMappings`:

  (Optional) A `PIOutputMapping` or list of `PIOutputMapping` objects
  mapping model outputs to observed data. Mutually exclusive with
  `pkOutputMappings`.

- `pkOutputMappings`:

  (Optional) A `PKOutputMapping` or list of `PKOutputMapping` objects
  for PK metric optimization. Mutually exclusive with `outputMappings`.

- `configuration`:

  (Optional) A `PIConfiguration` object specifying algorithm, CI method,
  and objective function settings. Defaults to a new configuration if
  omitted. See
  [`PIConfiguration`](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/reference/PIConfiguration.md)
  for configuration options.

#### Returns

A `ParameterIdentification` object ready to run parameter estimation.
Executes Parameter Identification

------------------------------------------------------------------------

### `ParameterIdentification$run()`

Runs parameter identification using the configured optimization
algorithm. Returns a structured `piResults`object containing estimated
parameters, diagnostics, and (optionally) confidence intervals.

#### Usage

    ParameterIdentification$run()

#### Returns

A
[`PIResult`](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/reference/PIResult.md)
object in standard mode, or a `PKResult` object (internal) when
`pkOutputMappings` was provided. Estimate Confidence Intervals

------------------------------------------------------------------------

### `ParameterIdentification$estimateCI()`

Computes confidence intervals for the optimized parameters using the
method defined in the associated `PIConfiguration`. Intended for
advanced use when `autoEstimateCI` was set to `FALSE` during the initial
run.

#### Usage

    ParameterIdentification$estimateCI()

#### Returns

The same
[`PIResult`](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/reference/PIResult.md)
object returned by the `run()` method, updated to include confidence
interval estimates. Plot Parameter Estimation Results

------------------------------------------------------------------------

### `ParameterIdentification$plotResults()`

Re-runs model simulations using the current or specified parameter
values and generates plots comparing predictions to observed data.

#### Usage

    ParameterIdentification$plotResults(par = NULL)

#### Arguments

- `par`:

  Optional parameter values for simulations, in the order of
  `ParameterIdentification$parameters`. Use current values if `NULL`.

#### Returns

A list of `patchwork` objects (one per output mapping), showing:

- Individual time profiles

- Predicted vs. observed values

- Residuals vs. time Perform a Parameter Grid Search

Generates a grid of parameter combinations, computes the OFV for each,
and optionally sets the best result as the starting point for s
subsequent optimization.

Note: The resulting grid can be used to explore the parameter space or
initialize better starting values.

------------------------------------------------------------------------

### `ParameterIdentification$gridSearch()`

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

------------------------------------------------------------------------

### `ParameterIdentification$calculateOFVProfiles()`

Generates OFV profiles by varying each `PIParameter` independently while
holding the others fixed at `par`. Useful as a post-optimization
diagnostic: around a (local) minimum the OFV is expected to be roughly
convex along each axis.

#### Usage

    ParameterIdentification$calculateOFVProfiles(
      par = NULL,
      boundFactor = 0.1,
      totalEvaluations = 20
    )

#### Arguments

- `par`:

  Numeric vector of parameter values, one for each `PIParameter`.
  Defaults to current parameter values if `NULL`, not numeric, or of
  mismatched length.

- `boundFactor`:

  Numeric scalar. A value of `0.1` (default) means bounds extend ±10%
  around `par` for each parameter.

- `totalEvaluations`:

  Integer specifying the number of grid points per parameter profile.
  Default is `20`.

#### Details

For each parameter `i` a one-dimensional grid of `totalEvaluations`
equally spaced points is built between `lower[i]` and `upper[i]`, with
the bounds derived from `par[i]` and `boundFactor`:

- `par[i] >= 0`: `lower[i] = (1 - boundFactor) * par[i]`,
  `upper[i] = (1 + boundFactor) * par[i]`.

- `par[i] < 0`: bounds are mirrored so that `lower < upper` is
  preserved.

The objective function is evaluated along each axis with all other
parameters held at their `par` values. Failed simulations contribute
`Inf` to the corresponding `ofv` cell.

#### Returns

A named list of tibbles, one element per `PIParameter`. List names are
the parameter paths (taken from `parameters[[1]]$path`). Each tibble has
two columns:

- a column named after the parameter path, holding the grid values;

- `ofv`, holding the matching objective function values.

Pass the returned list to
[`plotOFVProfiles()`](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/reference/plotOFVProfiles.md)
to visualize the profiles.

#### Examples

    # piTask is a configured ParameterIdentification instance
    # Default: +/-10% around current values, 20 grid points per parameter
    # ofvProfiles <- piTask$calculateOFVProfiles()

    # Wider neighborhood, finer grid
    # ofvProfiles <- piTask$calculateOFVProfiles(
    #   boundFactor = 0.5,
    #   totalEvaluations = 50
    # )

    # plotOFVProfiles(ofvProfiles)[[1]]

------------------------------------------------------------------------

### `ParameterIdentification$print()`

Prints a summary of `ParameterIdentification` instance.

#### Usage

    ParameterIdentification$print()

## Examples

``` r

## ------------------------------------------------
## Method `ParameterIdentification$calculateOFVProfiles()`
## ------------------------------------------------

# piTask is a configured ParameterIdentification instance
# Default: +/-10% around current values, 20 grid points per parameter
# ofvProfiles <- piTask$calculateOFVProfiles()

# Wider neighborhood, finer grid
# ofvProfiles <- piTask$calculateOFVProfiles(
#   boundFactor = 0.5,
#   totalEvaluations = 50
# )

# plotOFVProfiles(ofvProfiles)[[1]]
```
