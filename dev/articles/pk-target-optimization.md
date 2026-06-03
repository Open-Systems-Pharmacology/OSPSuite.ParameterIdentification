# Fitting to PK parameter targets

### Introduction

Parameter identification typically fits a model to a full time profile.
In some workflows only a single PK metric is available or relevant as a
target: for example, a C_max reconstructed from a human biomonitoring
study, or an AUC derived from a non-compartmental analysis. In these
cases, optimising the full time-series objective is unnecessary.

`PKOutputMapping` supports this by linking a simulation quantity to a
scalar PK metric target. The optimizer adjusts the free parameter until
the simulated metric matches the target. Any parameter that has a
monotonic effect on the metric can be used.

PK-metric optimization is a separate mode from standard observed
time-series fitting. A `ParameterIdentification` run accepts either
`pkOutputMappings` or `outputMappings` (observed data via
`PIOutputMapping`), not both simultaneously.

### Workflow

A PK metric optimization task requires three components:

- A `Simulation` object loaded via
  [`loadSimulation()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/loadSimulation.html).
- A `PIParameters` object wrapping the parameter to optimize.
- One or more `PKOutputMapping` objects, each specifying a simulated
  quantity, a PK metric, and the target value.

#### Simulation

``` r

library(ospsuite.parameteridentification)

sim <- loadSimulation(
  system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
)
```

#### Parameter to optimize

The parameter is retrieved by its full path in the simulation.
`minValue` and `maxValue` define the search space.

``` r

doseParam <- getParameter(
  path = "Events|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
  container = sim
)

piParameterDose <- PIParameters$new(parameters = list(doseParam))
piParameterDose$minValue <- 0.0001 # 100 mg in kg
piParameterDose$maxValue <- 0.001 # 1 g in kg
print(piParameterDose)
#> <PIParameters>
#>   • Number of parameters: 1
#>   • Value: 0.00025
#>   • Start value: 0.00025
#>   • Min value: 1e-04
#>   • Max value: 0.001
#>   • Unit: kg
```

#### Output mapping

`PKOutputMapping` connects a simulation quantity to a target PK
parameter. The `pkParameter` argument must be one of
[`ospsuite::StandardPKParameter`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/StandardPKParameter.html).
The target value must be given in a unit compatible with the PK
parameter’s dimension.

``` r

plasmaConc <- getQuantity(
  path = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  container = sim
)

pkOutputMapping <- PKOutputMapping$new(
  quantity = plasmaConc,
  pkParameter = "C_max",
  targetValue = 30,
  targetUnit = "µmol/l"
)
print(pkOutputMapping)
#> <PKOutputMapping>
#>   • Quantity path: Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral
#>   Venous Blood)
#>   • PK parameter: C_max
#>   • Target value: 30 µmol/l
```

If the PK parameter has a different physical dimension than the quantity
(e.g. `"AUC_tEnd"` relative to a concentration output), provide
`targetUnit` in a unit consistent with what
[`calculatePKAnalyses()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/calculatePKAnalyses.html)
returns for that PK parameter, which is the base unit of the PK
parameter’s dimension.

### Running the optimization

Pass the mapping via `pkOutputMappings` to `ParameterIdentification`.

``` r

piTask <- ParameterIdentification$new(
  simulations = sim,
  parameters = piParameterDose,
  pkOutputMappings = pkOutputMapping
)

piResult <- piTask$run()
```

### Inspecting results

``` r

print(piResult)
#> <PKResult>
#> Optimization Summary:
#>   • Algorithm: BOBYQA
#>   • Convergence: TRUE
#>   • Objective value: 0.00e+00
#>   • Iterations: 13
#>   • Function evaluations: 13
#>   • Elapsed: 2.593 s
#> PK Parameters:
#>   • C_max: Target = 30.00 µmol/l, Achieved = 30.00 µmol/l
```

``` r

piResult$toDataFrame()[, c(
  "pkParameter",
  "targetValue",
  "targetUnit",
  "achievedValue",
  "estimatedValue",
  "parameterUnit"
)]
#>   pkParameter targetValue targetUnit achievedValue estimatedValue parameterUnit
#> 1       C_max          30     µmol/l            30   0.0001492457            kg
```

The data frame contains one row per output mapping with the estimated
parameter value (`estimatedValue`), its unit (`parameterUnit`), the
achieved PK metric value (`achievedValue`), and the target
(`targetValue`, `targetUnit`).

`$toList()` returns the full result including convergence status, number
of iterations, elapsed time, and the algorithm used.

### Multiple output mappings

When multiple `PKOutputMapping` objects are supplied, the optimizer
minimises the sum of normalised squared differences across all mappings
simultaneously.

``` r

pkOutputMappingCmax <- PKOutputMapping$new(
  quantity = plasmaConc,
  pkParameter = "C_max",
  targetValue = 30,
  targetUnit = "µmol/l"
)

pkOutputMappingAuc <- PKOutputMapping$new(
  quantity = plasmaConc,
  pkParameter = "AUC_tEnd",
  targetValue = 2400,
  targetUnit = "µmol*min/l"
)

piTaskMulti <- ParameterIdentification$new(
  simulations = sim,
  parameters = piParameterDose,
  pkOutputMappings = list(pkOutputMappingCmax, pkOutputMappingAuc)
)

piResultMulti <- piTaskMulti$run()

print(piResultMulti)
#> <PKResult>
#> Optimization Summary:
#>   • Algorithm: BOBYQA
#>   • Convergence: TRUE
#>   • Objective value: 5.90e-05
#>   • Iterations: 13
#>   • Function evaluations: 13
#>   • Elapsed: 2.402 s
#> PK Parameters:
#>   • C_max: Target = 30.00 µmol/l, Achieved = 29.84 µmol/l
#>   • AUC_tEnd: Target = 2400. µmol*min/l, Achieved = 2413. µmol*min/l

piResultMulti$toDataFrame()[, c(
  "pkParameter",
  "targetValue",
  "targetUnit",
  "achievedValue",
  "estimatedValue",
  "parameterUnit"
)]
#>   pkParameter targetValue targetUnit achievedValue estimatedValue parameterUnit
#> 1       C_max          30     µmol/l      29.83637   0.0001484316            kg
#> 2    AUC_tEnd        2400 µmol*min/l    2412.97827   0.0001484316            kg
```

By default `ParameterIdentification` uses BOBYQA. A different algorithm
or iteration limit can be set via `PIConfiguration`; see
[`?PIConfiguration`](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/dev/reference/PIConfiguration.md)
and the [optimization algorithms
vignette](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/dev/articles/optimization-algorithms.md).
