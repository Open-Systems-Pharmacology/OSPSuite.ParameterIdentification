# Validates Matching IDs across Simulation IDs, PI Parameters, and Output Mappings

Ensures that every Simulation ID is present and matches with
corresponding IDs in `PIParameter` and `OutputMapping` instances. This
function is crucial for maintaining consistency and preventing
mismatches that could disrupt parameter identification processes.

## Usage

``` r
.validateSimulationIds(simulationIds, piParameters, outputMappings)
```

## Arguments

- simulationIds:

  Vector of simulation IDs.

- piParameters:

  List of `PIParameter` instances, from which IDs are extracted and
  validated against `simulationIds`.

- outputMappings:

  List of `OutputMapping` instances, from which IDs are extracted and
  validated against `simulationIds`.

## Value

TRUE if all IDs match accordingly, otherwise throws an error detailing
the mismatch or absence of IDs.
