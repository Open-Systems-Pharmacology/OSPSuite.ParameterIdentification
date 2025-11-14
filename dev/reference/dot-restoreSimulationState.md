# Restore simulation output state

Restore simulation output state

## Usage

``` r
.restoreSimulationState(simulations, simStateList)
```

## Arguments

- simulations:

  List of `Simulation` objects

- simStateList:

  Output of the function `.storeSimulationState`. A named list with
  entries `outputIntervals`, `timePoints`, and `outputSelections`. Every
  entry is a named list with names being the IDs of the simulations.
