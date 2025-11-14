# Stores current simulation output state

Stores simulation output intervals, output time points, and output
selections in the current state.

## Usage

``` r
.storeSimulationState(simulations)
```

## Arguments

- simulations:

  List of `Simulation` objects

## Value

A named list with entries `outputIntervals`, `timePoints`, and
`outputSelections`. Every entry is a named list with names being the IDs
of the simulations.
