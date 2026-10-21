# Stores current simulation output state

Stores the output selections, output intervals, and output time points
of simulations in their current state.

## Usage

``` r
.storeSimulationState(simulations)
```

## Arguments

- simulations:

  List of `Simulation` objects

## Value

A named list with names being the IDs of the simulations. Every entry is
a list with the paths of the output selections (`outputs`), the output
time points (`timePoints`), the name, start time, end time and
resolution of every output interval (`intervals`), and the output
interval objects (`intervalObjects`).
