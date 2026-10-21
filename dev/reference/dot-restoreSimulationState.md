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

  Output of the function `.storeSimulationState`.

## Details

The output selections and the output schema are restored only if they
changed. The output schema gets its original output interval objects
back, so that interval objects held by the user stay part of the
simulation. If a simulation cannot be restored, a warning names it and
the other simulations are still restored, so that the result or the
error of the calling method is kept.
