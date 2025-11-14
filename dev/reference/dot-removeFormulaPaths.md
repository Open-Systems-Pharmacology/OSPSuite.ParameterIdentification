# Remove paths with formulas

Removes paths to quantities that are defined by an explicit formula in
the simulation

## Usage

``` r
.removeFormulaPaths(paths, simulation, stopIfNotFound = TRUE)
```

## Arguments

- paths:

  List of paths to be filtered

- simulation:

  A `Simulation` object containing the quantities

- stopIfNotFound:

  Boolean. If `TRUE` (default), an error is thrown when a path is not
  found in the simulation.

## Value

List of quantity paths that are not defined by explicit formula.
