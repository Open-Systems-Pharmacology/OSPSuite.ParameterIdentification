# Optimization Algorithms

Optimization algorithms supported by optimization routines, available
for use in the `ParameterIdentification` class. These algorithms are
configured via the `PIConfiguration` class.

## Usage

``` r
Algorithms
```

## Format

An object of class `list` of length 3.

## Details

Supported algorithms include:

- **`HJKB`** – Hooke-Jeeves algorithm from the dfoptim package.

- **`BOBYQA`** – BOBYQA algorithm from the nloptr package.

- **`DEoptim`** – Differential evolution algorithm from the DEoptim
  package, suitable for stochastic global optimization.

  These algorithms can be specified and configured within the
  `PIConfiguration` class to tailor the parameter identification process
  to specific needs.
