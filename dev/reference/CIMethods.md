# Confidence Interval Estimation Methods

Confidence interval estimation methods supported in the
`ParameterIdentification` class. These methods are configured via the
`PIConfiguration` class.

## Usage

``` r
CIMethods
```

## Format

An object of class `list` of length 3.

## Details

Supported methods:

- **`hessian`** - Hessian-based approximation using the Fisher
  Information Matrix.

- **`PL`** – Profile likelihood estimation, iterating over each
  parameter.

- **`bootstrap`** – Bootstrap resampling to estimate parameter
  uncertainty.

  These methods can be specified and configured within the
  `PIConfiguration` class to customize confidence interval estimation.
