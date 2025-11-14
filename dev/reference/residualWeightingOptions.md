# Residual Weighting Methods for Cost Function

Methods for weighting residuals in the model cost function, affecting
how discrepancies between simulations and observed data are evaluated.

## Usage

``` r
residualWeightingOptions
```

## Format

An object of class `list` of length 4.

## Details

The methods include:

- **`none`** – No weighting applied, treating all residuals equally.

- **`error`** – Weights based on error estimates for the dependent
  variable in observed data.

- **`std`** – Weights equal to the reciprocal of the standard deviation
  of the observed data.

- **`mean`** – Weights based on the reciprocal of the mean of the
  absolute values of the observed data, useful for relative error
  scaling.
