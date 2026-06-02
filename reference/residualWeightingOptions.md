# Residual Weighting Methods for Cost Function

Methods for weighting residuals in the model cost function, affecting
how discrepancies between simulations and observed data are evaluated.

## Usage

``` r
residualWeightingOptions
```

## Details

The methods include:

- **`none`** – No weighting applied, treating all residuals equally.

- **`error`** – Weights based on error estimates for the dependent
  variable in observed data.
