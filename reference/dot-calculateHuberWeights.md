# Calculate Huber Weights for Residuals

This function calculates Huber weights for residuals, reducing the
influence of outliers. Uses MAD for scaling and applies a cutoff at `k`
times MAD.

## Usage

``` r
.calculateHuberWeights(residuals, k = 1.345)
```

## Arguments

- residuals:

  Numeric vector of residuals.

- k:

  Tuning parameter for outlier cutoff. Default is 1.345.

## Value

Numeric vector of Huber weights.
