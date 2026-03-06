# Calculate Bisquare Weights for Residuals

This function calculates Bisquare (Tukey's biweight) weights for
residuals, aggressively reducing outlier influence. Scales residuals
using MAD with a cutoff at `c` times MAD.

## Usage

``` r
.calculateBisquareWeights(residuals, c = 4.685)
```

## Arguments

- residuals:

  Numeric vector of residuals.

- c:

  Tuning parameter for outlier exclusion. Default is 4.685.

## Value

Numeric vector of Bisquare weights.
