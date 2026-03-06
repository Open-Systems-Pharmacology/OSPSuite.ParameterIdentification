# Objective Function Specifications

Specifies supported objective function configurations for error
calculation in parameter optimization. These specifications detail the
allowable options for tailoring how model fit is assessed, configured
within `PIConfiguration`.

## Usage

``` r
ObjectiveFunctionSpecs
```

## Format

An object of class `list` of length 7.

## Details

Includes:

- **`objectiveFunctionType`** – Type of error calculation. Allowed
  values: `lsq` (least squares), `m3"` (M3 method).

- **`residualWeightingMethod`** – Method for weighting residuals.
  Allowed values: `none`, `std`, `mean`, `error`.

- **`robustMethod`** – Approach for robust outlier handling. Allowed
  values: `none`, `huber`, `bisquare`.

- **`scaleVar`** – Boolean for variance scaling. Allowed values: `TRUE`,
  `FALSE`.

- **`scaling`** – Scaling type. Allowed values: `lin` (linear), `log`
  (logarithmic).

- **`linScaleCV`** – Coefficient of variation for linear scaling.
  Numeric range: `1e-9` to `1`.

- **`logScaleSD`** – Standard deviation for log scaling. Numeric range:
  `1e-9` to `Inf`.

  These options directly influence the optimization process by defining
  how discrepancies between simulated and observed data are quantified
  and managed.
