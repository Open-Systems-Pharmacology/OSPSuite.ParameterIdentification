# Objective Function Options for Model Fit Assessment

Default settings for objective function options in model fit analysis,
pivotal for calculating error and fit. These configurations enable
tailored analysis approaches, pivotal in the `ParameterIdentification`
process for optimizing parameter values against observed data. These
options are configured via the `PIConfiguration` class.

## Usage

``` r
ObjectiveFunctionOptions
```

## Format

An object of class `list` of length 6.

## Details

Settings include:

- **`objectiveFunctionType`** – Type of objective function used. Default
  is `lsq` (least squares), influencing error calculation.

- **`residualWeightingMethod`** – Method for residual weighting. Default
  is `none`.

- **`robustMethod`** – Method for robust outlier handling. Default is
  `none` (standard analysis).

- **`scaleVar`** – Whether residual scaling is applied. Default is
  `FALSE`.

- **`linScaleCV`** – Coefficient of variation for linear scaling.
  Default is `0.2`.

- **`logScaleSD`** – Standard deviation for log scaling. Default is
  `NULL`.

  These options are configurable in `PIConfiguration`, directly
  influencing the `calculateCostMetrics` functionality for detailed
  model fit assessment.
