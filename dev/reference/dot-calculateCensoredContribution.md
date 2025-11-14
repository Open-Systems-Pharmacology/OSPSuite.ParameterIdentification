# Calculate Contribution of Censored Data

Evaluates the impact of censored data (below quantification limit, BQL)
on model cost, employing maximum likelihood estimation to integrate BQL
observations effectively. By acknowledging BQL data as censored
observations, this method ensures such data contribute to model accuracy
without misrepresenting actual concentrations. It applies linear or
logarithmic scaling to calculate standard deviations for censored
probabilities, enhancing overall model cost assessment with respect to
detection limits.

## Usage

``` r
.calculateCensoredContribution(
  observed,
  simulated,
  scaling,
  linScaleCV = NULL,
  logScaleSD = NULL
)
```

## Arguments

- observed:

  Data frame containing observed data, must include 'lloq', 'xValues',
  'xUnit', 'xDimension', and 'yValues' columns.

- simulated:

  Data frame containing simulated data, must include 'xValues', 'xUnit',
  'xDimension', and 'yValues' columns.

- scaling:

  Character string specifying the scaling method; should be one of the
  predefined scaling options.

- linScaleCV:

  Numeric, coefficient used to calculate standard deviation for linear
  scaling, applied to 'lloq' values.

- logScaleSD:

  Numeric, standard deviation for logarithmic scaling, applied uniformly
  to all censored observations.

## Value

Numeric value representing the sum of squared errors for censored
observations, contributing to the model's total cost.

## Examples

``` r
if (FALSE) { # \dontrun{
.calculateCensoredContribution(observedData, simulatedData, scaling = "lin", linScaleCV = 0.2)
} # }
```
