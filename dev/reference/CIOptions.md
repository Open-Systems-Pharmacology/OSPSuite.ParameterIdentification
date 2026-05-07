# Confidence Interval Estimation Options

Default options for various confidence interval estimation methods used
within the package. These methods are configured via the
`PIConfiguration` class.

## Usage

``` r
CIOptions_hessian

CIOptions_PL

CIOptions_bootstrap

CIDefaults
```

## Format

A list containing default settings for confidence interval estimation
methods.

## CIOptions_hessian

Default options for the **`hessian`** method. The Hessian matrix is
computed via
[`numDeriv::hessian()`](https://rdrr.io/pkg/numDeriv/man/hessian.html)
using the Richardson method (the only supported method). The `r` and `d`
options map directly to `method.args` of that function.

- **`epsilon`**: Numerical step size for numerical differentiation.
  Default is `NULL`, which applies an adaptive step size based on the
  parameter values.

- **`confLevel`**: Confidence level for interval estimation. Default is
  `0.95` (95% confidence intervals).

- **`r`**: Number of Richardson iterations. Controls the trade-off
  between accuracy and computation time: `(N^2 + N)*r + 1` objective
  function evaluations are performed, where `N` is the number of free
  parameters. Must be at least `2`. Default is `NULL`, which uses
  `numDeriv`'s default of `4`.

- **`d`**: Fractional step size. Smaller values reduce the step size
  relative to the parameter value. Default is `NULL`, which uses
  `numDeriv`'s default of `0.1`.

## CIOptions_PL

Default options for the **`PL`** (Profile Likelihood) method.

- **`epsilon`**: Numerical step size for profile likelihood adjustments.
  Default is `NULL`, which applies an adaptive step size.

- **`confLevel`**: Confidence level for interval estimation. Default is
  `0.95` (95% confidence intervals).

- **`maxIter`**: Maximum number of iterations for likelihood profiling.
  Default is `100`.

## CIOptions_bootstrap

Default options for the **`bootstrap`** method.

- **`nSamples`**: Number of bootstrap resampling iterations. Default is
  `1000`.

- **`confLevel`**: Confidence level for interval estimation. Default is
  `0.95` (95% confidence intervals).

- **`seed`**: Random seed for reproducibility. Default is `NULL`.
