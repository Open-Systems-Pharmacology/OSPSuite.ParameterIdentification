# Confidence Interval Estimation Options

Default options for various confidence interval estimation methods used
within the package. These methods are configured via the
`PIConfiguration` class.

## Usage

``` r
CIOptions_Hessian

CIOptions_PL

CIOptions_Bootstrap
```

## Format

A list containing default settings for confidence interval estimation
methods.

An object of class `list` of length 3.

An object of class `list` of length 3.

## CIOptions_Hessian

Default options for the **`hessian`** method.

- **`epsilon`**: Numerical step size for numerical differentiation.
  Default is `NULL`, which applies an adaptive step size.

- **`confLevel`**: Confidence level for interval estimation. Default is
  `0.95` (95% confidence intervals).

## CIOptions_PL

Default options for the **`PL`** (Profile Likelihood) method.

- **`epsilon`**: Numerical step size for profile likelihood adjustments.
  Default is `NULL`, which applies an adaptive step size.

- **`confLevel`**: Confidence level for interval estimation. Default is
  `0.95` (95% confidence intervals).

- **`maxIter`**: Maximum number of iterations for likelihood profiling.
  Default is `100`.

## CIOptions_Bootstrap

Default options for the **`bootstrap`** method.

- **`nSamples`**: Number of bootstrap resampling iterations. Default is
  `1000`.

- **`confLevel`**: Confidence level for interval estimation. Default is
  `0.95` (95% confidence intervals).

- **`seed`**: Random seed for reproducibility. Default is `NULL`.
