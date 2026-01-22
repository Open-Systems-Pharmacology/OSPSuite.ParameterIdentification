# Confidence intervals

## Introduction

The `ospsuite.parameteridentification` package provides tools to
estimate model parameters by fitting simulations to observed data. After
the optimization step, confidence intervals (CIs) can be used to assess
the uncertainty of the estimated parameters and to evaluate how robust
the results are.

This vignette describes the available methods for confidence interval
estimation, explains how to configure and run them, and shows how to
inspect the results.

## Choosing a CI Method

- **`hessian`** is the default method. It estimates confidence intervals
  from the curvature of the objective function at the optimum. This
  approach is fast and works well when the problem is well-behaved and
  the objective function is approximately quadratic.

- **`PL`** (profile likelihood) computes confidence intervals by
  scanning the likelihood along each parameter. It is more robust for
  parameters near bounds or in situations where the likelihood is not
  well approximated by a quadratic shape, but it is slower.

- **`bootstrap`** generates confidence intervals by repeatedly
  resampling the observed data and re-fitting the model. This method is
  robust and makes only minimal assumptions about the likelihood, but it
  is computationally more intensive than the other methods.

  Resampling depends on the type of observed data:

  - For individual datasets, new samples of individuals are drawn.
  - For aggregated datasets, resampling is based on Gaussian Process
    Regression (GPR).
  - With both types of data, a mixed strategy is applied.
  - User-defined dataset weights are respected during resampling.

  **Note**: When using the bootstrap method, if the parameter estimate
  falls outside the computed confidence interval, which can occur with
  highly skewed distributions, the package automatically reports a
  one-sided confidence interval. The conflicting bound is set to `NA`,
  and the `ciType` field is set to `"one-sided"`. If both bounds
  conflict, `ciType` is set to `"failed"`. For successfully estimated
  two-sided intervals, `ciType` is `"two-sided"`.

## Advanced Configuration Options

Each confidence interval method provides a predefined set of default
options. These defaults are available through the helper objects
`CIOptions_hessian`, `CIOptions_PL`, and `CIOptions_bootstrap`.  
The corresponding object is assigned to `piConfiguration$ciOptions` and
individual fields can then be modified as required.

### Explanation of options

Common options:  
- `epsilon`: numerical step size used in approximations  
- `confLevel`: confidence level (e.g. `0.95` for 95% confidence
intervals)

Additional options for profile likelihood (PL):  
- `maxIter`: maximum number of iterations

Additional options for bootstrap:  
- `nBootstrap`: number of bootstrap resamples  
- `seed`: random seed for reproducibility

If no method is specified, the default is the **`hessian`** method with
its standard options.

### Minimal configuration example

Configuration of confidence intervals is done through the
`PIConfiguration` object, which also holds all other settings for
parameter identification.  
The example below shows how the Hessian method is selected and its
default options adjusted:

``` r
piConfiguration <- PIConfiguration$new()

# select CI method
piConfiguration$ciMethod <- "hessian"

# assign default options and adjust if necessary
piConfiguration$ciOptions <- CIOptions_hessian
piConfiguration$ciOptions$confLevel <- 0.95
```

By default, confidence intervals are estimated immediately after each
optimization run (`autoEstimateCI = TRUE`). To disable this behaviour
and perform CI estimation later, set the flag to `FALSE`:

``` r
piConfiguration$autoEstimateCI <- FALSE
```

In this case, the optimization is executed first and CI estimation is
triggered explicitly when needed, for example to review results before
running the more time-consuming CI calculation.

``` r
piResult <- piTask$run()
piResult <- piTask$estimateCI()
```

## Reading CI results

The printout of a `PIResult` object shows the main outcomes of a
parameter identification run, including parameter estimates, standard
deviations, coefficients of variation, and confidence intervals (if
available).

For programmatic access, the results of a PI run can be exported either
as a data frame or as a list:

``` r
df <- piResult$toDataFrame() # tidy table with estimates, sd, cv, lowerCI, upperCI, ciType, etc.
lst <- piResult$toList() # full details including cost information and CI-specific details
```

The list output also contains a `ciDetails` entry where method-specific
information is stored:

- `hessian`: Hessian matrix, covariance matrix, eigenvalues, and
  correlation matrix
- `PL`: parameter history and likelihood thresholds
- `bootstrap`: bootstrap replicates, empirical confidence bounds, and
  the seed used for resampling

## Practical tips

- Start with the `hessian` method as it is the default and runs quickly.
  If confidence intervals appear implausible (too wide, asymmetric, or
  `NA`), repeat the analysis with `bootstrap`.

- When CI calculation fails, check parameter bounds, scaling (`lin` or
  `log`), and residual weighting. Poorly chosen settings in these areas
  often prevent reliable CI estimation.

- For `bootstrap`, begin with a small number of resamples (for example
  100–200) to test the setup, then increase as needed. Always set a
  `seed` if reproducible results are required.
