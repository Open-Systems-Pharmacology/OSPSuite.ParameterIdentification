# Changelog

## ospsuite.parameteridentification (development version)

### Breaking Changes

- Default CI options lists were renamed to `CIOptions_hessian` and
  `CIOptions_bootstrap` for consistency
  ([\#220](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/220)).

### Minor improvements and bug fixes

- Automatically report one-sided confidence intervals when parameter
  estimate falls outside the bootstrap CI in skewed distributions. In
  the result, the opposite bound is set to `NA` and `ciType` is set to
  `one-sided`
  ([\#217](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/217)).
- Allow dataSet with a single observation (`#221`)

## ospsuite.parameteridentification 2.1.0

### Breaking changes

- ´ospsuite.parameteridentification`now requires`ospsuite.utils\`
  version \>= 1.7.0.
- ´ospsuite.parameteridentification`now requires`ospsuite\` version \>=
  12.2.0.
- Optimization and CI results are now returned as a `PIResult` object
  instead of a list. This provides a unified structure and new helper
  methods for summaries and export
  ([\#196](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/196)).

### Major changes

- Optimization backend refactored for improved robustness. No changes to
  the user interface
  ([\#161](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/161),
  [\#186](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/186)).
- `PIOutputMapping`: new `$setDataWeights()` method for assigning
  weights to observed data sets and individual data points
  ([\#178](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/178)).
- New confidence interval methods `PL` (profile likelihood) and
  `bootstrap` are now supported via `PIConfiguration`
  ([\#167](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/167),
  [\#186](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/186),
  [\#188](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/188),
  [\#189](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/189),
  [\#190](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/190)).
- New `PIResult` supports `$print()`, `$toDataFrame()`, and `$toList()`
  methods for summaries, export, and diagnostics
  ([\#196](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/196)).

### Minor improvements and bug fixes

- Improved print outputs for all classes
  ([\#171](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/171)).
- All classes do not inherit from
  [`ospsuite.utils::Printable`](https://www.open-systems-pharmacology.org/OSPSuite.RUtils/reference/Printable.html)
  any more
  ([\#171](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/171)).
- Default settings for CI methods are provided through
  `CIOptions_Hessian`, `CIOptions_PL`, and `CIOptions_Bootstrap`
  ([\#167](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/167)).
- Supports `ArithmeticStdDev` and `GeometricStdDev` error types passed
  in the observed `DataSet` as `yErrorType`. This has an effect when
  `residualWeightingMethod = "error"` is set via
  `PIConfiguration$objectiveFunctionOptions`
  ([\#181](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/181)).
- Harmonized CI output naming: `sd`, `se`, `cv` and `rse` are now used
  consistently and correctly
  ([\#191](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/191)).
- Improved Hessian-based confidence interval estimation: covariance
  matrix scaled for SSR objective functions
  ([\#192](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/192)).
- Optimization and CI estimation validated against PK-Sim results for
  the Aciclovir model, confirming correctness of estimates
  ([\#193](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/193)).
- Confidence interval estimation can be disabled by setting
  `piConfiguration$autoEstimateCI <- FALSE`; it can then be run
  explicitly with `ParameterIdentification$estimateCI()`
  ([\#196](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/196)).
- New vignette on confidence intervals, covering available methods,
  configuration, and result inspection
  ([\#198](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/198)).
- New vignette on data mapping, including adding data weights and set
  data transformation
  ([\#199](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/199)).
- `PIResult` stores `costDetails` from the best (minimum modelCost)
  evaluation rather than the last, ensuring correct output when runs are
  limited or stop early
  ([\#206](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/206)).

## ospsuite.parameteridentification 2.0.2

### Major changes

- `ParameterIdentification$gridSearch()`
  and`ParameterIdentification$calculateOFVProfiles()` are made available
  and refactored for robustness, clarity, and efficiency
  ([\#151](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/151)).

### Minor improvements and fixes

- `ParameterIdentification` will validate observed data availability in
  `PIOutputMapping` during initialization
  ([\#145](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/145)).
- Cache Simulation ID in
  `PIOutputMapping`([\#146](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/146)).
- `PIOutputMapping` will attempt to retrieve the molecular weight for
  unit conversion when adding observed data
  ([\#147](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/147)).
- `ParameterIdentification` now differentiates between simulation
  failures during the first iteration (stopping optimization) and
  subsequent iterations (returning infinite cost structure)
  ([\#148](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/148)).
- Simulation failure in `gridSearch`and `calculateOFVProfile` won’t
  break evaluation and return `Inf` for specific parameters
  ([\#153](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/153)).
- Robust Hessian epsilon calculation in `ParameterIdentification`
  ([\#160](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/160)).

## ospsuite.parameteridentification 2.0.1

### Breaking changes

- Function `getSteadyState` has been removed in favor of
  `getSteadyState` from the {ospsuite} package
  ([\#128](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/128)).
- Function `validateIsOption` has been removed in favor of
  [`ospsuite.utils::validateIsOption`](https://www.open-systems-pharmacology.org/OSPSuite.RUtils/reference/validateIsOption.html)
  ([\#130](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/130)).

### Minor improvements and fixes

- Fix pkgdown build
  ([\#131](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/131))
- Fix bug in simulation ID verification
  ([\#138](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/138))

## ospsuite.parameteridentification 2.0.0

### Breaking changes

- Requires {ospsuite} version 12.0 or later
  ([\#98](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/98),
  [\#114](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/114)).

- `PIConfiguration` now configures the objective function via
  `objectiveFunctionOptions`. Users can specify options directly,
  including `objectiveFunctionType`, `residualWeightingMethod`,
  `robustMethod`, `scaleVar`, and `linScaleCV`
  ([\#100](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/100)).

- `ParameterIdentification$gridSearch()`
  and`ParameterIdentification$calculateOFVProfiles()` functions have
  been disabled until fixed
  ([\#91](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/91),
  [\#92](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/92)).

### Major changes

- New `calculateCostMetrics()` function in `ParameterIdentification`
  enhances model evaluation with integrated configuration via
  `PIConfiguration`. Settings and defaults are specified in
  `ObjectiveFunctionOptions`
  ([\#64](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/64),
  [\#65](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/65),
  [\#100](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/100)).

  - `objectiveFunctionType`: `lsq` for least squares or `m3` for
    censored data maximum likelihood estimation.
  - `residualWeightingMethod`: options include `none`, `std` normalizes
    by standard deviation for variable data, `mean` scales by mean for
    diverse magnitudes, and `error` weights by inverse variance for
    known error data.
  - `robustMethod`: `none` for uniform treatment, `huber` or `bisquare`
    for outlier minimization.
  - `scaleVar`: A boolean indicating whether to scale residuals by the
    number of observations.
  - `linScaleCV`: Numeric coefficient used to calculate standard
    deviation for linear scaling, applied to `lloq` values when
    `m3`method is used.
  - `logScaleSD`: Numeric standard deviation for logarithmic scaling,
    applied to `lloq` values when `m3`method is used.

- New `error-calculation` vignette, explaining error model methodologies
  within the ospsuite.parameteridentification package. The document
  elaborates on the lsq (Least Squares Error) and m3 (Extended Least
  Squares Error for censored data) error models, along with advanced
  customization options for error modeling. This resource aids users in
  refining their parameter identification processes.
  ([\#102](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/102),
  [\#111](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/111)).

- New `optimization-algorithms` vignette, introducing algorithms
  available for parameter estimation and offering insights on their
  optimal application scenarios
  ([\#104](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/104),
  [\#111](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/111)).

- New `user-guide` vignette on parameter identification (PI). This
  vignette provides a comprehensive overview for setting up PI tasks,
  including defining simulations, specifying parameters to be
  identified, mapping model outputs to observed data, and configuring
  optimization tasks. Examples across three complexity levels of models
  demonstrate the package’s functionality in detail
  ([\#48](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/48)).

- New
  [`plot.modelCost()`](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/dev/reference/plot.modelCost.md)
  function for visualizing raw and weighted residuals from `modelCost`
  objects
  ([\#100](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/100)).

- Comprehensive overhaul of documentation, for clarity,
  comprehensiveness, and ease of navigation for all users
  ([\#111](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/111)).

- Unit test overhaul and coverage enhancement, increasing from 67% to
  85%
  ([\#99](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/99),
  [\#100](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/100),
  [\#113](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/113)).

### Minor improvements and fixes

- Enhanced error handling now ensures `ParameterIdentification` tasks
  validate the existence of simulation objects referenced by
  `PIParameters` and `OutputMapping` to prevent runtime errors due to
  missing dependencies
  ([\#117](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/117),
  [\#120](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/120)).

- Enhanced `calculateCostMetrics()` output in `ParameterIdentification`
  offers a more detailed results summary for model evaluation. The
  summary now includes `modelCost`, `minLogProbability`, and a
  `costVariables` dataframe with `scaleFactor`, `nObservations`,
  `M3Contribution`, `SSR` (sum of squared residuals), `weightedSSR`,
  `normalizedSSR`, and `robustSSR`
  ([\#100](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/100)).

- README file conversion to .rmd format and subsequent updates
  ([\#86](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/86),
  [\#93](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/93)).

- Improved error handling in `ParameterIdentification` for cases of
  simulation failure, ensuring consistent and informative error cost
  structure in output
  ([\#66](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/66),
  [\#70](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/70),
  [\#100](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/100)).

- `validateIsOption()` ensures user-specified options adhere to defined
  constraints, enhancing the robustness of user inputs
  ([\#100](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/100)).

- `ParameterIdentification` now directly accesses default optimization
  algorithm options for `BOBYQA`, `HJKB`, and `DEoptim` from their
  respective packages ([nloptr](https://github.com/astamm/nloptr),
  `{dfoptim}`, and [DEoptim](https://github.com/ArdiaD/DEoptim)) if not
  explicitly defined in `PIConfiguration`
  ([\#48](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/48),
  [\#81](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/81)).

- Continuous Integration/Continuous Deployment pipeline improvements
  ([\#95](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/95),
  [\#106](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/106),
  [\#110](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/110))

- Several bug fixes
  ([\#83](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/83),
  [\#109](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/109),
  [\#110](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/110),
  [\#115](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/115),
  [\#119](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/119),
  [\#122](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/122))

## ospsuite.parameteridentification 1.3

### Breaking changes

- The parameter in the `PIConfiguration` class that is controlling the
  feedback at each function evaluation is now called
  `printEvaluationFeedback` instead of `printItera tionFeedback`.

### Major changes

- Added new optimization algorithms: the default local algorithm is now
  an implementation of the BOBYQA algorithm (bounded optimization by
  quadratic approximation) from the
  [nloptr](https://github.com/astamm/nloptr) package; additional local
  algorithm is `HJKB`, a bounded implementation of the Hooke-Jeeves
  derivative-free algorithm from the `{dfoptim}` package; a global
  algorithm is `DEoptim` for differential evolution optimization.

- `FME::modCost()` is re-implemented as part of the parameter
  identification package and used for calculation of residuals.

### Minor bug fixes and improvements

- Calculation of residuals does not fail if observed data contains only
  one time point.
- Calculation of the hessian close to the bounds of parameter values is
  improved.

## ospsuite.parameteridentification 1.2

### Breaking changes

- requires
  [ospsuite](https://github.com/open-systems-pharmacology/ospsuite-r)
  v11.1 or later.

### Minor bug fixes and improvements

- [`getSteadyState()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/getSteadyState.html)
  accepts steady state time individually for each simulation.
