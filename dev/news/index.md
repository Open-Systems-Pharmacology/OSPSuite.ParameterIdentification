# Changelog

## ospsuite.parameteridentification (development version)

### Breaking changes

- Minimum required `ospsuite` version is now 13.0.1. The vignettes and
  tests use the example `Aciclovir.pkml` shipped with `ospsuite` 13.0.1,
  in which the dose parameter has a new path
  ([\#313](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/313)).

### Major changes

- `ParameterIdentification` now converts parameter values from
  `PIParameters$unit` to the base unit before applying them to the
  model. Previously they were applied unconverted, so a non-base unit
  silently optimized the wrong quantity
  ([\#300](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/300)).
- The objective function of `ParameterIdentification` is faster, most of
  all for tasks with many simulations, parameters and output mappings.
  Apart from the simulations themselves, little work is left in an
  evaluation: the model parameters are looked up once per task, the
  observed data are converted to base units once per call, and the
  simulated values and the cost are calculated on numeric vectors
  instead of `DataCombined` objects and data frames, apart from the
  contribution of censored values with `objectiveFunctionType = "m3"`.
  The results are identical, apart from the correction of the M3 method
  for several LLOQs in one output mapping below
  ([\#317](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/317)).
  In the examples of
  [\#303](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/303),
  an evaluation is about 2 to 2.5 times faster, and 7 to 9 times faster
  where the simulations themselves are fast
  ([\#302](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/302),
  [\#303](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/303)).

### Minor improvements and bug fixes

- `ParameterIdentification` now reads the observed data once per call of
  a method and per bootstrap replicate and caches them, instead of
  re-reading them from the underlying datasets on every objective
  function evaluation. This removes the dominant source of R heap growth
  during long optimizations and bootstrap runs
  ([\#271](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/271)).
- [`plot.modelCost()`](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/dev/reference/plot.modelCost.md)
  now reads the residual columns produced by the cost kernel, so it
  correctly plots raw residuals against time and overlays the weighted
  residuals
  ([\#275](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/275)).
- `ParameterIdentification` can now optimize state-variable parameters
  (those defined by a right-hand-side formula), which previously crashed
  ([\#280](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/280)).
- `PIParameters$new()` accepts optional `minValue`/`maxValue`, errors on
  a zero start value when no bounds are supplied, and rejects zero-width
  bounds (`minValue == maxValue`) that leave nothing to optimize
  ([\#282](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/282)).
- Every method of `ParameterIdentification` that runs the simulations
  (`run()`, `estimateCI()`, `gridSearch()`, `calculateOFVProfiles()` and
  `plotResults()`) reads the observed data again at its start, so
  changes of the observed data sets or of their data transformations
  between two calls take effect. `run()` with `autoEstimateCI = TRUE`
  reads them once, for the optimization and the confidence intervals.
  The output time points of the simulations are set at the first call,
  however: after a change of the x values of the observed data (for
  example with `xOffsets` or `xFactors` of `setDataTransformations()`)
  or with a new data set, the simulated values at new observed times are
  interpolated between the output time points of the first call. With a
  new observed time outside the simulated times, or with
  `objectiveFunctionType = "m3"` and a censored value at a new time that
  was not simulated, the cost of the output mapping is infinite, because
  there is no simulated value; a warning says so once per call. Create a
  new `ParameterIdentification` object to simulate at the new times
  ([\#310](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/310)).
- A failed simulation is now reported by its name and the reason given
  by the simulation engine (“Simulation ‘…’ failed: …”), instead of as
  an error about a `NULL` argument, also with `pkOutputMappings`. If
  other simulations of the task have the same name, the message gives
  the position of the failed simulation in the list of simulations.
  `run()`, `estimateCI()`, `gridSearch()` and `calculateOFVProfiles()`
  no longer show the warning of the simulation engine on every failed
  evaluation. They log a reason in full only at the first failure of its
  kind in a call, and later reasons of that kind with their first line
  only, because a reason can be long, for example the list of all
  variables that became negative. Reasons are of one kind if their first
  lines differ only in numbers, such as the time of the failure.
  `plotResults()` still shows the warning. With `pkOutputMappings`, the
  failure of a simulation that no PK mapping uses does not fail the
  evaluation and shows the warning, as before
  ([\#299](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/299)).
- An error in the observed data sets of an output mapping or in their
  data transformations, for example
  [\#311](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/311)
  with labels for all data sets, now stops `run()`, `estimateCI()`,
  `gridSearch()` and `calculateOFVProfiles()` with its own message, as
  an error in the unit conversion of the observed data already did.
  Previously it was reported as a failed simulation: `run()` and
  `estimateCI()` stopped with “Initial simulation failed.”, and
  `gridSearch()` and `calculateOFVProfiles()` returned an infinite
  objective function value for every point
  ([\#310](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/310)).
- With an LLOQ and `objectiveFunctionType = "lsq"`, a missing simulated
  value still stops the evaluation, now with an explicit message instead
  of an error from `tibble`
  ([\#310](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/310)).
- When no observed or simulated values of an output mapping enter the
  cost, for example because `xOffsets` of `setDataTransformations()`
  shift all observed times below 0, the error of
  `ParameterIdentification` now names the output mapping by its position
  and quantity path and gives the reason. Previously it said “No
  observed data found when calculating cost function.”
  ([\#310](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/310)).
- With `objectiveFunctionType = "m3"`, the contribution of the values
  below the LLOQ is now correct when they have different LLOQs, for
  example in two data sets with different LLOQs in one output mapping.
  Each such value is now compared with the simulated value at its own
  time, and with `linScaleCV` its standard deviation is calculated from
  its own LLOQ. Previously an LLOQ could be compared with the simulated
  value at another time, and a value could get the standard deviation of
  another LLOQ, so the cost was wrong and depended on the order in which
  the data sets were added to the output mapping. With one LLOQ per
  output mapping, the results are unchanged
  ([\#317](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/317)).

## ospsuite.parameteridentification 2.2.0

### Breaking changes

- Minimum required R version is now 4.4, consistent with `ospsuite`.
- `residualWeightingMethod` values `"std"` and `"mean"` have been
  removed from `residualWeightingOptions`. Use
  `outputMapping$scaling = "log"` for proportional error handling or
  `"error"` for inverse-variance weighting.

### Major changes

- `PKOutputMapping` adds PK-metric optimization to
  `ParameterIdentification`: fit one or more parameters to target PK
  metrics (C_max, AUC_tEnd, etc.) supplied as scalar values or computed
  from observed `DataSet` objects. PK-metric mode and observed
  time-series mode (via `PIOutputMapping`) are mutually exclusive within
  a single `ParameterIdentification` run.
- `PIConfiguration` active bindings (`objectiveFunctionOptions`,
  `algorithmOptions`, `ciOptions`) now validate input at assignment
  time, warn on unknown keys, and merge partial lists with current
  settings. Changing `algorithm` or `ciMethod` resets the corresponding
  options and emits a message
  ([\#228](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/228)).
- `ParameterIdentification$plotResults()` migrated from the
  soft-deprecated `{tlf}`-based `ospsuite` plotting functions
  (`plotIndividualTimeProfile()`, `plotObservedVsSimulated()`,
  `plotResidualsVsTime()`) to the new
  [ospsuite.plots](https://www.open-systems-pharmacology.org/OSPSuite.Plots/)-based
  equivalents
  ([`plotTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/plotTimeProfile.html),
  [`plotPredictedVsObserved()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/plotPredictedVsObserved.html),
  [`plotResidualsVsCovariate()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/plotResidualsVsCovariate.html)).
  The `DefaultPlotConfiguration` object is no longer used; axis scales
  are derived directly from each `PIOutputMapping$scaling`, and the
  residual sub-plot now matches the mapping’s scale instead of being
  hard-coded to linear. Visual output of `plotResults()` changes
  accordingly.
- Sub-plot composition in `plotResults()` switched from
  `ospsuite::plotGrid()` to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html);
  the returned objects are now `patchwork` objects rather than the
  previous `ospsuite` plot-grid objects.

### Minor improvements and bug fixes

- `CIOptions_hessian` gains `r` and `d` options to tune the post-hoc
  Hessian CI step. `r` controls the number of iterations (minimum `2`,
  default `4`) and `d` the fractional step size (default `0.1`).
  Reducing `r` lowers the number of objective function evaluations at
  the cost of accuracy
  ([\#215](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/215)).
- `residualWeightingMethod = "error"` now computes arithmetic SD from
  geometric standard deviation without approximation. A warning is
  issued when any error values are invalid and fall back to unit weights
  ([\#255](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/255)).
- Re-enabled
  [`plotOFVProfiles()`](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/dev/reference/plotOFVProfiles.md)
  for visualizing OFV profiles produced by
  `ParameterIdentification$calculateOFVProfiles()`
  ([\#91](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/91)).
- New `Imports`: `patchwork` (used to compose the sub-plots produced by
  `plotResults()`).
- `ParameterIdentification$estimateCI()` now resets the objective
  function evaluation counter before each bootstrap iteration and each
  profile likelihood step, so `printEvaluationFeedback` output restarts
  from 1 for every sub-optimization
  ([\#238](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/238)).
- `ParameterIdentification` now converts observed and simulated data to
  OSPSuite base units before computing residuals, ensuring consistent
  and reproducible OFV values
  ([\#229](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/229),
  [\#237](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/237)).
- `PIResult$toDataFrame()` now returns one row per parameter path for
  grouped `PIParameters`, instead of only the first path
  ([\#230](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/230)).
- Removed
  [`clearOutputIntervals()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/clearOutputIntervals.html)
  call from `ParameterIdentification` initialization, which could lead
  to wrong simulation results when events are triggered in time
  intervals without observed data
  ([\#226](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/226)).

## ospsuite.parameteridentification 2.1.1

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
- Allow dataSet with a single observation
  ([\#221](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/221))

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
