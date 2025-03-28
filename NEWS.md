# ospsuite.parameteridentification (development version)

## Breaking changes

- ´{ospsuite.parameteridentification}` now requires `{ospsuite.utils}` version \>= 1.7.0.
- ´{ospsuite.parameteridentification}` now requires `{ospsuite}` version \>= 12.2.0.

## Major changes

- Extend `Optimizer` class with confidence interval calculation (not yet integrated, #167). No visible change for the user.

## Minor improvements and bug fixes

- Improved print outputs for all classes
- All classes do not interit from `ospsuite.utils::Printable` any more.

# ospsuite.parameteridentification 2.0.2

## Major changes

- `ParameterIdentification$gridSearch()` and`ParameterIdentification$calculateOFVProfiles()` are made available and refactored for robustness, clarity, and efficiency (#151).
- New `Optimizer` class for improved stability and extensibility - no changes to user workflow (#161).


## Minor improvements and fixes

- `ParameterIdentification` will validate observed data availability in `PIOutputMapping` during initialization (#145).  
- Cache Simulation ID in `PIOutputMapping`(#146).
- `PIOutputMapping` will attempt to retrieve the molecular weight for unit conversion when adding observed data (#147).  
- `ParameterIdentification` now differentiates between simulation failures during the first iteration (stopping optimization) and subsequent iterations (returning infinite cost structure) (#148).
- Simulation failure in `gridSearch`and `calculateOFVProfile` won't break evaluation and return `Inf` for specific parameters (#153). 
- Robust Hessian epsilon calculation in `ParameterIdentification` (#160).

# ospsuite.parameteridentification 2.0.1

## Breaking changes

- Function `getSteadyState` has been removed in favor of `getSteadyState` from the {ospsuite} package (#128, @PavelBal).
- Function `validateIsOption` has been removed in favor of `ospsuite.utils::validateIsOption` (#130, @Felixmil).

## Minor improvements and fixes

- Fix pkgdown build (#131, @Felixmil)
- Fix bug in simulation ID verification (#138, @rengelke)

# ospsuite.parameteridentification 2.0.0

## Breaking changes

- Requires {ospsuite} version 12.0 or later (#98, #114, @rengelke).

- `PIConfiguration` now configures the objective function via `objectiveFunctionOptions`. Users can specify options directly, including `objectiveFunctionType`, `residualWeightingMethod`, `robustMethod`, `scaleVar`, and `linScaleCV` (#100, @rengelke).

- `ParameterIdentification$gridSearch()` and`ParameterIdentification$calculateOFVProfiles()` functions have been disabled until fixed (#91, #92).


## Major changes

-  New `calculateCostMetrics()` function in `ParameterIdentification` enhances model evaluation with integrated configuration via `PIConfiguration`. Settings and defaults are specified in `ObjectiveFunctionOptions` (#64, #65, #100, @rengelke).
    - `objectiveFunctionType`: `lsq` for least squares or `m3` for censored data maximum likelihood estimation.
    - `residualWeightingMethod`: options include `none`, `std` normalizes by standard deviation for variable data, `mean` scales by mean for diverse magnitudes, and `error` weights by inverse variance for known error data.
    - `robustMethod`: `none` for uniform treatment, `huber` or `bisquare` for outlier minimization.
    - `scaleVar`: A boolean indicating whether to scale residuals by the number of observations.
    - `linScaleCV`: Numeric coefficient used to calculate standard deviation for linear scaling, applied to `lloq` values when `m3`method is used.
    - `logScaleSD`: Numeric standard deviation for logarithmic scaling, applied to `lloq` values when `m3`method is used.

- New `error-calculation` vignette, explaining error model methodologies within the ospsuite.parameteridentification package. The document elaborates on the lsq (Least Squares Error) and m3 (Extended Least Squares Error for censored data) error models, along with advanced customization options for error modeling. This resource aids users in refining their parameter identification processes. (#102, #111, @rengelke).

- New `optimization-algorithms` vignette, introducing algorithms available for parameter estimation and offering insights on their optimal application scenarios (#104, #111, @rengelke). 

- New `user-guide` vignette on parameter identification (PI). This vignette provides a comprehensive overview for setting up PI tasks, including defining simulations, specifying parameters to be identified, mapping model outputs to observed data, and configuring optimization tasks. Examples across three complexity levels of models demonstrate the package's functionality in detail (#48, @svavil, @PavelBal).

- New `plot.modelCost()` function for visualizing raw and weighted residuals from `modelCost` objects (#100, @rengelke).

- Comprehensive overhaul of documentation, for clarity, comprehensiveness, and ease of navigation for all users (#111, @rengelke).

- Unit test overhaul and coverage enhancement, increasing from 67% to 85% (#99, #100, #113, @rengelke).


## Minor improvements and fixes

- Enhanced error handling now ensures `ParameterIdentification` tasks validate the existence of simulation objects referenced by `PIParameters` and `OutputMapping` to prevent runtime errors due to missing dependencies (#117, #120, @rengelke).

- Enhanced `calculateCostMetrics()` output in `ParameterIdentification` offers a more detailed results summary for model evaluation. The summary now includes `modelCost`, `minLogProbability`, and a `costVariables` dataframe with `scaleFactor`, `nObservations`, `M3Contribution`, `SSR` (sum of squared residuals), `weightedSSR`, `normalizedSSR`, and `robustSSR` (#100, @rengelke). 

- README file conversion to .rmd format and subsequent updates (#86, #93, @FelixMil).

- Improved error handling in `ParameterIdentification` for cases of simulation failure, ensuring consistent and informative error cost structure in output (#66, #70, #100, @svavil, @PaveBal, @rengelke).

- `validateIsOption()` ensures user-specified options adhere to defined constraints, enhancing the robustness of user inputs (#100, @rengelke). 

- `ParameterIdentification` now directly accesses default optimization algorithm options for `BOBYQA`, `HJKB`, and `DEoptim` from their respective packages (`{nloptr}`, `{dfoptim}`, and `{DEoptim}`) if not explicitly defined in `PIConfiguration` (#48, #81, @PavelBal).

- Continuous Integration/Continuous Deployment pipeline improvements (#95, #106, #110, @FelixMil)

- Several bug fixes (#83, #109, #110, #115, #119, #122, @PavelBal, @FelixMil, @rengelke)


# ospsuite.parameteridentification 1.3

## Breaking changes

- The parameter in the `PIConfiguration` class that is controlling the feedback at each function evaluation is now called `printEvaluationFeedback` instead of `printItera
tionFeedback`.

## Major changes

- Added new optimization algorithms: the default local algorithm is now an implementation of the BOBYQA algorithm (bounded optimization by quadratic approximation) from the `{nloptr}` package; additional local algorithm is `HJKB`, a bounded implementation of the Hooke-Jeeves derivative-free algorithm from the `{dfoptim}` package; a global algorithm is `DEoptim` for differential evolution optimization.

- `FME::modCost()` is re-implemented as part of the parameter identification package and used for calculation of residuals.

## Minor bug fixes and improvements

- Calculation of residuals does not fail if observed data contains only one time point.
- Calculation of the hessian close to the bounds of parameter values is improved.


# ospsuite.parameteridentification 1.2  

## Breaking changes

- requires `{ospsuite}` v11.1 or later.

## Minor bug fixes and improvements

- `getSteadyState()` accepts steady state time individually for each simulation.
