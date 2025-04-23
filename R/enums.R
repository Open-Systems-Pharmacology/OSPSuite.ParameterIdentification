#' Optimization Algorithms
#'
#' Optimization algorithms supported by optimization routines, available for use
#' in the `ParameterIdentification` class. These algorithms are configured via
#' the `PIConfiguration` class.
#'
#' @export
#' @name Algorithms
#' @details Supported algorithms include:
#' - **`HJKB`** – Hooke-Jeeves algorithm from the \pkg{dfoptim} package.
#' - **`BOBYQA`** – BOBYQA algorithm from the \pkg{nloptr} package.
#' - **`DEoptim`** – Differential evolution algorithm from the \pkg{DEoptim}
#' package, suitable for stochastic global optimization.
#'
#' These algorithms can be specified and configured within the `PIConfiguration`
#' class to tailor the parameter identification process to specific needs.
Algorithms <- ospsuite.utils::enum(c(
  "HJKB",
  "BOBYQA",
  "DEoptim"
))

#' Algorithm Options for Optimization Algorithms
#'
#' Default options for various optimization algorithms used within the package.
#' These algorithms are configured via the `PIConfiguration` class.
#'
#' @section AlgorithmOptions_HJKB:
#' Default options for the `HJKB` algorithm. See [dfoptim::hjkb()] for more details
#' on the options.
#'
#' @section AlgorithmOptions_BOBYQA:
#' Default options for the `BOBYQA` algorithm. See [nloptr::nl.opts()] for more
#' details on the options.
#'
#' @section AlgorithmOptions_DEoptim:
#' Default options for the `DEoptim` algorithm. See [DEoptim::DEoptim.control()]
#' for more details on the options.
#'
#' @name AlgorithmOptions
#' @rdname AlgorithmOptions
#' @export
AlgorithmOptions_HJKB <- ospsuite.utils::enum(list(
  tol = 1e-06, maxfeval = Inf, maximize = FALSE,
  target = Inf, info = FALSE
))

#' @rdname AlgorithmOptions
#' @export
AlgorithmOptions_BOBYQA <- ospsuite.utils::enum(
  nloptr::nl.opts() |>
    (\(x) x[names(x) != "algorithm"])()
)

#' @rdname AlgorithmOptions
#' @export
AlgorithmOptions_DEoptim <- ospsuite.utils::enum(
  modifyList(
    DEoptim::DEoptim.control(),
    list(trace = FALSE)
  )
)

#' @noRd
AlgorithmDefaults <- list(
  HJKB = AlgorithmOptions_HJKB,
  BOBYQA = AlgorithmOptions_BOBYQA,
  DEoptim = AlgorithmOptions_DEoptim
)

#' Confidence Interval Estimation Methods
#'
#' Confidence interval estimation methods supported in the `ParameterIdentification`
#' class. These methods are configured via the `PIConfiguration` class.
#'
#' @export
#' @name CIMethods
#' @details Supported methods:
#' - **`hessian`** - Hessian-based approximation using the Fisher Information Matrix.
#' - **`PL`** – Profile likelihood estimation, iterating over each parameter.
#' - **`bootstrap`** – Bootstrap resampling to estimate parameter uncertainty.
#'
#' These methods can be specified and configured within the `PIConfiguration`
#' class to customize confidence interval estimation.
CIMethods <- ospsuite.utils::enum(c(
  "hessian",
  "PL",
  "bootstrap"
))

#' Confidence Interval Estimation Options
#'
#' Default options for various confidence interval estimation methods used within
#' the package. These methods are configured via the `PIConfiguration` class.
#'
#' @format A list containing default settings for confidence interval estimation
#' methods.
#'
#' @section CIOptions_Hessian:
#' Default options for the **`hessian`** method.
#'
#' - **`epsilon`**: Numerical step size for numerical differentiation. Default
#' is `NULL`, which applies an adaptive step size.
#' - **`confLevel`**: Confidence level for interval estimation. Default is `0.95`
#' (95% confidence intervals).
#'
#' @section CIOptions_PL:
#' Default options for the **`PL`** (Profile Likelihood) method.
#'
#' - **`epsilon`**: Numerical step size for profile likelihood adjustments.
#' Default is `NULL`, which applies an adaptive step size.
#' - **`confLevel`**: Confidence level for interval estimation. Default is `0.95`
#' (95% confidence intervals).
#' - **`maxIter`**: Maximum number of iterations for likelihood profiling.
#' Default is `100`.
#'
#' @section CIOptions_Bootstrap:
#' Default options for the **`bootstrap`** method.
#'
#' - **`nSamples`**: Number of bootstrap resampling iterations. Default is `1000`.
#' - **`confLevel`**: Confidence level for interval estimation. Default is `0.95`
#' (95% confidence intervals).
#' - **`seed`**: Random seed for reproducibility. Default is `NULL`.
#'
#' @name CIOptions
#' @rdname CIOptions
#' @export
CIOptions_Hessian <- ospsuite.utils::enum(list(
  epsilon = NULL,
  confLevel = 0.95
))

#' @rdname CIOptions
#' @export
CIOptions_PL <- ospsuite.utils::enum(list(
  epsilon = NULL, # vector or scalar / length not validated
  confLevel = 0.95,
  maxIter = 1000
))

#' @rdname CIOptions
#' @export
CIOptions_Bootstrap <- ospsuite.utils::enum(list(
  nBootstrap = 100,
  confLevel = 0.95,
  seed = NULL
))

#' @noRd
CIDefaults <- list(
  hessian = CIOptions_Hessian,
  PL = CIOptions_PL,
  bootstrap = CIOptions_Bootstrap
)

#' Objective Function Options for Model Fit Assessment
#'
#' Default settings for objective function options in model fit analysis,
#' pivotal for calculating error and fit. These configurations enable tailored
#' analysis approaches, pivotal in the `ParameterIdentification` process for
#' optimizing parameter values against observed data. These options are
#' configured via the `PIConfiguration` class.
#'
#' @export
#' @name ObjectiveFunctionOptions
#' @details Settings include:
#' - **`objectiveFunctionType`** – Type of objective function used. Default is
#' `lsq` (least squares), influencing error calculation.
#' - **`residualWeightingMethod`** – Method for residual weighting. Default is
#' `none`.
#' - **`robustMethod`** – Method for robust outlier handling. Default is `none`
#' (standard analysis).
#' - **`scaleVar`** – Whether residual scaling is applied. Default is `FALSE`.
#' - **`linScaleCV`** – Coefficient of variation for linear scaling. Default is
#' `0.2`.
#' - **`logScaleSD`** – Standard deviation for log scaling. Default is `NULL`.
#'
#' These options are configurable in `PIConfiguration`, directly influencing the
#' `calculateCostMetrics` functionality for detailed model fit assessment.
ObjectiveFunctionOptions <- ospsuite.utils::enum(list(
  objectiveFunctionType = "lsq",
  residualWeightingMethod = "none",
  robustMethod = "none",
  scaleVar = FALSE,
  linScaleCV = 0.2,
  logScaleSD = sqrt(log(1 + 0.2^2, base = 10) / log(10))
))

#' @export
CIOptions <- ospsuite.utils::enum(list(
  method = "hessian",
  epsilon = NULL
))

#' Objective Function Specifications
#'
#' Specifies supported objective function configurations for error calculation
#' in parameter optimization. These specifications detail the allowable options
#' for tailoring how model fit is assessed, configured within `PIConfiguration`.
#'
#' @export
#' @name ObjectiveFunctionSpecs
#' @details Includes:
#' - **`objectiveFunctionType`** – Type of error calculation. Allowed values:
#' `lsq` (least squares), `m3"` (M3 method).
#' - **`residualWeightingMethod`** – Method for weighting residuals. Allowed values:
#' `none`, `std`, `mean`, `error`.
#' - **`robustMethod`** – Approach for robust outlier handling. Allowed values:
#' `none`, `huber`, `bisquare`.
#' - **`scaleVar`** – Boolean for variance scaling. Allowed values: `TRUE`, `FALSE`.
#' - **`scaling`** – Scaling type. Allowed values: `lin` (linear), `log` (logarithmic).
#' - **`linScaleCV`** – Coefficient of variation for linear scaling. Numeric
#' range: `1e-9` to `1`.
#' - **`logScaleSD`** – Standard deviation for log scaling. Numeric range: `1e-9`
#' to `Inf`.
#'
#' These options directly influence the optimization process by defining how
#' discrepancies between simulated and observed data are quantified and managed.
ObjectiveFunctionSpecs <- list(
  objectiveFunctionType = list(
    type = "character",
    allowedValues = c("lsq", "m3")
  ),
  residualWeightingMethod = list(
    type = "character",
    allowedValues = c("none", "std", "mean", "error")
  ),
  robustMethod = list(
    type = "character",
    allowedValues = c("none", "huber", "bisquare")
  ),
  scaleVar = list(type = "logical"),
  scaling = list(type = "character", allowedValues = c("lin", "log")),
  linScaleCV = list(type = "numeric", valueRange = c(1e-9, 1)),
  logScaleSD = list(type = "numeric", valueRange = c(1e-9, 1))
)

#' Scaling Options for Output Mapping
#'
#' Scaling options applicable to output mappings, impacting how simulation
#' results are compared to observed data.
#'
#' @export
#' @name ScalingOptions
#' @details Available scaling options:
#' - **`lin`** – Linear scaling (default).
#' - **`log`** – Logarithmic scaling, used when data spans several orders of
#' magnitude.
ScalingOptions <- ospsuite.utils::enum(c(
  "lin",
  "log"
))

#' Residual Weighting Methods for Cost Function
#'
#' Methods for weighting residuals in the model cost function, affecting how
#' discrepancies between simulations and observed data are evaluated.
#'
#' @export
#' @name residualWeightingOptions
#' @details The methods include:
#' - **`none`** – No weighting applied, treating all residuals equally.
#' - **`error`** – Weights based on error estimates for the dependent variable
#' in observed data.
#' - **`std`** – Weights equal to the reciprocal of the standard deviation of
#' the observed data.
#' - **`mean`** – Weights based on the reciprocal of the mean of the absolute
#' values of the observed data, useful for relative error scaling.
residualWeightingOptions <- ospsuite.utils::enum(c(
  "none",
  "error",
  "std",
  "mean"
))

#' Robust Weighting Methods for Cost Function
#'
#' Robust weighting methods to address outliers in the cost function calculation
#' during parameter optimization.
#'
#' @export
#' @name robustMethodOptions
#' @details The available methods are:
#' - **`none`** – No robust weighting applied.
#' - **`huber`** – Huber weighting for moderate outliers.
#' - **`bisquare`** – Bisquare (Tukey's biweight) weighting for severe outliers.
robustMethodOptions <- ospsuite.utils::enum(c(
  "none",
  "huber",
  "bisquare"
))
