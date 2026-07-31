#' Optimization Algorithms
#'
#' Optimization algorithms supported by optimization routines, available for use
#' in the `ParameterIdentification` class. These algorithms are configured via
#' the `PIConfiguration` class.
#'
#' @export
#' @name Algorithms
#' @details Supported algorithms include:
#' - **`HJKB`** - Hooke-Jeeves algorithm from the \pkg{dfoptim} package.
#' - **`BOBYQA`** - BOBYQA algorithm from the \pkg{nloptr} package.
#' - **`DEoptim`** - Differential evolution algorithm from the \pkg{DEoptim}
#'   package, suitable for stochastic global optimization.
#'
#'   These algorithms can be specified and configured within the
#'   `PIConfiguration` class to tailor the parameter identification process to
#'   specific needs.
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
#' @section AlgorithmOptions_HJKB: Default options for the `HJKB` algorithm. See
#'   [dfoptim::hjkb()] for more details on the options.
#'
#' @section AlgorithmOptions_BOBYQA: Default options for the `BOBYQA` algorithm.
#'   See [nloptr::nl.opts()] for more details on the options.
#'
#' @section AlgorithmOptions_DEoptim: Default options for the `DEoptim`
#'   algorithm. See [DEoptim::DEoptim.control()] for more details on the
#'   options.
#'
#' @name AlgorithmOptions
#' @rdname AlgorithmOptions
#' @format NULL
#' @export
AlgorithmOptions_HJKB <- ospsuite.utils::enum(list(
  tol = 1e-06,
  maxfeval = Inf,
  maximize = FALSE,
  target = Inf,
  info = FALSE
))

#' @rdname AlgorithmOptions
#' @format NULL
#' @export
AlgorithmOptions_BOBYQA <- ospsuite.utils::enum(
  nloptr::nl.opts() |>
    (\(x) x[names(x) != "algorithm"])()
)

#' @rdname AlgorithmOptions
#' @format NULL
#' @export
AlgorithmOptions_DEoptim <- ospsuite.utils::enum(
  modifyList(
    DEoptim::DEoptim.control(),
    list(trace = FALSE)
  )
)

#' @rdname AlgorithmOptions
#' @format NULL
#' @export
AlgorithmDefaults <- list(
  HJKB = AlgorithmOptions_HJKB,
  BOBYQA = AlgorithmOptions_BOBYQA,
  DEoptim = AlgorithmOptions_DEoptim
)

#' Confidence Interval Estimation Methods
#'
#' Confidence interval estimation methods supported in the
#' `ParameterIdentification` class. These methods are configured via the
#' `PIConfiguration` class.
#'
#' @export
#' @name CIMethods
#' @details Supported methods:
#' - **`hessian`** - Hessian-based approximation using the Fisher Information Matrix.
#' - **`PL`** - Profile likelihood estimation, iterating over each parameter.
#' - **`bootstrap`** - Bootstrap resampling to estimate parameter uncertainty.
#'
#'   These methods can be specified and configured within the `PIConfiguration`
#'   class to customize confidence interval estimation.
CIMethods <- ospsuite.utils::enum(c(
  "hessian",
  "PL",
  "bootstrap"
))

#' Confidence Interval Estimation Options
#'
#' Default options for various confidence interval estimation methods used
#' within the package. These methods are configured via the `PIConfiguration`
#' class.
#'
#' @format A list containing default settings for confidence interval estimation
#'   methods.
#'
#' @section CIOptions_hessian: Default options for the **`hessian`** method.
#'   The Hessian matrix is computed via [numDeriv::hessian()] using the
#'   Richardson method (the only supported method). The `r` and `d` options map
#'   directly to `method.args` of that function.
#'
#' - **`epsilon`**: Numerical step size for numerical differentiation. Default
#'   is `NULL`, which applies an adaptive step size based on the parameter
#'   values.
#' - **`confLevel`**: Confidence level for interval estimation. Default is `0.95`
#'   (95% confidence intervals).
#' - **`r`**: Number of Richardson iterations. Controls the trade-off between
#'   accuracy and computation time: approximately `(N^2 + N)*r` objective
#'   function evaluations are performed, where `N` is the number of free
#'   parameters.
#'   Must be at least `2`. Default is `NULL`, which uses `numDeriv`'s default
#'   of `4`.
#' - **`d`**: Fractional step size. Smaller values reduce the step size relative
#'   to the parameter value. Default is `NULL`, which uses `numDeriv`'s default
#'   of `0.1`.
#'
#' @section CIOptions_PL: Default options for the **`PL`** (Profile Likelihood)
#'   method.
#'
#' - **`epsilon`**: Numerical step size for profile likelihood adjustments.
#'   Default is `NULL`, which applies an adaptive step size.
#' - **`confLevel`**: Confidence level for interval estimation. Default is `0.95`
#'   (95% confidence intervals).
#' - **`maxIter`**: Maximum number of iterations for likelihood profiling.
#'   Default is `100`.
#'
#' @section CIOptions_bootstrap: Default options for the **`bootstrap`** method.
#'
#' - **`nSamples`**: Number of bootstrap resampling iterations. Default is `1000`.
#' - **`confLevel`**: Confidence level for interval estimation. Default is `0.95`
#'   (95% confidence intervals).
#' - **`seed`**: Random seed for reproducibility. Default is `NULL`.
#'
#' @name CIOptions
#' @rdname CIOptions
#' @format NULL
#' @export
CIOptions_hessian <- ospsuite.utils::enum(list(
  epsilon = NULL,
  confLevel = 0.95,
  r = NULL,
  d = NULL
))

#' @rdname CIOptions
#' @format NULL
#' @export
CIOptions_PL <- ospsuite.utils::enum(list(
  epsilon = NULL, # vector or scalar / length not validated
  confLevel = 0.95,
  maxIter = 1000
))

#' @rdname CIOptions
#' @format NULL
#' @export
CIOptions_bootstrap <- ospsuite.utils::enum(list(
  nBootstrap = 100,
  confLevel = 0.95,
  seed = NULL
))

#' @rdname CIOptions
#' @format NULL
#' @export
CIDefaults <- list(
  hessian = CIOptions_hessian,
  PL = CIOptions_PL,
  bootstrap = CIOptions_bootstrap
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
#' - **`residualWeightingMethod`** - Method for residual weighting. Default is
#'   `none`.
#' - **`robustMethod`** - Method for robust outlier handling. Default is `none`
#'   (standard analysis).
#' - **`scaleVar`** - Whether residual scaling is applied. Default is `FALSE`.
#'
#'   These options are configurable in `PIConfiguration`, directly influencing
#'   the `calculateCostMetrics` functionality for detailed model fit assessment.
ObjectiveFunctionOptions <- ospsuite.utils::enum(list(
  residualWeightingMethod = "none",
  robustMethod = "none",
  scaleVar = FALSE
))

#' Scaling Options for Output Mapping
#'
#' Scaling options applicable to output mappings, impacting how simulation
#' results are compared to observed data.
#'
#' @export
#' @name ScalingOptions
#' @details Available scaling options:
#' - **`lin`** - Linear scaling (default).
#' - **`log`** - Logarithmic scaling, used when data spans several orders of
#'   magnitude.
ScalingOptions <- ospsuite.utils::enum(c(
  "lin",
  "log"
))

#' BLQ Removal Modes
#'
#' Modes selecting which below-limit-of-quantification (BLQ) observations enter
#' the objective function.
#'
#' @export
#' @name BLQRemoveModes
#' @details The available modes are:
#' - **`none`** - Keep all BLQ observations (default).
#' - **`always`** - Remove all BLQ observations (Beal M1, PK-Sim "Always").
#' - **`trailingSingle`** - Keep the first point of each trailing BLQ run per
#'   dataset and remove the rest (Beal M6, PK-Sim "Reduce trailing").
#'
#' @seealso The `error-calculation` vignette for the Beal (2001) method taxonomy.
BLQRemoveModes <- ospsuite.utils::enum(c(
  "none",
  "always",
  "trailingSingle"
))

#' Objective Function Types
#'
#' Scoring modes for the objective function. Least squares is the special case
#' of the likelihood in which the residual standard deviation is constant.
#'
#' @export
#' @name ObjectiveTypes
#' @details The available types are:
#' - **`lsq`** - Least squares. Minimizes the weighted sum of squared residuals.
#'   The default.
#' - **`mle`** - Maximum likelihood. Minimizes the negative log-likelihood, with
#'   the residual standard deviation supplied by the error model.
#'
#' @seealso The `error-calculation` vignette for the scoring mathematics.
ObjectiveTypes <- ospsuite.utils::enum(c(
  "lsq",
  "mle"
))

#' Residual Error Models
#'
#' Rules defining the residual standard deviation consumed by the `mle`
#' objective. Selected from `residualWeightingMethod` rather than configured
#' directly.
#'
#' @keywords internal
#' @noRd
ErrorModels <- ospsuite.utils::enum(c(
  "constant",
  "dataError"
))

#' BLQ Handling Methods
#'
#' Methods selecting how the retained BLQ observations contribute to the cost.
#' Substitution is observed-only: the simulated prediction is never modified,
#' and quantifiable observations (at or above the LLOQ) are never touched.
#' Agreement between a censored observation and a prediction below the LLOQ is
#' `m3`'s job, not the substitution methods'.
#'
#' @export
#' @name BLQMethods
#' @details The available methods are:
#' - **`none`** - Use the BLQ value as stored, with no special handling.
#' - **`lloq`** - Substitute a BLQ observation with the LLOQ (PK-Sim default).
#' - **`lloqHalf`** - Substitute a BLQ observation with half the LLOQ (Beal
#'   M5).
#' - **`m3`** - Add the censored-likelihood contribution (Beal M3).
#'
#' @seealso The `error-calculation` vignette for the Beal (2001) method taxonomy.
BLQMethods <- ospsuite.utils::enum(c(
  "none",
  "lloq",
  "lloqHalf",
  "m3"
))

#' BLQ Options for Censored Data Handling
#'
#' Default parameters consumed by the `m3` BLQ method to derive the standard
#' deviation of the censored-likelihood contribution. Configured via the
#' `blqOptions` field of `PIConfiguration`.
#'
#' @export
#' @name BLQOptions
#' @details Settings include:
#' - **`linScaleCV`** - Coefficient of variation for linear scaling, applied to
#'   `lloq` values. Default is `0.2`.
#' - **`logScaleSD`** - Standard deviation for logarithmic scaling. Default is
#'   derived from a coefficient of variation of `0.2`.
BLQOptions <- ospsuite.utils::enum(list(
  linScaleCV = 0.2,
  logScaleSD = sqrt(log(1 + 0.2^2))
))

#' Residual Weighting Methods for Cost Function
#'
#' Methods for weighting residuals in the model cost function, affecting how
#' discrepancies between simulations and observed data are evaluated.
#'
#' @export
#' @name residualWeightingOptions
#' @details The methods include:
#' - **`none`** - No weighting applied, treating all residuals equally.
#' - **`error`** - Weights based on error estimates for the dependent variable
#'   in observed data.
residualWeightingOptions <- ospsuite.utils::enum(c(
  "none",
  "error"
))

#' Robust Weighting Methods for Cost Function
#'
#' Robust weighting methods to address outliers in the cost function calculation
#' during parameter optimization.
#'
#' @export
#' @name robustMethodOptions
#' @details The available methods are:
#' - **`none`** - No robust weighting applied.
#' - **`huber`** - Huber weighting for moderate outliers.
#' - **`bisquare`** - Bisquare (Tukey's biweight) weighting for severe outliers.
robustMethodOptions <- ospsuite.utils::enum(c(
  "none",
  "huber",
  "bisquare"
))

#' Model Cost Fields for Optimization
#'
#' Available cost fields that can be used as optimization targets in parameter
#' estimation.
#'
#' @name ModelCostFields
#' @details The available field is:
#' - **`modelCost`** - Accesses the RSS-based model cost value (currently the
#'   only supported option).
#'
#' @keywords internal
#' @noRd
ModelCostFields <- ospsuite.utils::enum(c(
  "modelCost"
))
