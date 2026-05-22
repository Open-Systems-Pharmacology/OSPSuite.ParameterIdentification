#' @noRd
CIOptionSpecs <- list(
  hessian = list(
    epsilon = ospsuite.utils::numericOption(
      min = .Machine$double.eps,
      nullAllowed = TRUE,
      expectedLength = NULL
    ),
    r = ospsuite.utils::integerOption(min = 2L, nullAllowed = TRUE),
    d = ospsuite.utils::numericOption(
      min = .Machine$double.eps,
      nullAllowed = TRUE
    ),
    confLevel = ospsuite.utils::numericOption(min = 0, max = 1)
  ),
  PL = list(
    epsilon = ospsuite.utils::numericOption(
      min = .Machine$double.eps,
      nullAllowed = TRUE,
      expectedLength = NULL
    ),
    confLevel = ospsuite.utils::numericOption(min = 0, max = 1),
    maxIter = ospsuite.utils::integerOption(min = 1L)
  ),
  bootstrap = list(
    nBootstrap = ospsuite.utils::integerOption(min = 1L),
    confLevel = ospsuite.utils::numericOption(min = 0, max = 1),
    seed = ospsuite.utils::integerOption(nullAllowed = TRUE)
  )
)

#' @noRd
ObjectiveFunctionSpecs <- list(
  objectiveFunctionType = ospsuite.utils::characterOption(
    allowedValues = c("lsq", "m3")
  ),
  residualWeightingMethod = ospsuite.utils::characterOption(
    allowedValues = c("none", "error")
  ),
  robustMethod = ospsuite.utils::characterOption(
    allowedValues = c("none", "huber", "bisquare")
  ),
  scaleVar = ospsuite.utils::logicalOption(),
  scaling = ospsuite.utils::characterOption(allowedValues = c("lin", "log")),
  linScaleCV = ospsuite.utils::numericOption(min = 1e-9, max = 1),
  logScaleSD = ospsuite.utils::numericOption(min = 1e-9)
)
