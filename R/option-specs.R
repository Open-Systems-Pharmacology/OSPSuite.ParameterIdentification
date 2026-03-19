#' @noRd
CIOptionSpecs <- list(
  hessian = list(
    epsilon = numericOption(
      min = .Machine$double.eps,
      nullAllowed = TRUE,
      expectedLength = 1
    ),
    confLevel = numericOption(min = 0, max = 1)
  ),
  PL = list(
    epsilon = numericOption(
      min = .Machine$double.eps,
      nullAllowed = TRUE,
      expectedLength = NULL
    ),
    confLevel = numericOption(min = 0, max = 1),
    maxIter = integerOption(min = 1L)
  ),
  bootstrap = list(
    nBootstrap = integerOption(min = 1L),
    confLevel = numericOption(min = 0, max = 1),
    seed = integerOption(nullAllowed = TRUE)
  )
)

#' @noRd
ObjectiveFunctionSpecs <- list(
  objectiveFunctionType = characterOption(allowedValues = c("lsq", "m3")),
  residualWeightingMethod = characterOption(
    allowedValues = c("none", "std", "mean", "error")
  ),
  robustMethod = characterOption(
    allowedValues = c("none", "huber", "bisquare")
  ),
  scaleVar = logicalOption(),
  scaling = characterOption(allowedValues = c("lin", "log")),
  linScaleCV = numericOption(min = 1e-9, max = 1),
  logScaleSD = numericOption(min = 1e-9)
)
