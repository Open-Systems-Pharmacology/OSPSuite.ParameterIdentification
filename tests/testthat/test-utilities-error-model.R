obsVsPredDf <- readr::read_csv(
  getTestDataFilePath("Aciclovir_obsVsPredDf.csv"),
  show_col_types = FALSE
)

test_that(".errorModelFor maps the residual weighting method to an error model", {
  expect_equal(.errorModelFor("none"), "constant")
  expect_equal(.errorModelFor("error"), "dataError")
})

test_that(".errorModelFor rejects an unrecognized weighting method", {
  expect_snapshot(error = TRUE, .errorModelFor("geometric"))
})

test_that(".negLogLikelihood concentrates the scale under the constant model", {
  # NLL = (N/2) log(2 pi) + sumLogSigma + N log(c) + N/2,
  # with c = sqrt(weightedSSR / N).
  # N = 4, weightedSSR = 8, sumLogSigma = 0  =>  c = sqrt(2)
  #   2 * log(2 pi) + 0 + 4 * log(sqrt(2)) + 2
  expected <- 2 * log(2 * pi) + 4 * log(sqrt(2)) + 2
  expect_equal(
    .negLogLikelihood(
      weightedSSR = 8,
      nObservations = 4,
      sumLogSigma = 0,
      errorModel = "constant"
    ),
    expected
  )
})

test_that(".negLogLikelihood carries sumLogSigma into the constant model", {
  # Same as above with sumLogSigma = 1.5, which enters additively.
  expected <- 2 * log(2 * pi) + 1.5 + 4 * log(sqrt(2)) + 2
  expect_equal(
    .negLogLikelihood(
      weightedSSR = 8,
      nObservations = 4,
      sumLogSigma = 1.5,
      errorModel = "constant"
    ),
    expected
  )
})

test_that(".negLogLikelihood takes sigma as known under the data-error model", {
  # NLL = (N/2) log(2 pi) + sumLogSigma + weightedSSR / 2.
  # N = 4, weightedSSR = 8, sumLogSigma = 1.5
  expected <- 2 * log(2 * pi) + 1.5 + 4
  expect_equal(
    .negLogLikelihood(
      weightedSSR = 8,
      nObservations = 4,
      sumLogSigma = 1.5,
      errorModel = "dataError"
    ),
    expected
  )
})

test_that(".negLogLikelihood handles a single observation", {
  # N = 1, weightedSSR = 3, sumLogSigma = 0  =>  c = sqrt(3)
  expected <- 0.5 * log(2 * pi) + log(sqrt(3)) + 0.5
  expect_equal(
    .negLogLikelihood(
      weightedSSR = 3,
      nObservations = 1,
      sumLogSigma = 0,
      errorModel = "constant"
    ),
    expected
  )
})

test_that(".negLogLikelihood returns zero when no observation carries information", {
  # N = 0 is reachable when every retained row is censored under m3.
  # Returning 0 keeps modelCost equal to the censored contribution alone.
  expect_equal(
    .negLogLikelihood(
      weightedSSR = 0,
      nObservations = 0,
      sumLogSigma = 0,
      errorModel = "constant"
    ),
    0
  )
  expect_equal(
    .negLogLikelihood(
      weightedSSR = 0,
      nObservations = 0,
      sumLogSigma = 0,
      errorModel = "dataError"
    ),
    0
  )
})

test_that(".negLogLikelihood stays finite for an exact fit", {
  # weightedSSR = 0 would give N log(0) = -Inf, so the scale is
  # floored at .Machine$double.eps.
  floored <- sqrt(.Machine$double.eps)
  expected <- log(2 * pi) + 2 * log(floored) + 1
  expect_equal(
    .negLogLikelihood(
      weightedSSR = 0,
      nObservations = 2,
      sumLogSigma = 0,
      errorModel = "constant"
    ),
    expected
  )
  expect_true(is.finite(
    .negLogLikelihood(
      weightedSSR = 0,
      nObservations = 2,
      sumLogSigma = 0,
      errorModel = "constant"
    )
  ))
})

test_that(".negLogLikelihood rejects an unrecognized error model", {
  expect_snapshot(
    error = TRUE,
    .negLogLikelihood(
      weightedSSR = 8,
      nObservations = 4,
      sumLogSigma = 0,
      errorModel = "proportional"
    )
  )
})

test_that(".finalizeObjective leaves the lsq cost untouched", {
  cost <- .calculateCostMetrics(obsVsPredDf)
  finalized <- .finalizeObjective(cost, "lsq", "constant")
  expect_equal(finalized$modelCost, cost$modelCost)
  expect_identical(finalized, cost)
})

test_that(".finalizeObjective writes the likelihood into modelCost under mle", {
  cost <- .calculateCostMetrics(obsVsPredDf, objectiveType = "mle")
  finalized <- .finalizeObjective(cost, "mle", "constant")
  expected <- .negLogLikelihood(
    weightedSSR = cost$costVariables$weightedSSR,
    nObservations = cost$costVariables$nObservations,
    sumLogSigma = cost$costVariables$sumLogSigma,
    errorModel = "constant"
  )
  expect_equal(finalized$modelCost, expected)
  expect_false(finalized$modelCost == cost$modelCost)
})

test_that(".finalizeObjective adds the censored contribution under mle", {
  # An implementation that overwrites modelCost with the NLL alone
  # would silently drop all censored scoring.
  obsVsPredDfLLOQ <- obsVsPredDf
  obsVsPredDfLLOQ$lloq <- 2.5
  cost <- .calculateCostMetrics(
    df = obsVsPredDfLLOQ,
    blqMethod = "m3",
    scaling = "lin",
    linScaleCV = 0.2,
    objectiveType = "mle"
  )
  finalized <- .finalizeObjective(cost, "mle", "constant")
  nll <- .negLogLikelihood(
    weightedSSR = cost$costVariables$weightedSSR,
    nObservations = cost$costVariables$nObservations,
    sumLogSigma = cost$costVariables$sumLogSigma,
    errorModel = "constant"
  )
  expect_equal(
    finalized$modelCost,
    nll + cost$costVariables$M3Contribution
  )
  expect_true(cost$costVariables$M3Contribution > 0)
})

test_that(".finalizeObjective's non-finite guard actually gates the formula", {
  # Without the guard, nObservations = 0 alone makes
  # .negLogLikelihood() return 0 regardless of weightedSSR (its own
  # nObservations == 0 short-circuit), so weightedSSR = Inf would be masked:
  # modelCost would become the finite 0 + M3Contribution instead of staying
  # Inf. Constructing exactly that combination (Inf weightedSSR, 0
  # observations, a finite M3Contribution) is what makes this test bite: the
  # guarded path leaves modelCost at its already-Inf value, the guard-less
  # path would compute a finite number instead.
  cost <- .createErrorCostStructure(objectiveType = "mle")
  cost$costVariables$nObservations <- 0
  cost$costVariables$M3Contribution <- 5
  finalized <- .finalizeObjective(cost, "mle", "constant")
  expect_equal(finalized$modelCost, Inf)
})
