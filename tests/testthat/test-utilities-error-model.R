test_that(".errorModelFor maps the residual weighting method to an error model", {
  expect_equal(.errorModelFor("none"), "constant")
  expect_equal(.errorModelFor("error"), "dataError")
})

test_that(".errorModelFor rejects an unrecognized weighting method", {
  expect_snapshot(error = TRUE, .errorModelFor("geometric"))
})

test_that(".negLogLikelihood concentrates the scale under the constant model", {
  # Section 2.3: NLL = (N/2) log(2 pi) + sumLogSigma + N log(c) + N/2,
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
  # Section 2.3: NLL = (N/2) log(2 pi) + sumLogSigma + weightedSSR / 2.
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
  # Section 5.1: N = 0 is reachable when every retained row is censored under m3.
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
  # Section 5.1: weightedSSR = 0 would give N log(0) = -Inf, so the scale is
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
