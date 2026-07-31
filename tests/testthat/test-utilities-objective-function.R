# .calculateCensoredContribution

obsVsPredDf <- readr::read_csv(
  getTestDataFilePath("Aciclovir_obsVsPredDf.csv"),
  show_col_types = FALSE
)

obsDf <- obsVsPredDf[obsVsPredDf$dataType == "observed", ]
predDf <- obsVsPredDf[obsVsPredDf$dataType == "simulated", ]

.blqKernelFixture <- function() {
  # 4 simulated points spanning the observed times; 4 observed points, one of
  # which (yValues 1 at LLOQ 2.5) is censored, the other three uncensored.
  tibble::tibble(
    dataType = c(rep("simulated", 4), rep("observed", 4)),
    xValues = c(1, 2, 3, 4, 1, 2, 3, 4),
    yValues = c(9, 6, 3, 1.5, 10, 5, 4, 1),
    xUnit = "min",
    yUnit = "mol/l",
    xDimension = "Time",
    yDimension = "Concentration (molar)",
    lloq = c(rep(NA_real_, 4), rep(2.5, 4)),
    weights = NA_real_
  )
}

.blqAllCensoredFixture <- function() {
  # Same shape, but every observed value is below LLOQ 2.5 (all censored).
  df <- .blqKernelFixture()
  df$yValues[df$dataType == "observed"] <- c(2, 1.5, 1, 0.5)
  df
}

test_that(".calculateCensoredContribution correctly calculates result with linear scaling", {
  censored <- obsDf$yValues <= 2.5
  lloq <- rep(2.5, sum(censored))
  simCensored <- predDf$yValues[match(obsDf$xValues[censored], predDf$xValues)]
  expected <- sum(
    -2 * log(stats::pnorm((lloq - simCensored) / abs(0.25 * lloq)))
  )
  result <- .calculateCensoredContribution(
    lloq = lloq,
    simulated = simCensored,
    scaling = "lin",
    linScaleCV = 0.25
  )
  expect_equal(result, expected)
  # Changes from the old 0.545702: the old code applied `log(p, base = 10)`
  # unconditionally (even on the "lin" path), so the sqrt/square round-trip
  # was not a no-op with respect to log base. The natural-log fix moves this
  # to 0.545702 * log(10) = 1.256526.
  expect_equal(result, 1.256526, tolerance = 1e-4)
})

test_that(".calculateCensoredContribution correctly calculates result with logarithmic scaling", {
  obsVsPredDfLloq <- obsVsPredDf
  obsVsPredDfLloq$lloq <- 2.5
  obsVsPredDfLog <- .applyLogTransformation(obsVsPredDfLloq)
  obsDfLog <- obsVsPredDfLog[obsVsPredDfLog$dataType == "observed", ]
  predDfLog <- obsVsPredDfLog[obsVsPredDfLog$dataType == "simulated", ]
  censored <- obsDfLog$yValues <= obsDfLog$lloq
  lloq <- obsDfLog$lloq[censored]
  simCensored <- predDfLog$yValues[
    match(obsDfLog$xValues[censored], predDfLog$xValues)
  ]
  sd <- sqrt(log(1 + 0.2^2))
  expected <- sum(-2 * log(stats::pnorm((lloq - simCensored) / sd)))
  result <- .calculateCensoredContribution(
    lloq = lloq,
    simulated = simCensored,
    scaling = "log",
    logScaleSD = sd
  )
  expect_equal(result, expected)
  # Old value 0.437086 used the log10-CV sigma (0.086) and a base-10 log
  # penalty; the natural-log sigma (sqrt(log(1.04)) ~= 0.198) and natural-log
  # penalty together move this to 1.210831.
  expect_equal(result, 1.210831, tolerance = 1e-4)
})

test_that(".calculateCensoredContribution returns 0 when no rows are censored", {
  # obsDf[1, ] has yValues above the LLOQ, so nothing is censored.
  censored <- obsDf$yValues[1] <= 2.5
  lloq <- rep(2.5, sum(censored))
  simCensored <- predDf$yValues[1][censored]
  result <- .calculateCensoredContribution(
    lloq = lloq,
    simulated = simCensored,
    scaling = "lin",
    linScaleCV = 0.2
  )
  expect_equal(result, 0, tolerance = 1e-4)
})

test_that(".calculateCensoredContribution throws errors on invalid options", {
  lloq <- rep(2.5, 3)
  simCensored <- c(1.9, 0.7, 0.1)
  expect_snapshot(
    error = TRUE,
    .calculateCensoredContribution(
      lloq = lloq,
      simulated = simCensored,
      scaling = "invalidOption",
      linScaleCV = 0.2
    )
  )
  expect_snapshot(
    error = TRUE,
    .calculateCensoredContribution(
      lloq = lloq,
      simulated = simCensored,
      scaling = "lin",
      logScaleSD = 0.086
    )
  )
  expect_snapshot(
    error = TRUE,
    .calculateCensoredContribution(
      lloq = lloq,
      simulated = simCensored,
      scaling = "log",
      linScaleCV = 0.2
    )
  )
})

test_that("m3 excludes censored rows from weightedSSR (no double count)", {
  # Build a small combined frame: 3 uncensored + 1 censored observed point,
  # with a simple simulated curve. weightedSSR must equal the SSR over the 3
  # uncensored rows only.
  df <- .blqKernelFixture()
  cost <- .calculateCostMetrics(
    df,
    blqMethod = "m3",
    scaling = "lin",
    linScaleCV = 0.2
  )
  uncCost <- .calculateCostMetrics(
    df[
      df$dataType == "simulated" |
        (df$dataType == "observed" & df$yValues > 2.5),
    ],
    blqMethod = "none",
    scaling = "lin"
  )
  expect_equal(
    cost$costVariables$weightedSSR,
    uncCost$costVariables$weightedSSR
  )
  expect_equal(
    cost$costVariables$nObservations,
    uncCost$costVariables$nObservations
  )
})

test_that("m3 censored contribution uses per-row lin sigma abs(linScaleCV * lloq)", {
  # Two censored points with different LLOQs; expected sigma is per row.
  lloq <- c(2.5, 5)
  sim <- c(1.0, 2.0)
  expected <- sum(-2 * log(stats::pnorm((lloq - sim) / abs(0.2 * lloq))))
  expect_equal(
    .calculateCensoredContribution(lloq, sim, "lin", linScaleCV = 0.2),
    expected
  )
})

test_that("m3 log-scale censored term uses natural-log sigma", {
  lloq <- log(c(2.5))
  sim <- log(c(1.0))
  sd <- sqrt(log(1 + 0.2^2))
  expected <- sum(-2 * log(stats::pnorm((lloq - sim) / sd)))
  expect_equal(
    .calculateCensoredContribution(lloq, sim, "log", logScaleSD = sd),
    expected
  )
})

test_that("all-censored m3 mapping does not error and cost is the censored term", {
  df <- .blqAllCensoredFixture()
  cost <- .calculateCostMetrics(
    df,
    blqMethod = "m3",
    scaling = "lin",
    linScaleCV = 0.2
  )
  expect_equal(cost$costVariables$weightedSSR, 0)
  expect_equal(cost$costVariables$nObservations, 0)
  expect_equal(cost$modelCost, cost$costVariables$M3Contribution)
})

test_that("m3 guard errors when a mapping's LLOQ is entirely NA", {
  obsVsPredDfNoLloq <- obsVsPredDf
  obsVsPredDfNoLloq$lloq <- NA_real_
  expect_snapshot(
    error = TRUE,
    .calculateCostMetrics(
      obsVsPredDfNoLloq,
      blqMethod = "m3",
      scaling = "lin",
      linScaleCV = 0.2
    )
  )
})

test_that("m3 guard errors when a mapping's LLOQ column is absent", {
  obsVsPredDfNoLloqCol <- obsVsPredDf
  obsVsPredDfNoLloqCol$lloq <- NULL
  expect_snapshot(
    error = TRUE,
    .calculateCostMetrics(
      obsVsPredDfNoLloqCol,
      blqMethod = "m3",
      scaling = "lin",
      linScaleCV = 0.2
    )
  )
})

# sumLogSigma and objectiveType

test_that("the kernel reports sumLogSigma as the negated log of the applied weights", {
  # Section 6: sumLogSigma = -sum(log(s * w_i)), from the unrounded product
  # that forms the weighted residuals.
  df <- .blqKernelFixture()
  df$weights <- 2.5
  result <- .calculateCostMetrics(df, blqMethod = "none")
  # s = 1 (scaleVar FALSE), errorWeights = 1, robustWeights = 1, so w_i = 2.5.
  expected <- -sum(rep(log(2.5), result$costVariables$nObservations))
  expect_equal(result$costVariables$sumLogSigma, expected)
})

test_that("sumLogSigma accounts for the scaleVar factor", {
  df <- .blqKernelFixture()
  result <- .calculateCostMetrics(df, blqMethod = "none", scaleVar = TRUE)
  n <- result$costVariables$nObservations
  # s = 1/n, w_i = 1, so each term is log(1/n).
  expect_equal(result$costVariables$sumLogSigma, -n * log(1 / n))
})

test_that("a non-positive total weight is dropped from sumLogSigma but not from the count", {
  # A zero weight means sigma is infinite, so the row carries no likelihood
  # information and must not contribute -Inf. nObservations is deliberately
  # unchanged, because the lsq Hessian CI reads it for its degrees of freedom.
  df <- .blqKernelFixture()
  df$weights <- 1
  observedIdx <- which(df$dataType == "observed")
  df$weights[observedIdx[1]] <- 0
  result <- .calculateCostMetrics(df, blqMethod = "none")
  expect_true(is.finite(result$costVariables$sumLogSigma))
  expect_equal(result$costVariables$sumLogSigma, 0)
  expect_equal(result$costVariables$nObservations, length(observedIdx))
  expect_equal(
    nrow(result$residualDetails),
    result$costVariables$nObservations
  )
})

test_that("the canonical schema gains sumLogSigma and the objective tag", {
  # Mirrors the exact-schema assertions at lines 243-250, which this task's
  # additions change. Kept exact rather than relaxed to a subset check so the
  # schema stays pinned.
  result <- .calculateCostMetrics(obsVsPredDf)
  expect_equal(
    names(result),
    c(
      "modelCost",
      "minLogProbability",
      "objectiveType",
      "costVariables",
      "residualDetails"
    )
  )
  expect_equal(
    names(result$costVariables),
    c("nObservations", "M3Contribution", "rawSSR", "weightedSSR", "sumLogSigma")
  )
})

test_that("every modelCost producer carries an objectiveType tag", {
  result <- .calculateCostMetrics(obsVsPredDf)
  expect_equal(result$objectiveType, "lsq")

  tagged <- .calculateCostMetrics(obsVsPredDf, objectiveType = "mle")
  expect_equal(tagged$objectiveType, "mle")

  errorStructure <- .createErrorCostStructure(objectiveType = "mle")
  expect_equal(errorStructure$objectiveType, "mle")
  expect_equal(errorStructure$costVariables$sumLogSigma, 0)
})

test_that("aggregation preserves the tag and sums the new column", {
  first <- .calculateCostMetrics(obsVsPredDf, objectiveType = "mle")
  second <- .calculateCostMetrics(obsVsPredDf, objectiveType = "mle")
  merged <- .summarizeCostLists(first, second)
  expect_equal(merged$objectiveType, "mle")
  expect_equal(
    merged$costVariables$sumLogSigma,
    2 * first$costVariables$sumLogSigma
  )
})

test_that("the failure substitute aggregates to an infinite cost, never NA", {
  # The per-mapping error path does reach aggregation, so an NA default in the
  # new column would hand the optimizer NA where lsq gives Inf.
  good <- .calculateCostMetrics(obsVsPredDf)
  merged <- .summarizeCostLists(good, .createErrorCostStructure())
  expect_true(is.infinite(merged$costVariables$weightedSSR))
  expect_false(is.na(merged$costVariables$sumLogSigma))
})

# .newModelCost

test_that(".newModelCost builds the canonical schema with index owned by the constructor", {
  result <- .newModelCost(
    modelCost = 12.5,
    minLogProbability = 7.25,
    nObservations = 3,
    weightedSSR = 12.5,
    rawSSR = 14.0,
    M3Contribution = 1.5,
    x = c(1, 2, 3),
    yObserved = c(10, 8, 6),
    ySimulated = c(11, 7, 5),
    scaleFactor = c(1, 1, 1),
    errorWeights = c(1, 1, 1),
    robustWeights = c(1, 1, 1),
    userWeights = c(1, 1, 1),
    totalWeights = c(1, 1, 1),
    rawResiduals = c(1, -1, -1),
    weightedResiduals = c(1, -1, -1),
    index = 2
  )

  expect_s3_class(result, "modelCost")
  expect_equal(
    names(result),
    c(
      "modelCost",
      "minLogProbability",
      "objectiveType",
      "costVariables",
      "residualDetails"
    )
  )
  expect_equal(
    names(result$costVariables),
    c("nObservations", "M3Contribution", "rawSSR", "weightedSSR", "sumLogSigma")
  )
  expect_equal(
    names(result$residualDetails),
    c(
      "index",
      "x",
      "yObserved",
      "ySimulated",
      "scaleFactor",
      "errorWeights",
      "robustWeights",
      "userWeights",
      "totalWeights",
      "rawResiduals",
      "weightedResiduals"
    )
  )
  expect_equal(result$residualDetails$index, c(2, 2, 2))
})

test_that(".newModelCost fills a single NA residual row when per-observation vectors are omitted", {
  result <- .newModelCost(
    modelCost = Inf,
    minLogProbability = Inf,
    nObservations = 1,
    weightedSSR = Inf
  )

  expect_equal(nrow(result$residualDetails), 1)
  expect_equal(result$residualDetails$x, NA_real_)
  expect_equal(result$residualDetails$index, NA_real_)
})

# .createErrorCostStructure

test_that(".createErrorCostStructure shares the canonical schema with kernel output", {
  kernelOut <- .calculateCostMetrics(obsVsPredDf)
  errorOut <- .createErrorCostStructure()

  expect_equal(
    names(errorOut$costVariables),
    names(kernelOut$costVariables)
  )
  expect_equal(
    names(errorOut$residualDetails),
    names(kernelOut$residualDetails)
  )
  expect_equal(errorOut$modelCost, Inf)
})

test_that(".createErrorCostStructure stamps a non-NA index onto the residual row", {
  errorOut <- .createErrorCostStructure(index = 2)
  expect_equal(errorOut$residualDetails$index, 2)
})

test_that(".summarizeCostLists aggregates a kernel output and an error structure without error", {
  kernelOut <- .calculateCostMetrics(obsVsPredDf)
  errorOut <- .createErrorCostStructure()

  merged <- .summarizeCostLists(kernelOut, errorOut)

  expect_equal(merged$modelCost, Inf)
  expect_equal(
    names(merged$costVariables),
    names(kernelOut$costVariables)
  )
  expect_equal(
    nrow(merged$residualDetails),
    nrow(kernelOut$residualDetails) + nrow(errorOut$residualDetails)
  )
  expect_equal(
    merged$costVariables$nObservations,
    kernelOut$costVariables$nObservations + errorOut$costVariables$nObservations
  )
})

# plot.modelCost

test_that("plot.modelCost shows only the raw series when weighting leaves residuals unchanged", {
  result <- .calculateCostMetrics(obsVsPredDf, index = 1)
  # Default weighting is inert, so weighted == raw and the overlay is skipped.
  expect_true(all(
    result$residualDetails$rawResiduals ==
      result$residualDetails$weightedResiduals
  ))
  expect_silent(plot.modelCost(result, legpos = NA))
})

test_that("plot.modelCost overlays the weighted series when weighting changes the residuals", {
  result <- .calculateCostMetrics(obsVsPredDf, robustMethod = "huber")
  # Mirror the predicate that gates the overlay: the weighted series is drawn
  # only when it differs from the raw series.
  expect_false(all(
    result$residualDetails$rawResiduals ==
      result$residualDetails$weightedResiduals
  ))
  vdiffr::expect_doppelganger(
    "model-cost-raw-and-weighted-residuals",
    function() plot.modelCost(result)
  )
})

test_that("plot.modelCost errors on a failed-evaluation cost object with no finite residuals", {
  errorCost <- .createErrorCostStructure()
  expect_true(all(is.na(errorCost$residualDetails$rawResiduals)))
  expect_error(
    plot.modelCost(errorCost),
    regexp = messages$errorNoResidualsToPlot(),
    fixed = TRUE
  )
})

# .applyLogTransformation

test_that(".applyLogTransformation correctly log-transforms `yValues` and `lloq`", {
  obsVsPredDfLog <- .applyLogTransformation(obsVsPredDf)
  expect_snapshot_value(
    obsVsPredDfLog$yValues,
    style = "deparse",
    tolerance = 1e-5
  )
  expect_snapshot_value(
    obsVsPredDfLog$lloq,
    style = "deparse",
    tolerance = 1e-5
  )
})

# .calculateHuberWeights

test_that(".calculateHuberWeights calculates correct weights for standard residuals", {
  residuals <- c(-2, -1, 0, 1, 2)
  expected_weights <- c(1, 1, 1, 1, 1)
  expect_equal(
    .calculateHuberWeights(residuals),
    expected_weights,
    tolerance = 0.01
  )
})

test_that(".calculateHuberWeights returns empty vector for empty residuals", {
  residuals <- numeric(0)
  expect_equal(.calculateHuberWeights(residuals), logical(0))
})

# .calculateBisquareWeights

test_that(".calculateBisquareWeights calculates correct weights for standard residuals", {
  residuals <- c(-2, -1, 0, 1, 2)
  expected_weights <- c(0.841, 0.959, 1, 0.959, 0.841)
  expect_equal(
    .calculateBisquareWeights(residuals),
    expected_weights,
    tolerance = 0.01
  )
})

test_that(".calculateBisquareWeights returns empty vector for empty residuals", {
  residuals <- numeric(0)
  expect_equal(.calculateHuberWeights(residuals), logical(0))
})

# calculateCostMetrics

test_that("calculateCostMetrics returns expected cost metrics for valid input data and default parameters", {
  result <- .calculateCostMetrics(obsVsPredDf)
  expect_s3_class(result, "modelCost")
  expect_true(all(
    c("modelCost", "minLogProbability", "costVariables", "residualDetails") %in%
      names(result)
  ))
})

test_that("calculateCostMetrics returns correct cost metric values for default parameters", {
  result <- .calculateCostMetrics(obsVsPredDf)
  expect_snapshot_value(result, style = "deparse")
})

test_that("calculateCostMetrics stamps the supplied index onto every residual row", {
  result <- .calculateCostMetrics(obsVsPredDf, index = 7)
  expect_equal(
    result$residualDetails$index,
    rep(7, result$costVariables$nObservations)
  )
})

test_that("calculateCostMetrics returns the indexed error structure when the cost is NA", {
  # Push one observed time beyond the simulated range so interpolation yields
  # NA, which propagates to an NA model cost and triggers the fallback.
  obsVsPredDfNA <- obsVsPredDf
  firstObs <- which(obsVsPredDfNA$dataType == "observed")[1]
  obsVsPredDfNA$xValues[firstObs] <- max(obsVsPredDfNA$xValues) * 10

  expect_warning(
    result <- .calculateCostMetrics(obsVsPredDfNA, index = 5),
    regexp = "Invalid model cost"
  )
  expect_s3_class(result, "modelCost")
  expect_equal(result$modelCost, Inf)
  expect_equal(result$residualDetails$index, 5)
})

test_that("calculateCostMetrics with residualWeightingMethod `none` returns expected results", {
  result <- .calculateCostMetrics(obsVsPredDf, residualWeightingMethod = "none")
  expect_s3_class(result, "modelCost")
  expect_snapshot_value(result$modelCost, tolerance = 1e-3)
})

test_that("calculateCostMetrics rejects removed weighting methods std and mean", {
  expect_error(
    .calculateCostMetrics(obsVsPredDf, residualWeightingMethod = "std"),
    regexp = "std"
  )
  expect_error(
    .calculateCostMetrics(obsVsPredDf, residualWeightingMethod = "mean"),
    regexp = "mean"
  )
})

test_that("calculateCostMetrics with residualWeightingMethod `error` returns expected results", {
  # ArithmeticStdDev
  resultArith <- .calculateCostMetrics(
    obsVsPredDf,
    residualWeightingMethod = "error"
  )
  expect_snapshot_value(resultArith$modelCost, tolerance = 1e-3)

  # GeometricStdDev: convert ArithSD to exact lognormal-equivalent GSD values
  # GSD = exp(sqrt(log(1 + CV^2))) where CV = arithSD / mean
  obsVsPredDfGeom <- obsVsPredDf
  obs <- obsVsPredDfGeom[obsVsPredDfGeom$dataType == "observed", ]
  validIdx <- !is.na(obs$yErrorValues) & obs$yErrorValues > 0
  obs$yErrorValues[validIdx] <- exp(
    sqrt(log(1 + (obs$yErrorValues[validIdx] / obs$yValues[validIdx])^2))
  )
  obsVsPredDfGeom[obsVsPredDfGeom$dataType == "observed", ] <- obs
  obsVsPredDfGeom$yErrorType <- "GeometricStdDev"
  resultGeom <- .calculateCostMetrics(
    obsVsPredDfGeom,
    residualWeightingMethod = "error"
  )
  expect_equal(resultArith$modelCost, resultGeom$modelCost, tolerance = 1e-3)
})

test_that(".applyLogTransformation preserves the linear observed values", {
  df <- .blqKernelFixture()
  transformed <- .applyLogTransformation(df)
  expect_true("yValuesLinear" %in% colnames(transformed))
  expect_equal(transformed$yValuesLinear, df$yValues)
  expect_equal(transformed$yValues, log(df$yValues))
})

test_that(".computeErrorWeights converts an arithmetic SD to the log scale", {
  # Section 2.7: sigma_log = sqrt(log(1 + (SD/y)^2)), weight = 1 / sigma_log.
  yValues <- c(10, 4)
  yErrorValues <- c(2, 1)
  cv <- yErrorValues / yValues
  expected <- 1 / sqrt(log(1 + cv^2))
  expect_equal(
    .computeErrorWeights(
      yValues = yValues,
      yErrorValues = yErrorValues,
      yErrorType = rep("ArithmeticStdDev", 2),
      scaling = "log"
    ),
    expected
  )
})

test_that(".computeErrorWeights uses log(GSD) directly on the log scale", {
  # Section 2.7: a geometric SD is already a multiplicative spread.
  yValues <- c(10, 4)
  gsd <- c(1.3, 1.5)
  expected <- 1 / log(gsd)
  expect_equal(
    .computeErrorWeights(
      yValues = yValues,
      yErrorValues = gsd,
      yErrorType = rep("GeometricStdDev", 2),
      scaling = "log"
    ),
    expected
  )
})

test_that("the two error types agree on the log scale as they do on the linear scale", {
  # Mirrors the existing linear-scale agreement test: for
  # GSD = exp(sqrt(log(1 + CV^2))) both formulas must give the same weight.
  yValues <- c(10, 4)
  arithSd <- c(2, 1)
  cv <- arithSd / yValues
  gsd <- exp(sqrt(log(1 + cv^2)))
  expect_equal(
    .computeErrorWeights(
      yValues = yValues,
      yErrorValues = arithSd,
      yErrorType = rep("ArithmeticStdDev", 2),
      scaling = "log"
    ),
    .computeErrorWeights(
      yValues = yValues,
      yErrorValues = gsd,
      yErrorType = rep("GeometricStdDev", 2),
      scaling = "log"
    )
  )
})

test_that(".computeErrorWeights keeps the linear formula under linear scaling", {
  yValues <- c(10, 4)
  yErrorValues <- c(2, 1)
  expect_equal(
    .computeErrorWeights(
      yValues = yValues,
      yErrorValues = yErrorValues,
      yErrorType = rep("ArithmeticStdDev", 2),
      scaling = "lin"
    ),
    1 / yErrorValues
  )
})

test_that("observations below one still receive measured weights on the log scale", {
  # The eligibility guard must be evaluated against the linear reference value.
  # Reading a log-transformed yValues would exclude every row here, silently
  # falling back to unit weights.
  yValues <- c(0.5, 0.2)
  yErrorValues <- c(0.1, 0.05)
  cv <- yErrorValues / yValues
  expected <- 1 / sqrt(log(1 + cv^2))
  expect_equal(
    .computeErrorWeights(
      yValues = yValues,
      yErrorValues = yErrorValues,
      yErrorType = rep("ArithmeticStdDev", 2),
      scaling = "log"
    ),
    expected
  )
})

test_that("the kernel hands the linear reference value to the error weights", {
  # This is the test that pins Step 5's wiring. Every other test in this task
  # calls .computeErrorWeights() directly with linear values, so a skipped or
  # mis-wired handover would leave them all passing while the kernel silently
  # computed weights from log(y_i) — the exact defect spec section 8.1 exists
  # to prevent.
  df <- .blqKernelFixture()
  df$yErrorValues <- 0.4
  df$yErrorType <- "ArithmeticStdDev"
  df$yErrorUnit <- df$yUnit
  observed <- df[df$dataType == "observed", ]
  dfLog <- .applyLogTransformation(df)

  result <- .calculateCostMetrics(
    dfLog,
    residualWeightingMethod = "error",
    scaling = "log"
  )

  # residualDetails stores errorWeights rounded to two decimals.
  cv <- 0.4 / observed$yValues
  expected <- round(1 / sqrt(log(1 + cv^2)), 2)
  expect_equal(result$residualDetails$errorWeights, expected)
  # A mis-wire would fall back to unit weights for every row whose
  # log-transformed value is not positive.
  expect_false(all(result$residualDetails$errorWeights == 1))
})

test_that("robust methods (huber, bisquare) modify the residuals appropriately", {
  resultHuber <- .calculateCostMetrics(obsVsPredDf, robustMethod = "huber")
  resultBisquare <- .calculateCostMetrics(
    obsVsPredDf,
    robustMethod = "bisquare"
  )
  expect_equal(resultHuber$modelCost, 8.94396, tolerance = 1e-4)
  expect_equal(resultBisquare$modelCost, 4.929464, tolerance = 1e-4)
})

test_that("M3 and least-squares kernel costs match recorded values and differ from each other", {
  obsVsPredDfLLOQ <- obsVsPredDf
  obsVsPredDfLLOQ$lloq <- 2.5
  result_lsq <- .calculateCostMetrics(
    df = obsVsPredDfLLOQ,
    blqMethod = "none"
  )
  result_m3 <- .calculateCostMetrics(
    df = obsVsPredDfLLOQ,
    blqMethod = "m3",
    scaling = "lin",
    linScaleCV = 0.2
  )
  expect_equal(result_lsq$modelCost, 677.3902833227, tolerance = 1e-4)
  # Was 677.9181354224 before the M3 rework (double-counted censored rows in
  # weightedSSR, log10 penalty). Now weightedSSR excludes the 3 censored rows
  # (677.1065441631 over 8 uncensored rows) plus the natural-log censored term
  # (1.2154243761), for 678.3219685393.
  expect_equal(result_m3$modelCost, 678.3219685393, tolerance = 1e-4)
  expect_true(result_lsq$modelCost != result_m3$modelCost)
})

test_that(".calculateCostMetrics defaults to no censored contribution", {
  result_default <- .calculateCostMetrics(df = obsVsPredDf)
  result_none <- .calculateCostMetrics(df = obsVsPredDf, blqMethod = "none")
  expect_equal(result_default$modelCost, result_none$modelCost)
})

test_that("lloqHalf substitution reaches the kernel on the log scale", {
  # Row x=4 (yValues 1) is BLQ at lloq 2.5. Log-transform first, mirroring the
  # log-scale .calculateCensoredContribution fixture above, so the kernel's
  # `lloq` column already holds ln(LLOQ); the substituted target is then
  # ln(LLOQ) - ln(2) = ln(LLOQ / 2).
  dfLog <- .applyLogTransformation(.blqKernelFixture())
  result <- .calculateCostMetrics(
    dfLog,
    blqMethod = "lloqHalf",
    scaling = "log"
  )
  blqRow <- result$residualDetails$x == 4
  expect_equal(
    result$residualDetails$yObserved[blqRow],
    log(2.5) - log(2)
  )
})

test_that("calculateCostMetrics correctly scales residuals when scaleVar is TRUE", {
  result_scaled <- .calculateCostMetrics(obsVsPredDf, scaleVar = TRUE)
  result_unscaled <- .calculateCostMetrics(obsVsPredDf, scaleVar = FALSE)
  expect_true(result_scaled$modelCost != result_unscaled$modelCost)
})

test_that("calculateCostMetrics handles infinite values in xValues and yValues correctly", {
  obsVsPredDfInf <- obsVsPredDf
  obsVsPredDfInf$xValues[1] <- Inf
  obsVsPredDfInf$yValues[1] <- -Inf
  expect_silent(.calculateCostMetrics(obsVsPredDfInf))
})

# observed-data caching

currStartValues <- function(task) {
  vapply(task$parameters, function(p) p$startValue, numeric(1))
}

# state-variable parameter routing (issue #156)

test_that("fixture state-variable path is classified as a state variable", {
  sim <- loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  )
  expect_true(
    getParameter(stateVariableParameterPath, container = sim)$isStateVariable
  )
  expect_false(
    getParameter("Aciclovir|Lipophilicity", container = sim)$isStateVariable
  )
})

test_that(".batchInitialization routes state-variable parameters to molecules", {
  task <- testStateVariableMixedTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()

  simId <- names(priv$.simulations)[[1]]

  expect_true(
    stateVariableParameterPath %in% names(priv$.variableMolecules[[simId]])
  )
  expect_false(
    stateVariableParameterPath %in% names(priv$.variableParameters[[simId]])
  )

  expect_true(
    "Aciclovir|Lipophilicity" %in% names(priv$.variableParameters[[simId]])
  )
  expect_false(
    "Aciclovir|Lipophilicity" %in% names(priv$.variableMolecules[[simId]])
  )
})

test_that("objective function delivers the state-variable value into the molecule bucket", {
  task <- testStateVariableMixedTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  simId <- names(priv$.simulations)[[1]]

  # currVals order matches the parameters list: state-variable first, constant
  # second. Values differ from the start values so we can confirm the update
  # is routed into the molecule bucket rather than silently dropped.
  cost <- priv$.objectiveFunction(c(0.06, -0.1))

  expect_true(is.finite(cost$modelCost))
  expect_equal(
    priv$.variableMolecules[[simId]][[stateVariableParameterPath]],
    0.06
  )
  expect_equal(
    priv$.variableParameters[[simId]][["Aciclovir|Lipophilicity"]],
    -0.1
  )
})

test_that("objective function runs with only a state-variable parameter", {
  task <- testStateVariableOnlyTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()

  # .variableParameters is empty for this simulation (empty parametersOrPaths).
  simId <- names(priv$.simulations)[[1]]
  expect_length(priv$.variableParameters[[simId]], 0L)

  cost <- priv$.objectiveFunction(currStartValues(task))
  expect_true(is.finite(cost$modelCost))
})

test_that("state-variable initial value reaches the solver through evaluate", {
  sim <- loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  )
  svQuantity <- getQuantity(stateVariableParameterPath, container = sim)

  stateVar <- stateVarPIParameter(sim)

  # Map the output to the state variable's own quantity so its simulated
  # trajectory is observable. Observed values are placeholders in the state
  # variable's (Volume) dimension.
  obs <- DataSet$new(name = "obs")
  obs$yDimension <- svQuantity$dimension
  obs$setValues(xValues = c(0, 1, 2), yValues = c(0.05, 0.05, 0.05))
  mapping <- PIOutputMapping$new(quantity = svQuantity)
  mapping$addObservedDataSets(obs)

  task <- ParameterIdentification$new(
    simulations = sim,
    parameters = stateVar,
    outputMappings = mapping
  )
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()

  simulatedInitialValue <- function(startValue) {
    df <- priv$.evaluate(startValue, includeObserved = FALSE)[[1]]$toDataFrame()
    df$yValues[which.min(df$xValues)]
  }

  # Two distinct initial values must reach the solver and appear as the
  # simulated initial value, proving the molecule value is consumed downstream
  # (via addRunValues) and not merely stored in the R-side bucket.
  expect_equal(simulatedInitialValue(0.02), 0.02, tolerance = 1e-4)
  expect_equal(simulatedInitialValue(0.09), 0.09, tolerance = 1e-4)
})

test_that(".evaluate omits observed data when includeObserved = FALSE", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()

  dcList <- priv$.evaluate(currStartValues(task), includeObserved = FALSE)
  df <- dcList[[1]]$toDataFrame()

  expect_true("simulated" %in% df$dataType)
  expect_false("observed" %in% df$dataType)
})

test_that(".evaluate includes observed data by default", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()

  dcList <- priv$.evaluate(currStartValues(task))
  df <- dcList[[1]]$toDataFrame()

  expect_true(all(c("simulated", "observed") %in% df$dataType))
})

test_that("objective function builds an observed-data cache and reuses it", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  currVals <- currStartValues(task)

  expect_null(priv$.obsVsPredDfCache)

  cost1 <- priv$.objectiveFunction(currVals)
  expect_false(is.null(priv$.obsVsPredDfCache))

  cost2 <- priv$.objectiveFunction(currVals)
  expect_equal(cost2$modelCost, cost1$modelCost)
})

test_that("cached observed rows match the observed rows of a full evaluation", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  currVals <- currStartValues(task)

  full <- priv$.evaluate(currVals, includeObserved = TRUE)[[1]]$toDataFrame()
  fullObs <- full[full$dataType == "observed", , drop = FALSE]

  priv$.objectiveFunction(currVals)
  cached <- priv$.obsVsPredDfCache[[1]]

  expect_equal(
    as.data.frame(cached, stringsAsFactors = FALSE),
    as.data.frame(fullObs, stringsAsFactors = FALSE)
  )
})

test_that("observed-data cache is invalidated when the bootstrap seed changes", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  currVals <- currStartValues(task)

  priv$.gprModels <- .prepareGPRModels(priv$.outputMappings)
  priv$.objectiveFunction(currVals, bootstrapSeed = 1L)
  expect_false(is.null(priv$.obsVsPredDfCache))

  priv$.getOutputMappings(bootstrapSeed = 2L)
  expect_null(priv$.obsVsPredDfCache)
})

# .computeErrorWeights

test_that(".computeErrorWeights uses exact lognormal SD formula for GeometricStdDev", {
  yValues <- c(10, 20)
  yErrorValues <- c(2.0, 3.0)
  yErrorType <- c("GeometricStdDev", "GeometricStdDev")

  result <- .computeErrorWeights(yValues, yErrorValues, yErrorType)

  # GSD=2, mean=10: sigma=ln(2)=0.6931, exp(sigma^2)-1=0.617, SD=10*sqrt(0.617)=7.854
  # GSD=3, mean=20: sigma=ln(3)=1.0986, exp(sigma^2)-1=2.343, SD=20*sqrt(2.343)=30.62
  expect_equal(result, c(1 / 7.854, 1 / 30.62), tolerance = 1e-3)
})

test_that(".computeErrorWeights warns when error weighting falls back to unit weights", {
  yValues <- c(5, 10)
  yErrorValues <- c(0, 0)
  yErrorType <- c("ArithmeticStdDev", "ArithmeticStdDev")

  expect_warning(
    .computeErrorWeights(yValues, yErrorValues, yErrorType),
    regexp = "unit weights"
  )
})

test_that(".computeErrorWeights warns when some GSD error values are invalid", {
  yValues <- c(10, 20, 15)
  yErrorValues <- c(2.0, 0.5, 3.0)
  yErrorType <- c("GeometricStdDev", "GeometricStdDev", "GeometricStdDev")

  expect_warning(
    .computeErrorWeights(yValues, yErrorValues, yErrorType),
    regexp = "unit weights"
  )
})
