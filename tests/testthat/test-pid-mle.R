# Equivalence theorems between mle and lsq (spec section 12)

obsVsPredDf <- readr::read_csv(
  getTestDataFilePath("Aciclovir_obsVsPredDf.csv"),
  show_col_types = FALSE
)

test_that("mle with the constant error model finds the same estimates as lsq", {
  # Section 2.5: with unit weights the NLL is a strictly increasing function of
  # weightedSSR, so the two objectives share a minimizer even though their
  # values differ. `testPiTask()` builds a fresh `Simulation` on every call
  # (`getTestSimulation()` returns a new memoized closure each time, so the
  # two tasks below never share a live simulation), so the two fits are
  # independent of each other's fitted values by construction. Both tasks are
  # nevertheless constructed, with their configuration set, before either is
  # run, so the shared unfitted baseline is explicit in the test rather than
  # resting on that fixture detail. Both runs share algorithm, bounds, and
  # start values, and the optimizer tolerance is an order of magnitude
  # tighter than the parameter-level comparison below.
  taskLsq <- testPiTask()
  taskLsq$configuration$algorithm <- "BOBYQA"
  taskLsq$configuration$algorithmOptions <- list(xtol_rel = 1e-8, maxeval = 200)

  taskMle <- testPiTask()
  taskMle$configuration$algorithm <- "BOBYQA"
  taskMle$configuration$algorithmOptions <- list(xtol_rel = 1e-8, maxeval = 200)
  taskMle$configuration$objectiveType <- "mle"

  resultLsq <- taskLsq$run()
  # `autoEstimateCI` is on by default, so the run reaches the Hessian estimator,
  # which is still on the least-squares scale under mle and warns about it.
  expect_warning(resultMle <- taskMle$run(), "likelihood scale")

  # Primary assertion: cross-evaluate each objective at the other's estimate.
  # Two independently-converged parameter values only ever agree up to
  # simulation noise (observed gap ~6.4e-5 against a 1e-4 tolerance, a bare
  # 1.6x margin), because that floor is set by the ODE solver, not by
  # `xtol_rel` (four orders of magnitude tighter). Near a smooth optimum the
  # objective is second-order flat in the parameter, so the same ~6.4e-5
  # parameter gap becomes a far smaller, far more robust value gap. This is
  # the theorem's actual claim: each minimizer is equally good under the
  # other objective, not merely numerically close to it.
  privLsq <- taskLsq$.__enclos_env__$private
  privMle <- taskMle$.__enclos_env__$private
  privLsq$.batchInitialization()
  privMle$.batchInitialization()

  mleAtLsqEstimate <- privMle$.objectiveFunction(
    unname(resultLsq$toList()$finalParameters)
  )$modelCost
  lsqAtMleEstimate <- privLsq$.objectiveFunction(
    unname(resultMle$toList()$finalParameters)
  )$modelCost

  expect_equal(
    mleAtLsqEstimate,
    resultMle$toList()$objectiveValue,
    tolerance = 1e-6
  )
  expect_equal(
    lsqAtMleEstimate,
    resultLsq$toList()$objectiveValue,
    tolerance = 1e-6
  )

  # Secondary assertion: the parameter-level comparison is kept for coverage.
  expect_equal(
    unname(resultMle$toList()$finalParameters),
    unname(resultLsq$toList()$finalParameters),
    tolerance = 1e-4
  )
  # Same minimizer, different objective value.
  expect_false(isTRUE(all.equal(
    resultMle$toList()$objectiveValue,
    resultLsq$toList()$objectiveValue
  )))
})

test_that("the mle objective value equals the likelihood of the reported statistics", {
  task <- testPiTask()
  task$configuration$objectiveType <- "mle"
  task$configuration$algorithm <- "BOBYQA"
  task$configuration$algorithmOptions <- list(maxeval = 20)
  # See above: the default Hessian CI warns under mle.
  expect_warning(result <- task$run(), "likelihood scale")
  cost <- result$toList()$costDetails

  expect_equal(cost$objectiveType, "mle")
  expect_equal(
    cost$modelCost,
    .negLogLikelihood(
      weightedSSR = cost$costVariables$weightedSSR,
      nObservations = cost$costVariables$nObservations,
      sumLogSigma = cost$costVariables$sumLogSigma,
      errorModel = "constant"
    ) +
      cost$costVariables$M3Contribution
  )
})

test_that("a shared scale is concentrated across mappings, not within them", {
  # Section 2.4: the NLL is not additive across output mappings, which is why
  # it is assembled after aggregation. Demonstrated on aggregated statistics.
  first <- .calculateCostMetrics(obsVsPredDf, objectiveType = "mle")
  second <- .calculateCostMetrics(obsVsPredDf, objectiveType = "mle")
  merged <- .summarizeCostLists(first, second)

  aggregateNll <- .negLogLikelihood(
    weightedSSR = merged$costVariables$weightedSSR,
    nObservations = merged$costVariables$nObservations,
    sumLogSigma = merged$costVariables$sumLogSigma,
    errorModel = "constant"
  )
  perMappingSum <- .negLogLikelihood(
    weightedSSR = first$costVariables$weightedSSR,
    nObservations = first$costVariables$nObservations,
    sumLogSigma = first$costVariables$sumLogSigma,
    errorModel = "constant"
  ) *
    2

  # Equal here only because both mappings are identical; the aggregate form is
  # what the design requires, and it must be what is computed.
  expect_equal(aggregateNll, perMappingSum)

  # With unequal mappings the two disagree, which is the non-additivity.
  # The threshold must keep observed rows: the fixture's observed times start at
  # 16.36, so a filter below that leaves the kernel with nothing to score and it
  # stops. 200 keeps 8 simulated and 7 observed rows.
  uneven <- .summarizeCostLists(
    first,
    .calculateCostMetrics(
      obsVsPredDf[obsVsPredDf$xValues < 200, ],
      objectiveType = "mle"
    )
  )
  unevenAggregate <- .negLogLikelihood(
    weightedSSR = uneven$costVariables$weightedSSR,
    nObservations = uneven$costVariables$nObservations,
    sumLogSigma = uneven$costVariables$sumLogSigma,
    errorModel = "constant"
  )
  unevenPerMapping <- .negLogLikelihood(
    weightedSSR = first$costVariables$weightedSSR,
    nObservations = first$costVariables$nObservations,
    sumLogSigma = first$costVariables$sumLogSigma,
    errorModel = "constant"
  ) +
    .negLogLikelihood(
      weightedSSR = uneven$costVariables$weightedSSR -
        first$costVariables$weightedSSR,
      nObservations = uneven$costVariables$nObservations -
        first$costVariables$nObservations,
      sumLogSigma = uneven$costVariables$sumLogSigma -
        first$costVariables$sumLogSigma,
      errorModel = "constant"
    )
  expect_false(isTRUE(all.equal(unevenAggregate, unevenPerMapping)))
})

test_that("mle with data-error weighting rejects observations lacking an error value", {
  # The Aciclovir observed data carry NA error values on some rows, which is
  # also why the lsq suite emits the invalid-error-values warning. Under mle
  # those rows would silently assert sigma = 1 in the y-unit.
  task <- testPiTask()
  task$configuration$objectiveType <- "mle"
  task$configuration$objectiveFunctionOptions <- list(
    residualWeightingMethod = "error"
  )
  expect_snapshot(error = TRUE, task$run())
})

test_that("mle names a non-positive observation rather than blaming its error value", {
  # A pre-dose measurement of 0 with a perfectly good standard deviation is
  # common in PK data. It cannot be scored by the data-error model, because the
  # coefficient of variation is undefined there, but telling the user an error
  # value is missing would be false and unactionable.
  base <- testObservedDataMultiple()$dataSet1
  dataSet <- DataSet$new(name = "preDoseZero")
  dataSet$setValues(
    xValues = base$xValues,
    yValues = replace(base$yValues, 1, 0),
    yErrorValues = rep(1, length(base$yValues))
  )
  dataSet$yErrorType <- "ArithmeticStdDev"

  mapping <- PIOutputMapping$new(
    quantity = getQuantity(path = simOutputPath, container = sim_250mg)
  )
  mapping$addObservedDataSets(dataSet)
  task <- ParameterIdentification$new(
    simulations = sim_250mg,
    parameters = piParameterLipo_250mg,
    outputMappings = mapping
  )
  task$configuration$objectiveType <- "mle"
  task$configuration$objectiveFunctionOptions <- list(
    residualWeightingMethod = "error"
  )
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  startValues <- sapply(priv$.piParameters, `[[`, "startValue")
  expect_snapshot(error = TRUE, priv$.objectiveFunction(startValues))
})

test_that("mle rejects a dataset weight the user set to zero", {
  # Section 5.3: unlike lsq, a likelihood reads a zero weight as an infinite
  # residual standard deviation, not as an excluded point.
  task <- testPiTask()
  task$configuration$objectiveType <- "mle"
  mapping <- task$outputMappings[[1]]
  mapping$setDataWeights(
    stats::setNames(list(0), names(mapping$observedDataSets)[[1]])
  )
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  startValues <- sapply(priv$.piParameters, `[[`, "startValue")
  expect_snapshot(error = TRUE, priv$.objectiveFunction(startValues))
})

test_that("the mle preconditions run on every entry point, not only run()", {
  # The guards live in the cache-building branch of `.objectiveFunction()`. A
  # cache filled by an earlier lsq evaluation must not let a later mle
  # evaluation through unchecked, which would seed the fabricated sigma = 1 the
  # data-error guard exists to prevent.
  task <- testPiTask()
  task$configuration$objectiveFunctionOptions <- list(
    residualWeightingMethod = "error"
  )
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  startValues <- sapply(priv$.piParameters, `[[`, "startValue")
  # lsq tolerates the missing error values by falling back to unit weights, and
  # leaves the observed rows cached.
  suppressWarnings(priv$.objectiveFunction(startValues))
  expect_false(is.null(priv$.obsVsPredDfCache))

  task$configuration$objectiveType <- "mle"
  expect_snapshot(
    error = TRUE,
    task$calculateOFVProfiles(totalEvaluations = 2L)
  )
  expect_snapshot(error = TRUE, task$gridSearch(totalEvaluations = 2))
})

test_that("the mle objective is finalized on the aggregate of all mappings", {
  # Section 2.4: the likelihood is assembled once, after aggregation, because
  # the residual scale is shared across output mappings and cannot be
  # concentrated within each one. Verified on the real two-mapping pipeline with
  # deliberately unequal mappings, since identical mappings make the aggregate
  # and the per-mapping sum coincide.
  observedSets <- testObservedDataMultiple()
  buildMapping <- function(simulation, dataSet) {
    mapping <- PIOutputMapping$new(
      quantity = getQuantity(path = simOutputPath, container = simulation)
    )
    mapping$addObservedDataSets(dataSet)
    mapping
  }

  taskBoth <- ParameterIdentification$new(
    simulations = list(sim_250mg, sim_500mg),
    parameters = piParameterLipo,
    outputMappings = list(
      buildMapping(sim_250mg, observedSets$dataSet1),
      buildMapping(sim_500mg, observedSets$dataSet2)
    )
  )
  taskBoth$configuration$objectiveType <- "mle"
  privBoth <- taskBoth$.__enclos_env__$private
  privBoth$.batchInitialization()
  startValues <- sapply(privBoth$.piParameters, `[[`, "startValue")
  aggregate <- privBoth$.objectiveFunction(startValues)

  # The first mapping alone, scored identically. Its statistics subtracted from
  # the aggregate give the second mapping's, since `.summarizeCostLists()` sums
  # `costVariables` element-wise.
  taskFirst <- ParameterIdentification$new(
    simulations = sim_250mg,
    parameters = piParameterLipo_250mg,
    outputMappings = buildMapping(sim_250mg, observedSets$dataSet1)
  )
  taskFirst$configuration$objectiveType <- "mle"
  privFirst <- taskFirst$.__enclos_env__$private
  privFirst$.batchInitialization()
  first <- privFirst$.objectiveFunction(
    sapply(privFirst$.piParameters, `[[`, "startValue")
  )

  nllOf <- function(costVariables) {
    .negLogLikelihood(
      weightedSSR = costVariables$weightedSSR,
      nObservations = costVariables$nObservations,
      sumLogSigma = costVariables$sumLogSigma,
      errorModel = "constant"
    )
  }
  second <- aggregate$costVariables - first$costVariables

  # Both mappings really did contribute, and unequally.
  expect_equal(second$nObservations, 10)
  expect_false(isTRUE(all.equal(
    first$costVariables$weightedSSR,
    second$weightedSSR
  )))

  # The objective is the likelihood of the aggregated statistics.
  expect_equal(aggregate$modelCost, nllOf(aggregate$costVariables))
  # And not the sum of the two per-mapping likelihoods.
  expect_false(isTRUE(all.equal(
    aggregate$modelCost,
    nllOf(first$costVariables) + nllOf(second)
  )))
})

test_that("mle with the data-error model ranks parameter sets exactly as weighted lsq does", {
  # Section 2.5: with a measured sigma the NLL is an increasing affine function
  # of weightedSSR, so the two objectives induce the same ordering and
  # therefore the same minimizer. Verified directly on the transform, which
  # needs no optimizer run.
  #
  # Kernel-level form chosen over running an optimizer against a cleaned
  # PIOutputMapping: the observed DataSet loaded from the Aciclovir Excel
  # fixture is not cloneable (DataSet is constructed with cloneable = FALSE),
  # so building a clean copy for a full run would mean reconstructing a new
  # DataSet from scratch and re-deriving its units and dimensions. The
  # dataframe fixture used above has no such obstacle, so it is cleaned
  # directly: every observed row without a usable yErrorValues is filled with
  # the mean of the fixture's own valid errors. That fill is synthetic, not a
  # measured sigma; its only purpose is to keep every row eligible so the
  # unit-weight fallback never fires. The affine relationship pinned below
  # holds for any positive, parameter-independent sigma, so the particular
  # fill value chosen does not affect what this test demonstrates.
  cleanDf <- obsVsPredDf
  observedIdx <- cleanDf$dataType == "observed"
  meanValidError <- mean(
    cleanDf$yErrorValues[observedIdx & !is.na(cleanDf$yErrorValues)]
  )
  cleanDf$yErrorValues[observedIdx & is.na(cleanDf$yErrorValues)] <-
    meanValidError
  cleanDf$yErrorType[observedIdx] <- "ArithmeticStdDev"

  dfBetter <- cleanDf
  dfWorse <- cleanDf
  simIdx <- dfWorse$dataType == "simulated"
  dfWorse$yValues[simIdx] <- dfWorse$yValues[simIdx] * 1.2

  better <- .calculateCostMetrics(
    dfBetter,
    residualWeightingMethod = "error",
    objectiveType = "mle"
  )
  worse <- .calculateCostMetrics(
    dfWorse,
    residualWeightingMethod = "error",
    objectiveType = "mle"
  )

  nllBetter <- .negLogLikelihood(
    weightedSSR = better$costVariables$weightedSSR,
    nObservations = better$costVariables$nObservations,
    sumLogSigma = better$costVariables$sumLogSigma,
    errorModel = "dataError"
  )
  nllWorse <- .negLogLikelihood(
    weightedSSR = worse$costVariables$weightedSSR,
    nObservations = worse$costVariables$nObservations,
    sumLogSigma = worse$costVariables$sumLogSigma,
    errorModel = "dataError"
  )

  # The ordering of the likelihood matches the ordering of the weighted SSR.
  expect_true(
    better$costVariables$weightedSSR < worse$costVariables$weightedSSR
  )
  expect_true(nllBetter < nllWorse)
  # And the difference is exactly half the SSR difference, since sumLogSigma is
  # identical for both and sigma does not depend on the parameters.
  expect_equal(
    nllWorse - nllBetter,
    (worse$costVariables$weightedSSR - better$costVariables$weightedSSR) / 2
  )
})
