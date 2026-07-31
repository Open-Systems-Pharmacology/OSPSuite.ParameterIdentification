# ParameterIdentification - auxiliary exploration

resetTestFactories()

# Plot Results

test_that("plotResults() generates expected plot before parameter estimation", {
  piTask <- testPiTask()
  vdiffr::expect_doppelganger("before-estimation", piTask$plotResults()[[1]])
})

piTask <- testPiTask()
suppressMessages(piResults <- piTask$run())
test_that("plotResults() generates expected plot after parameter estimation", {
  vdiffr::expect_doppelganger("after-estimation", piTask$plotResults()[[1]])
})

test_that("plotResults() generates expected plot with parameter input", {
  vdiffr::expect_doppelganger("custom-parameter", piTask$plotResults(1.2)[[1]])
})

test_that("plotResults() errors when `par` length differs from parameter count", {
  twoParameterTask <- ParameterIdentification$new(
    simulations = sim_250mg,
    parameters = list(piParameterLipo_250mg, piParameterCl_250mg),
    outputMappings = outputMapping_250mg
  )
  expect_snapshot(twoParameterTask$plotResults(1.2), error = TRUE)
  expect_snapshot(
    twoParameterTask$plotResults(c(1.2, 3.4, 5.6)),
    error = TRUE
  )
})


# Grid Search

test_that("gridSearch() works without error for single parameter", {
  piTask <- testPiTask()
  expect_no_error(gridSearchResults <- piTask$gridSearch())
})

test_that("gridSearch() works with multiple parameters and default settings", {
  piTask <- ParameterIdentification$new(
    simulations = sim_250mg,
    parameters = list(piParameterLipo_250mg, piParameterCl_250mg),
    outputMappings = outputMapping_250mg
  )
  expect_no_error(gridSearchResults <- piTask$gridSearch())
  expect_snapshot(gridSearchResults[1:10, ])
})

test_that("gridSearch() errors for `logScaleFlag = TRUE` with non-positive values", {
  piTask <- ParameterIdentification$new(
    simulations = sim_250mg,
    parameters = list(piParameterLipo_250mg, piParameterCl_250mg),
    outputMappings = outputMapping_250mg
  )
  expect_no_error(
    piTask$gridSearch(logScaleFlag = c(FALSE, TRUE), totalEvaluations = 3)
  )
  expect_error(
    piTask$gridSearch(logScaleFlag = c(TRUE, TRUE), totalEvaluations = 3),
    messages$logScaleFlagError()
  )
})

test_that("gridSearch() sets new start values with correct message", {
  piParameterLipo_250mg$startValue <- 0
  piParameterCl_250mg$startValue <- 0
  piTask <- ParameterIdentification$new(
    simulations = sim_250mg,
    parameters = list(piParameterLipo_250mg, piParameterCl_250mg),
    outputMappings = outputMapping_250mg
  )
  startValueMessage <- capture_output(
    piTask$gridSearch(setStartValue = TRUE, totalEvaluations = 10)
  )
  expect_snapshot(startValueMessage)
})

test_that("gridSearch() returns `Inf` upon simulation failure", {
  piTask <- ParameterIdentification$new(
    simulations = sim_250mg,
    parameters = list(piParameterLipo_250mg, piParameterCl_250mg),
    outputMappings = outputMapping_250mg
  )
  suppressMessages(suppressWarnings(
    expect_warning(
      gridSearchResults <- piTask$gridSearch(
        lower = c(0, -0.5),
        totalEvaluations = 5
      )
    )
  ))
  expect_snapshot(gridSearchResults$ofv)
})

test_that("gridSearch OFVs are invariant to a non-base parameter unit", {
  pkmlPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  clPath <- "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS-Aciclovir|TSspec"
  outputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  observed <- testObservedData()$`AciclovirLaskinData.Laskin 1982.Group A`

  # Each task loads its own simulation and builds its own PIParameters, so that
  # assigning $unit cannot leak into the shared module-level fixtures.
  clearanceTask <- function(unit) {
    sim <- loadSimulation(pkmlPath, loadFromCache = FALSE, addToCache = FALSE)
    piParameter <- PIParameters$new(
      parameters = list(getParameter(clPath, container = sim))
    )
    piParameter$unit <- unit

    mapping <- PIOutputMapping$new(
      quantity = getQuantity(outputPath, container = sim)
    )
    mapping$addObservedDataSets(observed)

    ParameterIdentification$new(
      simulations = sim,
      parameters = piParameter,
      outputMappings = mapping
    )
  }

  # [1e-4, 1e-3] 1/min and [6e-3, 6e-2] 1/h are the same physical interval.
  # Explicit bounds mean the stale base-unit min/max are never consulted.
  baseGrid <- suppressMessages(
    clearanceTask(ospUnits$`Inversed time`$`1/min`)$gridSearch(
      lower = 1e-4,
      upper = 1e-3,
      totalEvaluations = 3
    )
  )
  hourGrid <- suppressMessages(
    clearanceTask(ospUnits$`Inversed time`$`1/h`)$gridSearch(
      lower = 1e-4 * 60,
      upper = 1e-3 * 60,
      totalEvaluations = 3
    )
  )

  expect_equal(hourGrid$ofv, baseGrid$ofv, tolerance = 1e-6)
})


# Calculate OFV Profiles

test_that("calculateOFVProfiles() works with multiple parameters", {
  piTask <- ParameterIdentification$new(
    simulations = sim_250mg,
    parameters = list(piParameterLipo_250mg, piParameterCl_250mg),
    outputMappings = outputMapping_250mg
  )
  ofvProfiles <- piTask$calculateOFVProfiles()
  expect_equal(length(ofvProfiles), length(piTask$parameters))
  expect_snapshot(ofvProfiles[[1]][1:10, ])
  expect_snapshot(ofvProfiles[[2]][1:10, ])
})

test_that("calculateOFVProfiles() returns `Inf` on simulation failure", {
  piTask <- ParameterIdentification$new(
    simulations = sim_250mg,
    parameters = list(piParameterLipo_250mg, piParameterCl_250mg),
    outputMappings = outputMapping_250mg
  )

  suppressMessages(suppressWarnings(
    expect_warning(
      ofvProfiles <- piTask$calculateOFVProfiles(
        par = c(0, -0.25),
        totalEvaluations = 3
      )
    )
  ))
  expect_snapshot(ofvProfiles[[2]]$ofv)
})
