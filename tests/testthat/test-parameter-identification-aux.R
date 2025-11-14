# ParameterIdentification - auxiliary exploration

# Plot Results

test_that("plotResults() generates expected plot before parameter estimation", {
  piTask <- createPiTask()
  vdiffr::expect_doppelganger("before_estimation", piTask$plotResults()[[1]])
})

piTask <- createPiTask()
suppressMessages(piResults <- piTask$run())
test_that("plotResults() generates expected plot after parameter estimation", {
  vdiffr::expect_doppelganger("after_estimation", piTask$plotResults()[[1]])
})

test_that("plotResults() generates expected plot with parameter input", {
  vdiffr::expect_doppelganger("custom_parameter", piTask$plotResults(1.2)[[1]])
})


# Grid Search

test_that("gridSearch() works without error for single parameter", {
  piTask <- createPiTask()
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
