# .toBaseValue

clearancePiParameter <- function() {
  clPath <- "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS-Aciclovir|TSspec"
  sim <- loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite"),
    loadFromCache = FALSE,
    addToCache = FALSE
  )
  PIParameters$new(
    parameters = list(getParameter(clPath, container = sim))
  )
}

test_that(".toBaseValue converts a value from a non-base unit to the base unit", {
  piParameter <- clearancePiParameter()
  piParameter$unit <- ospUnits$`Inversed time`$`1/h`

  expect_equal(.toBaseValue(piParameter, 60), 60 / 60)
})

test_that(".toBaseValue passes a value through unchanged when unit is already base", {
  piParameter <- clearancePiParameter()
  expect_equal(piParameter$unit, ospUnits$`Inversed time`$`1/min`)

  expect_equal(.toBaseValue(piParameter, 60), 60)
})
