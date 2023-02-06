test_that("`getSteadyState` runs", {
  simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simFilePath)
  output <- getSteadyState(
    simulations = sim,
    steadyStateTime = 1000
  )
  expect_equal(names(output[[sim$id]]), c("paths", "values"))
})
