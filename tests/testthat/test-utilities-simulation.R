test_that("Multiple simulations, single steadyStateTime", {
  simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim1 <- loadSimulation(simFilePath)
  sim2 <- loadSimulation(simFilePath)
  output <- getSteadyState(
    simulations = c(sim1, sim2),
    steadyStateTime = 1
  )
  expect_equal(names(output), c(sim1$id, sim2$id))
})

test_that("Multiple simulations, multiple steadyStateTime", {
  simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim1 <- loadSimulation(simFilePath)
  sim2 <- loadSimulation(simFilePath)
  output <- getSteadyState(
    simulations = c(sim1, sim2),
    steadyStateTime = c(1, 2)
  )
  expect_equal(names(output), c(sim1$id, sim2$id))

  expect_equal(names(output[[sim1$id]]), c("paths", "values"))
})
