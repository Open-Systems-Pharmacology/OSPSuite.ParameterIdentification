context("ParameterIdentification")

sim <- loadTestSimulation("Aciclovir")
param <- getParameter("Aciclovir|Permeability", sim)
livVol <- getParameter("Organism|Liver|Volume", sim)
kidvVol <- getParameter("Organism|Kidney|Volume", sim)
brainVol <- getParameter("Organism|Brain|Volume", sim)
params <- list(livVol, kidvVol, brainVol)
refVal <- param$value

test_that("It can create a PIParameters object", {
  piParam <- PIParameters$new(param)
  expect_error(capture.output(print(piParam)), NA)
})
