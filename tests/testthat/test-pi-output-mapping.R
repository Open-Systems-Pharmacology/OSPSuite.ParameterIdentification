context("PIOutputMapping")
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
simulations <- c(loadSimulation(simFilePath))
names(simulations) <- "Aciclovir"

piParameter <- PIParameters$new(parameters = ospsuite::getParameter(path = "Aciclovir|Lipophilicity", container = simulations[[1]]))

test_that("Output mappings can be safely created with aciclovir simulations", {
  expect_no_error(outputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    container = simulations$Aciclovir
  )))
})

outputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  container = simulations$Aciclovir
))

test_that("A newly created output mapping has default data transformations", {
  expect_equal(outputMapping$dataTransformations, list(
    xOffsets = 0,
    yOffsets = 0,
    xFactors = 1,
    yFactors = 1
  ))
})

test_that("X-offsets can be set up across different datasets", {
  outputMapping$setDataTransformations(xOffsets = -5)
  expect_equal(outputMapping$dataTransformations$xOffsets, -5)
})

test_that("X-factors can be set up across different datasets", {
  outputMapping$setDataTransformations(xFactors = 2)
  expect_equal(outputMapping$dataTransformations$xFactors, 2)
})

test_that("X-offsets and X-factors can be set up simultaneously", {
  outputMapping$setDataTransformations(xFactors = 2, xOffsets = 5)
  expect_equal(outputMapping$dataTransformations$xFactors, 2)
  expect_equal(outputMapping$dataTransformations$xOffsets, 5)
})

test_that("If not set, the offsets and factors observe their default values", {
  outputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    container = simulations$Aciclovir
  ))
  outputMapping$setDataTransformations()
  expect_equal(outputMapping$dataTransformations$xFactors, 1)
  expect_equal(outputMapping$dataTransformations$yFactors, 1)
  expect_equal(outputMapping$dataTransformations$xOffsets, 0)
  expect_equal(outputMapping$dataTransformations$yOffsets, 0)
})

test_that("X-factors can be set in a dataset-specific manner, with labels", {
  outputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    container = simulations$Aciclovir
  ))
  outputMapping$setDataTransformations(labels = c("data1", "data2"), xFactors = c(2, 3))
  expect_equal(outputMapping$dataTransformations$xFactors[["data1"]], 2)
  expect_equal(outputMapping$dataTransformations$xFactors[["data2"]], 3)
  expect_equal(outputMapping$dataTransformations$yFactors[["data1"]], 1)
  expect_equal(outputMapping$dataTransformations$yFactors[["data2"]], 1)
  expect_equal(outputMapping$dataTransformations$xOffsets[["data1"]], 0)
  expect_equal(outputMapping$dataTransformations$xOffsets[["data2"]], 0)
  expect_equal(outputMapping$dataTransformations$yOffsets[["data1"]], 0)
  expect_equal(outputMapping$dataTransformations$yOffsets[["data2"]], 0)
})

test_that("Adding data sets in Concentration (mass) without MW to an entity with Concentration (molar)", {
  ds <- DataSet$new(name = "Aciclovir observed")
  ds$setValues(c(0, 1, 2), c(0, 1, 2))

  outputMapping$addObservedDataSets(ds)
  # 2DO: Failing now - test to be extended when not failing to check for something plausible
})

test_that("Data transformations can be set for simulated results", {
  outputMapping$setDataTransformations(labels = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
                                       xOffsets = 0,
                                       xFactors = 0.5)

  # 2DO Failing because the test above is failing too
  pi <- ParameterIdentification$new(simulations = simulations, parameters = piParameter,
                                    outputMappings = outputMapping)
  #2DO - add meaningful check that transformations are applied
})
