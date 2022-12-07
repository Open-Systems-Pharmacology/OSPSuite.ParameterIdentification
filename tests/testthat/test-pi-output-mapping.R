context("PIOutputMapping")
simulations <- c(loadSimulation("tests/dev/Models/Simulations/Aciclovir.pkml"))
names(simulations) <- "Aciclovir"

test_that("Output mappings can be safely created with aciclovir simulations", {
  expect_no_error(outputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
                                                              container = simulations$Aciclovir)))
})

outputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
                                                            container = simulations$Aciclovir))

test_that("A newly created output mapping has an empty list for its data transformations", {
  expect_equal(outputMapping$dataTransformations, list())
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
                                                              container = simulations$Aciclovir))
  outputMapping$setDataTransformations()
  expect_equal(outputMapping$dataTransformations$xFactors, 1)
  expect_equal(outputMapping$dataTransformations$yFactors, 1)
  expect_equal(outputMapping$dataTransformations$xOffsets, 0)
  expect_equal(outputMapping$dataTransformations$yOffsets, 0)
})

test_that("X-factors can be set in a dataset-specific manner, with labels", {
  outputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
                                                              container = simulations$Aciclovir))
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
