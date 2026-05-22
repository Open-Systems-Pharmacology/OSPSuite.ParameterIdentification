# PKOutputMapping

test_that("PKOutputMapping constructs with scalar target and converts to base unit", {
  sim <- testSimulation()
  quantity <- testQuantity(sim)
  mapping <- PKOutputMapping$new(
    quantity = quantity,
    pkParameter = "C_max",
    targetValue = 0.5,
    targetUnit = "µmol/l"
  )
  expect_s3_class(mapping, "PKOutputMapping")
  expect_equal(mapping$pkParameter, "C_max")
  expect_equal(mapping$targetValue, 0.5)
  expect_equal(mapping$targetUnit, "µmol/l")
  expectedBase <- ospsuite::toBaseUnit(
    quantityOrDimension = ospsuite::pkParameterByName("C_max")$dimension,
    values = 0.5,
    unit = "µmol/l"
  )
  expect_equal(mapping$targetValueInBaseUnit, expectedBase)
  expect_equal(mapping$simId, sim$root$id)
})

test_that("PKOutputMapping errors on pkParameter not in StandardPKParameter", {
  expect_error(
    PKOutputMapping$new(
      quantity = testQuantity(),
      pkParameter = "not_a_real_param",
      targetValue = 0.5,
      targetUnit = "µmol/l"
    ),
    regexp = "not_a_real_param"
  )
})

test_that("PKOutputMapping errors on NA targetValue", {
  expect_error(
    PKOutputMapping$new(
      quantity = testQuantity(),
      pkParameter = "C_max",
      targetValue = NA_real_,
      targetUnit = "µmol/l"
    ),
    regexp = messages$errorNAValue("targetValue"),
    fixed = TRUE
  )
})

test_that("PKOutputMapping errors on non-positive targetValue", {
  expect_error(
    PKOutputMapping$new(
      quantity = testQuantity(),
      pkParameter = "C_max",
      targetValue = -1,
      targetUnit = "µmol/l"
    ),
    regexp = messages$errorNonPositiveValue("targetValue"),
    fixed = TRUE
  )
})

test_that("PKOutputMapping quantity and simId are read-only", {
  mapping <- PKOutputMapping$new(
    quantity = testQuantity(),
    pkParameter = "C_max",
    targetValue = 0.5,
    targetUnit = "µmol/l"
  )
  expect_error(
    mapping$quantity <- testQuantity(),
    regexp = messages$errorPropertyReadOnly("quantity"),
    fixed = TRUE
  )
  expect_error(
    mapping$simId <- "other",
    regexp = messages$errorPropertyReadOnly("simId"),
    fixed = TRUE
  )
})

test_that("PKOutputMapping pkParameter setter updates field and recomputes base unit", {
  aucBaseUnit <- ospsuite::getBaseUnit(
    ospsuite::pkParameterByName("AUC_tEnd")$dimension
  )
  mapping <- PKOutputMapping$new(
    quantity = testQuantity(),
    pkParameter = "AUC_tEnd",
    targetValue = 100,
    targetUnit = aucBaseUnit
  )
  mapping$pkParameter <- "AUC_inf"
  expect_equal(mapping$pkParameter, "AUC_inf")
  expected <- ospsuite::toBaseUnit(
    quantityOrDimension = ospsuite::pkParameterByName("AUC_inf")$dimension,
    values = 100,
    unit = aucBaseUnit
  )
  expect_equal(mapping$targetValueInBaseUnit, expected)
})


# targetValue and targetUnit setters

test_that("PKOutputMapping targetValue setter updates value and recomputes base unit", {
  mapping <- PKOutputMapping$new(
    quantity = testQuantity(),
    pkParameter = "C_max",
    targetValue = 0.5,
    targetUnit = "µmol/l"
  )
  mapping$targetValue <- 1.0
  expect_equal(mapping$targetValue, 1.0)
  expected <- ospsuite::toBaseUnit(
    quantityOrDimension = ospsuite::pkParameterByName("C_max")$dimension,
    values = 1.0,
    unit = "µmol/l"
  )
  expect_equal(mapping$targetValueInBaseUnit, expected)
})

test_that("PKOutputMapping targetUnit setter updates unit and recomputes base unit", {
  mapping <- PKOutputMapping$new(
    quantity = testQuantity(),
    pkParameter = "C_max",
    targetValue = 0.5,
    targetUnit = "µmol/l"
  )
  mapping$targetUnit <- "nmol/l"
  expect_equal(mapping$targetUnit, "nmol/l")
  expected <- ospsuite::toBaseUnit(
    quantityOrDimension = ospsuite::pkParameterByName("C_max")$dimension,
    values = 0.5,
    unit = "nmol/l"
  )
  expect_equal(mapping$targetValueInBaseUnit, expected)
})

test_that("PKOutputMapping targetUnit then targetValue setters chain correctly", {
  mapping <- PKOutputMapping$new(
    quantity = testQuantity(),
    pkParameter = "C_max",
    targetValue = 0.5,
    targetUnit = "µmol/l"
  )
  mapping$targetUnit <- "nmol/l"
  mapping$targetValue <- 1000.0
  expected <- ospsuite::toBaseUnit(
    quantityOrDimension = ospsuite::pkParameterByName("C_max")$dimension,
    values = 1000.0,
    unit = "nmol/l"
  )
  expect_equal(mapping$targetValueInBaseUnit, expected)
})

test_that("PKOutputMapping targetValue setter errors on zero value", {
  mapping <- PKOutputMapping$new(
    quantity = testQuantity(),
    pkParameter = "C_max",
    targetValue = 0.5,
    targetUnit = "µmol/l"
  )
  expect_error(
    mapping$targetValue <- 0,
    regexp = messages$errorNonPositiveValue("targetValue"),
    fixed = TRUE
  )
})

test_that("PKOutputMapping targetValue setter errors on NA value", {
  mapping <- PKOutputMapping$new(
    quantity = testQuantity(),
    pkParameter = "C_max",
    targetValue = 0.5,
    targetUnit = "µmol/l"
  )
  expect_error(
    mapping$targetValue <- NA_real_,
    regexp = messages$errorNAValue("targetValue"),
    fixed = TRUE
  )
})

test_that("PKOutputMapping targetValue setter rejects vectors", {
  mapping <- PKOutputMapping$new(
    quantity = testQuantity(),
    pkParameter = "C_max",
    targetValue = 0.5,
    targetUnit = "µmol/l"
  )
  expect_error(mapping$targetValue <- c(0.5, 1.0))
})

test_that("PKOutputMapping errors with errorPKMappingUnitConversion when targetUnit incompatible", {
  expect_error(
    PKOutputMapping$new(
      quantity = testQuantity(),
      pkParameter = "C_max",
      targetValue = 0.5,
      targetUnit = "min"
    ),
    regexp = "Incompatible.*targetUnit.*min",
    fixed = FALSE
  )
})

test_that("PKOutputMapping targetUnit setter rolls back to original value when unit is incompatible", {
  mapping <- PKOutputMapping$new(
    quantity = testQuantity(),
    pkParameter = "C_max",
    targetValue = 0.5,
    targetUnit = "µmol/l"
  )
  expect_error(mapping$targetUnit <- "min")
  expect_equal(mapping$targetUnit, "µmol/l")
})

test_that("PKOutputMapping pkParameter setter rolls back to original value when unit is incompatible", {
  mapping <- PKOutputMapping$new(
    quantity = testQuantity(),
    pkParameter = "C_max",
    targetValue = 0.5,
    targetUnit = "µmol/l"
  )
  expect_error(mapping$pkParameter <- "t_max")
  expect_equal(mapping$pkParameter, "C_max")
})

test_that("PKOutputMapping$print() does not error", {
  mapping <- PKOutputMapping$new(
    quantity = testQuantity(),
    pkParameter = "C_max",
    targetValue = 0.5,
    targetUnit = "µmol/l"
  )
  expect_no_error(mapping$print())
})
