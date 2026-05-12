# ParameterIdentification with pkOutputMappings

testPKMapping <- function(simulation = testSimulation()) {
  PKOutputMapping$new(
    quantity = testQuantity(simulation),
    pkParameter = "C_max",
    targetValue = 30,
    targetUnit = testQuantity(simulation)$unit
  )
}

testPKTask <- function(simulation = testSimulation(), iter = 5) {
  ParameterIdentification$new(
    simulations = simulation,
    parameters = testPKParameters(simulation),
    pkOutputMappings = testPKMapping(simulation),
    configuration = lowIterPiConfiguration(iter)
  )
}

test_that("ParameterIdentification accepts pkOutputMappings", {
  pi <- testPKTask()
  expect_s3_class(pi, "ParameterIdentification")
})

test_that("ParameterIdentification errors when both outputMappings and pkOutputMappings provided", {
  sim <- testSimulation()
  expect_error(
    ParameterIdentification$new(
      simulations = sim,
      parameters = testParameters(sim),
      outputMappings = testOutputMapping(sim),
      pkOutputMappings = testPKMapping(sim)
    ),
    regexp = messages$errorPIMixedMappings(),
    fixed = TRUE
  )
})

test_that("ParameterIdentification errors when neither mapping type provided", {
  sim <- testSimulation()
  expect_error(
    ParameterIdentification$new(
      simulations = sim,
      parameters = testParameters(sim)
    ),
    regexp = messages$errorPINoMappings(),
    fixed = TRUE
  )
})

test_that("ParameterIdentification$run() with pkOutputMappings returns PKResult", {
  result <- testPKTask()$run()
  expect_s3_class(result, "PKResult")
})

test_that("PKResult from pkOutputMappings has convergence field", {
  result <- testPKTask()$run()
  resultList <- result$toList()
  expect_equal(resultList$convergence, TRUE)
})

test_that("PKResult$toDataFrame() returns mapping-centric frame for pkOutputMappings", {
  result <- testPKTask()$run()
  df <- result$toDataFrame()
  expect_s3_class(df, "data.frame")
  expect_equal(
    names(df),
    c(
      "quantityPath",
      "pkParameter",
      "targetValue",
      "targetUnit",
      "achievedValue",
      "estimatedValue",
      "parameterUnit"
    )
  )
  expect_equal(nrow(df), 1L)
  expect_equal(df$pkParameter, "C_max")
  expect_equal(df$targetValue, 30)
  expect_true(is.finite(df$achievedValue))
  expect_true(is.finite(df$estimatedValue))
})

test_that("ParameterIdentification$plotResults() errors for pkOutputMappings", {
  pi <- testPKTask()
  pi$run()
  expect_error(
    pi$plotResults(),
    regexp = messages$errorMethodNotApplicableInPKMode("plotResults"),
    fixed = TRUE
  )
})

test_that("estimateCI errors for pkOutputMappings", {
  sim <- testSimulation()
  config <- bootstrapPiConfiguration()
  pi <- ParameterIdentification$new(
    simulations = sim,
    parameters = testPKParameters(sim),
    pkOutputMappings = testPKMapping(sim),
    configuration = config
  )
  pi$run()
  expect_error(
    pi$estimateCI(),
    regexp = messages$errorMethodNotApplicableInPKMode("estimateCI"),
    fixed = TRUE
  )
})

test_that("gridSearch errors for pkOutputMappings", {
  pi <- testPKTask()
  expect_error(
    pi$gridSearch(),
    regexp = messages$errorMethodNotApplicableInPKMode("gridSearch"),
    fixed = TRUE
  )
})

test_that("calculateOFVProfiles errors for pkOutputMappings", {
  pi <- testPKTask()
  pi$run()
  expect_error(
    pi$calculateOFVProfiles(),
    regexp = messages$errorMethodNotApplicableInPKMode("calculateOFVProfiles"),
    fixed = TRUE
  )
})

test_that(".getPKValues uses each mapping's own simulation batch, not always the first", {
  pkmlPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim1 <- ospsuite::loadSimulation(
    pkmlPath,
    loadFromCache = FALSE,
    addToCache = FALSE
  )
  sim2 <- ospsuite::loadSimulation(
    pkmlPath,
    loadFromCache = FALSE,
    addToCache = FALSE
  )

  # Lower clearance in sim2 so its C_max is detectably higher than sim1's
  # at the same dose — proves each mapping was evaluated against its own sim.
  cl2 <- ospsuite::getParameter(
    "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS-Aciclovir|TSspec",
    container = sim2
  )
  ospsuite::setParameterValues(cl2, cl2$value * 0.1)

  doseParam1 <- ospsuite::getParameter(
    "Events|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
    container = sim1
  )
  doseParam2 <- ospsuite::getParameter(
    "Events|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
    container = sim2
  )
  piParam <- PIParameters$new(parameters = list(doseParam1, doseParam2))
  piParam$minValue <- 0.0001
  piParam$maxValue <- 0.001

  outputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  q1 <- ospsuite::getQuantity(outputPath, container = sim1)
  q2 <- ospsuite::getQuantity(outputPath, container = sim2)

  mapping1 <- PKOutputMapping$new(
    quantity = q1,
    pkParameter = "C_max",
    targetValue = 30,
    targetUnit = q1$unit
  )
  mapping2 <- PKOutputMapping$new(
    quantity = q2,
    pkParameter = "C_max",
    targetValue = 30,
    targetUnit = q2$unit
  )

  pi <- ParameterIdentification$new(
    simulations = list(sim1, sim2),
    parameters = piParam,
    pkOutputMappings = list(mapping1, mapping2),
    configuration = lowIterPiConfiguration(iter = 2)
  )
  result <- pi$run()
  df <- result$toDataFrame()

  # Both rows present
  expect_equal(nrow(df), 2L)

  # With the bug, both achieved values come from sim1's pkAnalysis (identical).
  # With the fix, sim2's lower clearance produces a higher C_max than sim1's.
  expect_false(
    isTRUE(all.equal(df$achievedValue[[1]], df$achievedValue[[2]])),
    info = "sim2 has 10x lower clearance so its C_max must differ from sim1's"
  )
})
