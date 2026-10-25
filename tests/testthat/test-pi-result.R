test_that("PIResult can be initialized with optimization results only", {
  optimResult <- list(
    par = -0.097,
    value = 778.129,
    startValues = -0.097,
    convergence = TRUE,
    iterations = 3,
    fnEvaluations = 3,
    algorithm = "BOBYQA",
    elapsed = 1.51
  )

  expect_no_error(piResult <- PIResult$new(optimResult))
  expect_s3_class(piResult, "R6")
})

test_that("toDataFrame() errors clearly when PIResult has no parameter metadata", {
  optimResult <- list(
    par = -0.097,
    value = 778.129,
    startValues = -0.097,
    convergence = TRUE,
    iterations = 3,
    fnEvaluations = 3,
    algorithm = "BOBYQA",
    elapsed = 1.51
  )
  piResult <- PIResult$new(optimResult)
  expect_error(piResult$toDataFrame(), "Parameter metadata is not available")
})

piTask <- testPiTask()
piTask$configuration$algorithmOptions <- list(maxeval = 10)
suppressMessages(
  piResult <- piTask$run()
)
piResult$.__enclos_env__$private$.result$elapsed <- 0
piResult$.__enclos_env__$private$.result$ciElapsed <- 0

test_that("toList() produces expected output", {
  expect_snapshot(piResult$toList())
})

test_that("print() produces expected output", {
  expect_snapshot(piResult$print())
})

test_that("toDataFrame() produces expected output", {
  expect_snapshot(piResult$toDataFrame())
})

test_that("toDataFrame() returns one row per grouped parameter", {
  # piParameterLipo groups two Lipophilicity parameters (from sim_250mg and sim_500mg)
  optimResult <- list(
    par = 1.0,
    value = 100,
    startValues = 1.5,
    convergence = TRUE,
    iterations = 10,
    fnEvaluations = 10,
    algorithm = "BOBYQA",
    elapsed = 1.0
  )

  piResult <- PIResult$new(
    optimResult = optimResult,
    piParameters = list(piParameterLipo)
  )

  df <- piResult$toDataFrame()

  expect_equal(nrow(df), 2)
  expect_equal(df$estimate, c(1.0, 1.0))
  expect_equal(df$group, c("1", "1"))
})

optimResultFor <- function(...) {
  utils::modifyList(
    list(
      par = -0.097,
      value = 778.129,
      startValues = -0.097,
      convergence = TRUE,
      iterations = 3,
      fnEvaluations = 3,
      algorithm = "BOBYQA",
      elapsed = 1.51
    ),
    list(...)
  )
}

test_that("PIResult reports the profile likelihood se and rse as sd and cv", {
  ciResult <- list(
    se = 0.2,
    rse = 0.1,
    lowerCI = 0.9,
    upperCI = 1.7,
    method = "PL",
    elapsed = 0.5,
    details = list()
  )
  result <- PIResult$new(optimResultFor(par = 2), ciResult = ciResult)$toList()
  expect_identical(result$sd, 0.2)
  expect_identical(result$cv, 0.1)
  expect_identical(result$ciMethod, "PL")
})

test_that("PIResult reports one logical convergence", {
  convergenceOf <- function(...) PIResult$new(optimResultFor(...))$toList()$convergence
  expect_identical(convergenceOf(convergence = NULL), FALSE)
  expect_identical(convergenceOf(convergence = NA), FALSE)
  expect_identical(convergenceOf(value = Inf), FALSE)
  expect_identical(convergenceOf(), TRUE)
})

test_that("PIResult keeps a clone of the configuration", {
  configuration <- PIConfiguration$new()
  result <- PIResult$new(optimResultFor(), configuration = configuration)
  configuration$ciMethod <- "PL"
  expect_identical(
    result$.__enclos_env__$private$.configuration$ciMethod,
    "hessian"
  )
})

test_that("PIResult carries the PK targets only when given", {
  expect_false("pkMappings" %in% names(PIResult$new(optimResultFor())$toList()))
  simulation <- testSimulation()
  mapping <- PKOutputMapping$new(
    quantity = testQuantity(simulation),
    pkParameter = "C_max",
    targetValue = 30,
    targetUnit = testQuantity(simulation)$unit
  )
  result <- PIResult$new(
    optimResultFor(),
    pkMappings = list(mapping),
    achievedPKValues = list(28.5)
  )$toList()
  expect_identical(result$pkMappings[[1]]$targetValue, 30)
  expect_identical(result$achievedPKValues, list(28.5))
})
