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
