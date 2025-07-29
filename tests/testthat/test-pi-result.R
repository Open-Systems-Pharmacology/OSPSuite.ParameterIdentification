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

piTask <- createPiTask()
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
