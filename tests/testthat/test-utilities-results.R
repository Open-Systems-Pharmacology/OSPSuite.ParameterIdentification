test_that("print.piResult() prints the expected output structure", {
  piTask <- createPiTask()
  piTask$configuration$algorithmOptions <- list(maxeval = 3)

  result <- piTask$run()

  result$elapsed <- 0
  result$ciElapsed <- 0

  expect_snapshot(print(result))
})
