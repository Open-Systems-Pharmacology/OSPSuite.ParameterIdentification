context("PIConfiguration")

test_that("PIConfiguration object can be created", 
          expect_silent(config <- PIConfiguration$new()))
config <- PIConfiguration$new()

test_that("The created object belongs to PIConfiguration class", {
  expect_true("PIConfiguration" %in% class(config))
})
          

test_that("Initial steady-state flag can be extracted",
          expect_silent(ss_flag <- config$simulateSteadyState))

test_that("Initial steady-state flag is set to FALSE",
          expect_false(config$simulateSteadyState))

test_that("Initial steady-state time can be extracted",
          expect_silent(ss_time <- config$steadyStateTime))

test_that("Initial steady-state time is 1000",
          expect_equal(config$steadyStateTime, 1000))

test_that("Print flag can be extracted",
          expect_silent(flag_print <- config))

test_that("Default print flag is set to FALSE",
          expect_false(config$printIterationFeedback))

test_that("A print call prints all internal parameters",
          expect_output(print(config), "PIConfiguration: \\n   Simulate to steady-state: FALSE \\n   Steady-state time \\[min\\]: 1000 \\n   Print feedback after each iteration: FALSE"))

test_that("Print flag can be changed", {
  expect_false(config$printIterationFeedback) 
  config$printIterationFeedback <- TRUE
  expect_true(config$printIterationFeedback)})

test_that("Steady-state flag can be changed", {
  expect_false(config$simulateSteadyState) 
  config$simulateSteadyState <- TRUE
  expect_true(config$simulateSteadyState)})

test_that("Steady-state time can be changed", {
  expect_equal(config$steadyStateTime, 1000)
  config$steadyStateTime <- 2000
  expect_equal(config$steadyStateTime, 2000)
})
