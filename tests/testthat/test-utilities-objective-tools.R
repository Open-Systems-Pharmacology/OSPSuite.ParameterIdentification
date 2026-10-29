# A cheap objective with its minimum at (1, 2)
quadratic <- function(values) sum((values - c(1, 2))^2)

test_that(".ofvGrid() evaluates every point of the grid", {
  grid <- .ofvGrid(
    quadratic,
    lower = c(0, 0),
    upper = c(2, 4),
    logScaleFlag = c(FALSE, FALSE),
    totalEvaluations = 9,
    parameterNames = c("a", "b")
  )
  expect_s3_class(grid, "tbl_df")
  expect_identical(names(grid), c("a", "b", "ofv"))
  expect_identical(grid$a, rep(c(0, 1, 2), 3))
  expect_identical(grid$b, rep(c(0, 2, 4), each = 3))
  expect_equal(grid$ofv, (grid$a - 1)^2 + (grid$b - 2)^2)
})

test_that(".ofvGrid() spaces a log-scaled parameter geometrically", {
  grid <- .ofvGrid(
    quadratic,
    lower = c(1, 1),
    upper = c(100, 1),
    logScaleFlag = c(TRUE, FALSE),
    totalEvaluations = 9,
    parameterNames = c("a", "b")
  )
  expect_equal(unique(grid$a), c(1, 10, 100))
  expect_snapshot(
    .ofvGrid(
      quadratic,
      lower = c(0, 1),
      upper = c(100, 1),
      logScaleFlag = c(TRUE, FALSE),
      totalEvaluations = 9,
      parameterNames = c("a", "b")
    ),
    error = TRUE
  )
})

test_that(".ofvProfiles() varies one parameter at a time around par", {
  profiles <- .ofvProfiles(
    quadratic,
    par = c(1, -2),
    boundFactor = 0.5,
    totalEvaluations = 3,
    parameterNames = c("a", "b")
  )
  expect_identical(names(profiles), c("a", "b"))
  expect_identical(names(profiles$a), c("a", "ofv"))
  expect_equal(profiles$a$a, c(0.5, 1, 1.5))
  expect_equal(profiles$b$b, c(-3, -2, -1))
  expect_equal(profiles$a$ofv, (c(0.5, 1, 1.5) - 1)^2 + 16)
  expect_equal(profiles$b$ofv, (c(-3, -2, -1) - 2)^2)
})
