# .blqKeepMask

# One data set with times 1 to 4; the values at times 3 and 4 are below the
# LLOQ of 2.5.
blqVectors <- function() {
  list(
    name = rep("d1", 4),
    xValues = c(1, 2, 3, 4),
    yValues = c(10, 5, 1, 0.5),
    lloq = rep(2.5, 4)
  )
}

keepMask <- function(v, blqRemove) {
  .blqKeepMask(v$name, v$xValues, v$yValues, v$lloq, blqRemove)
}

test_that("none keeps every observation even when some are BLQ", {
  expect_identical(keepMask(blqVectors(), "none"), rep(TRUE, 4))
})

test_that("always drops exactly the BLQ observations", {
  expect_identical(
    keepMask(blqVectors(), "always"),
    c(TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("always keeps everything when no observation is BLQ", {
  v <- blqVectors()
  v$lloq[] <- 0.1
  expect_identical(keepMask(v, "always"), rep(TRUE, 4))
})

test_that("always on an all-BLQ data set drops every observation", {
  v <- blqVectors()
  v$lloq[] <- 100
  expect_identical(keepMask(v, "always"), rep(FALSE, 4))
})

test_that("trailingSingle keeps the first of a trailing BLQ run and drops the rest", {
  expect_identical(
    keepMask(blqVectors(), "trailingSingle"),
    c(TRUE, TRUE, TRUE, FALSE)
  )
})

test_that("trailingSingle keeps interior BLQ observations", {
  v <- blqVectors()
  v$yValues <- c(10, 1, 5, 0.5)
  expect_identical(keepMask(v, "trailingSingle"), rep(TRUE, 4))
})

test_that("trailingSingle on an all-BLQ data set keeps only its earliest observation", {
  v <- blqVectors()
  v$lloq[] <- 100
  expect_identical(
    keepMask(v, "trailingSingle"),
    c(TRUE, FALSE, FALSE, FALSE)
  )
})

test_that("trailingSingle finds the trailing run of each data set on its own", {
  v <- list(
    name = c("d1", "d1", "d1", "d2", "d2", "d2"),
    xValues = c(1, 2, 3, 1, 2, 3),
    yValues = c(10, 1, 0.5, 0.5, 1, 5),
    lloq = rep(2.5, 6)
  )
  # d1 ends with the run at times 2 and 3, d2 ends above the LLOQ
  expect_identical(
    keepMask(v, "trailingSingle"),
    c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE)
  )
})

test_that("trailingSingle finds the run by time and returns the mask in input order", {
  v <- lapply(blqVectors(), `[`, c(3, 1, 4, 2))
  expect_identical(
    keepMask(v, "trailingSingle"),
    c(TRUE, TRUE, FALSE, TRUE)
  )
})

test_that("observations with an NA LLOQ or an NA value are never BLQ", {
  v <- blqVectors()
  v$lloq[] <- NA_real_
  expect_identical(keepMask(v, "always"), rep(TRUE, 4))
  expect_identical(keepMask(v, "trailingSingle"), rep(TRUE, 4))

  # An NA value at time 3 ends no run and is not removed; time 4 is a single
  # trailing BLQ observation
  v <- blqVectors()
  v$yValues[3] <- NA_real_
  expect_identical(keepMask(v, "always"), c(TRUE, TRUE, TRUE, FALSE))
  expect_identical(keepMask(v, "trailingSingle"), rep(TRUE, 4))
})

test_that("trailingSingle groups observations with an NA data set name", {
  v <- list(
    name = c("d1", "d1", NA_character_, NA_character_),
    xValues = c(1, 2, 1, 2),
    yValues = c(10, 5, 0.5, 0.5),
    lloq = rep(2.5, 4)
  )
  expect_identical(
    keepMask(v, "trailingSingle"),
    c(TRUE, TRUE, TRUE, FALSE)
  )
})

test_that("an empty input gives an empty mask", {
  v <- lapply(blqVectors(), `[`, 0)
  for (blqRemove in c("none", "always", "trailingSingle")) {
    expect_identical(keepMask(v, blqRemove), logical(0))
  }
})

test_that("an unknown blqRemove stops at the default arm", {
  expect_snapshot(keepMask(blqVectors(), "sometimes"), error = TRUE)
})

# .applyBlqSubstitution

test_that("none returns the observed values unchanged even with below-LLOQ values", {
  obs <- c(10, 0.3)
  res <- .applyBlqSubstitution(obs, lloq = c(0.5, 0.5), "none", "lin")
  expect_equal(res, obs)
})

test_that("lloq (lin) substitutes a BLQ observation with the per-point LLOQ", {
  obs <- c(10, 0.3)
  res <- .applyBlqSubstitution(obs, lloq = c(0.5, 0.5), "lloq", "lin")
  expect_equal(res, c(10, 0.5))
})

test_that("lloq (lin) never modifies a quantifiable observation", {
  # At an LLOQ of 5 the second value would be BLQ and substituted back to 5,
  # so the LLOQ is 4 to keep the test from passing by coincidence
  obs <- c(10, 5)
  res <- .applyBlqSubstitution(obs, lloq = c(0.5, 4), "lloq", "lin")
  expect_equal(res, obs)
})

test_that("lloqHalf (lin) substitutes a BLQ observation with half the per-point LLOQ", {
  obs <- c(10, 0.3)
  res <- .applyBlqSubstitution(obs, lloq = c(0.5, 0.5), "lloqHalf", "lin")
  expect_equal(res, c(10, 0.25))
})

test_that("lloqHalf (log) substitutes with ln(LLOQ/2) = lloq - log(2)", {
  lnLloq <- log(0.5)
  obs <- c(log(10), log(0.2))
  res <- .applyBlqSubstitution(obs, lloq = c(lnLloq, lnLloq), "lloqHalf", "log")
  expect_equal(res, c(log(10), log(0.25)))
})

test_that("each element uses its own per-point LLOQ", {
  obs <- c(0.3, 0.3)
  res <- .applyBlqSubstitution(obs, lloq = c(0.5, 0.2), "lloq", "lin")
  expect_equal(res, c(0.5, 0.3))
})

test_that("NA lloq is never substituted", {
  obs <- c(0.3, 0.3)
  res <- .applyBlqSubstitution(obs, lloq = c(NA_real_, 0.5), "lloq", "lin")
  expect_equal(res, c(0.3, 0.5))
})

test_that("NA observed value is never substituted", {
  obs <- c(NA_real_, 0.3)
  res <- .applyBlqSubstitution(obs, lloq = c(0.5, 0.5), "lloq", "lin")
  expect_equal(res, c(NA_real_, 0.5))
})

test_that("a value exactly at the LLOQ is substituted", {
  res <- .applyBlqSubstitution(0.5, lloq = 0.5, "lloqHalf", "lin")
  expect_equal(res, 0.25)
})

test_that("removal and substitution agree on a value exactly at the LLOQ", {
  expect_false(.blqKeepMask("d1", 1, 2.5, 2.5, "always"))
  expect_equal(.applyBlqSubstitution(2.5, 2.5, "lloqHalf", "lin"), 1.25)
})

test_that("m3 returns the observed values unchanged (passthrough)", {
  obs <- c(10, 0.3)
  res <- .applyBlqSubstitution(obs, lloq = c(0.5, 0.5), "m3", "lin")
  expect_equal(res, obs)
})

test_that("an unrecognized method errors at the default switch arm", {
  expect_snapshot(
    .applyBlqSubstitution(1, lloq = 0.5, "sometimes", "lin"),
    error = TRUE
  )
})

test_that("a length-mismatched lloq errors instead of silently corrupting observed values", {
  expect_snapshot(
    .applyBlqSubstitution(c(0.3, 0.2), lloq = 0.5, "lloqHalf", "lin"),
    error = TRUE
  )
})
