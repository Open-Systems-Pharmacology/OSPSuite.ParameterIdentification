# .applyBlqRemove

# Rows 3 and 4 (yValues 1, 0.5) are BLQ at lloq 2.5; rows 1 and 2 are above.
blqSingleDataset <- function() {
  data.frame(
    name = "d1",
    xValues = c(1, 2, 3, 4),
    yValues = c(10, 5, 1, 0.5),
    lloq = 2.5,
    stringsAsFactors = FALSE
  )
}

test_that("none returns the input unchanged even when BLQ rows are present", {
  df <- blqSingleDataset()
  expect_equal(.applyBlqRemove(df, "none"), df)
})

test_that("always drops exactly the BLQ rows and keeps the rest", {
  df <- blqSingleDataset()
  result <- .applyBlqRemove(df, "always")
  expect_equal(result$xValues, c(1, 2))
  expect_equal(nrow(result), 2)
})

test_that("always returns the input unchanged when there are no BLQ rows", {
  df <- blqSingleDataset()
  df$lloq <- 0.1
  expect_equal(nrow(.applyBlqRemove(df, "always")), 4)
})

test_that("always on an all-BLQ single dataset returns zero rows", {
  df <- blqSingleDataset()
  df$lloq <- 100
  expect_equal(nrow(.applyBlqRemove(df, "always")), 0)
})

test_that("trailingSingle keeps the first of a trailing BLQ run and drops the rest", {
  df <- blqSingleDataset()
  result <- .applyBlqRemove(df, "trailingSingle")
  # Trailing run is rows 3,4; keep row 3, drop row 4.
  expect_equal(result$xValues, c(1, 2, 3))
})

test_that("trailingSingle keeps interior BLQ rows untouched", {
  # Row 2 is BLQ but followed by an above-LLOQ point, so it is interior.
  # Row 4 is a single trailing BLQ point, kept as the first of its run.
  df <- data.frame(
    name = "d1",
    xValues = c(1, 2, 3, 4),
    yValues = c(10, 1, 5, 0.5),
    lloq = 2.5,
    stringsAsFactors = FALSE
  )
  result <- .applyBlqRemove(df, "trailingSingle")
  expect_equal(result$xValues, c(1, 2, 3, 4))
})

test_that("trailingSingle removes trailing runs independently per dataset", {
  df <- data.frame(
    name = c("d1", "d1", "d1", "d2", "d2", "d2"),
    xValues = c(1, 2, 3, 1, 2, 3),
    yValues = c(10, 1, 0.5, 0.5, 1, 5),
    lloq = 2.5,
    stringsAsFactors = FALSE
  )
  result <- .applyBlqRemove(df, "trailingSingle")
  # d1 trailing run is rows 2,3 -> keep first (x=2), drop x=3.
  # d2 ends above LLOQ (x=3 -> 5) -> no trailing run, all kept.
  expect_equal(result$xValues[result$name == "d1"], c(1, 2))
  expect_equal(result$xValues[result$name == "d2"], c(1, 2, 3))
})

test_that("trailingSingle with no trailing BLQ run returns the input unchanged", {
  df <- blqSingleDataset()
  df$yValues <- c(1, 0.5, 5, 10)
  expect_equal(nrow(.applyBlqRemove(df, "trailingSingle")), 4)
})

test_that("rows with NA lloq are never treated as BLQ under any mode", {
  df <- blqSingleDataset()
  df$lloq <- NA_real_
  expect_equal(nrow(.applyBlqRemove(df, "always")), 4)
  expect_equal(nrow(.applyBlqRemove(df, "trailingSingle")), 4)
})

test_that("rows with NA yValues are never treated as BLQ under any mode", {
  # NA yValue must not become a spurious BLQ row (always) or crash the
  # trailing walk (trailingSingle). Row 3 has an NA measurement.
  df <- blqSingleDataset()
  df$yValues <- c(10, 5, NA_real_, 0.5)
  # always: only the genuine BLQ row (x = 4, y = 0.5) is removed; the NA row
  # (x = 3) is kept, so 3 rows remain.
  kept <- .applyBlqRemove(df, "always")
  expect_equal(kept$xValues, c(1, 2, 3))
  # trailingSingle: the NA row (x = 3) is not BLQ, so it terminates the
  # trailing run; the single trailing BLQ point (x = 4) is kept. No crash.
  expect_equal(nrow(.applyBlqRemove(df, "trailingSingle")), 4)
})

test_that("always on a multi-dataset frame keeps the surviving dataset's rows", {
  # Spec section 5: when 'always' empties one dataset of a mapping but another
  # survives, the survivors carry the fit. d1 is entirely BLQ (removed); d2 is
  # entirely above LLOQ (kept). The whole-frame result is non-empty, which is
  # what makes the objective-function empty-mapping guard pass.
  df <- data.frame(
    name = c("d1", "d1", "d2", "d2"),
    xValues = c(1, 2, 1, 2),
    yValues = c(1, 0.5, 10, 8),
    lloq = 2.5,
    stringsAsFactors = FALSE
  )
  result <- .applyBlqRemove(df, "always")
  expect_equal(unique(result$name), "d2")
  expect_equal(result$xValues, c(1, 2))
})

test_that("trailingSingle keeps rows with an NA name instead of dropping them", {
  # split() silently drops NA-name rows; none and always keep them, so
  # trailingSingle must keep them too (all rows here are above the LLOQ).
  df <- data.frame(
    name = c("d1", "d1", NA_character_),
    xValues = c(1, 2, 1),
    yValues = c(10, 5, 8),
    lloq = 2.5,
    stringsAsFactors = FALSE
  )
  result <- .applyBlqRemove(df, "trailingSingle")
  expect_equal(nrow(result), 3)
  expect_true(any(is.na(result$name)))
})

test_that("unsorted input still identifies the trailing run correctly", {
  df <- blqSingleDataset()[c(3, 1, 4, 2), ]
  result <- .applyBlqRemove(df, "trailingSingle")
  # After internal ordering by xValues, trailing run is x=3,4; keep x=3.
  expect_equal(sort(result$xValues), c(1, 2, 3))
})

test_that("an unrecognized mode errors at the default switch arm", {
  expect_snapshot(
    .applyBlqRemove(blqSingleDataset(), "sometimes"),
    error = TRUE
  )
})
