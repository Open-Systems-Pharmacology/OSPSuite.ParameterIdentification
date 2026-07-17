# BLQ ROW SELECTION
#
# Applies the `blqRemove` axis: selects which observed below-limit-of-
# quantification (BLQ) rows enter the objective function. Pure function of the
# observed data, applied once when the observed-data cache is built.

#' Identify BLQ observed rows
#'
#' @param df Observed-rows data frame with `yValues` and `lloq` columns.
#' @return Logical vector, `TRUE` where the row is below the limit of
#'   quantification. Never `NA`: rows with `NA` `lloq` or `NA` `yValues` are
#'   treated as not-BLQ.
#' @keywords internal
#' @noRd
.isBlq <- function(df) {
  !is.na(df$lloq) & !is.na(df$yValues) & df$yValues <= df$lloq
}

#' Apply the blqRemove filter to observed rows
#'
#' Selects which observed BLQ rows enter the objective function. A pure function
#' of the observed data, so it is applied once at cache-build time.
#'
#' @param observedDf Observed-rows data frame. Carries `name`, `xValues`,
#'   `yValues`, and `lloq` columns.
#' @param blqRemove A `BLQRemoveModes` value: `"none"`, `"always"`, or
#'   `"trailingSingle"`.
#' @return The filtered observed-rows data frame.
#' @keywords internal
#' @noRd
.applyBlqRemove <- function(observedDf, blqRemove) {
  switch(
    blqRemove,
    none = observedDf,
    always = observedDf[!.isBlq(observedDf), , drop = FALSE],
    trailingSingle = .removeTrailingBlq(observedDf),
    ospsuite.utils::validateEnumValue(blqRemove, BLQRemoveModes)
  )
}

#' Remove trailing BLQ runs, keeping the first point of each run per dataset
#'
#' @param observedDf Observed-rows data frame with a `name` column identifying
#'   datasets.
#' @return The data frame with each dataset's maximal contiguous trailing BLQ
#'   run reduced to its first point. Rows are regrouped by `name` and ordered by
#'   `xValues`, so their order can differ from the input (unlike the `none` and
#'   `always` modes, which preserve input order). This is safe because the
#'   downstream cost pairs observed to simulated by `name` and `xValues`, not by
#'   row position.
#' @keywords internal
#' @noRd
.removeTrailingBlq <- function(observedDf) {
  if (nrow(observedDf) == 0L) {
    return(observedDf)
  }
  # addNA keeps rows with an NA name in their own group; plain split() would
  # silently drop them, diverging from the none/always modes.
  groups <- split(observedDf, addNA(observedDf$name, ifany = TRUE))
  keptGroups <- vector("list", length(groups))
  for (g in seq_along(groups)) {
    grp <- groups[[g]]
    grp <- grp[order(grp$xValues), , drop = FALSE]
    isBlq <- .isBlq(grp)
    n <- nrow(grp)
    # Walk backward from the last row to bound the trailing BLQ run.
    # rev(seq_len(n)) yields integer(0) for an empty group (never c(0, 1)).
    runStart <- n + 1L
    for (i in rev(seq_len(n))) {
      if (!isBlq[i]) {
        break
      }
      runStart <- i
    }
    if (runStart <= n) {
      # Keep the first row of the run; drop the remaining run rows.
      dropRows <- seq.int(runStart, n)[-1]
      if (length(dropRows) > 0L) {
        grp <- grp[-dropRows, , drop = FALSE]
      }
    }
    keptGroups[[g]] <- grp
  }
  result <- do.call(rbind, keptGroups)
  rownames(result) <- NULL
  result
}

#' Apply the blqMethod substitution to observed values
#'
#' Substitutes below-LLOQ observed values against the per-point LLOQ. The
#' simulated prediction is never modified: substitution is observed-only, as is
#' standard in the community/Beal taxonomy (agreement between a censored
#' observation and the prediction below the LLOQ is `m3`'s job, not this
#' function's). Quantifiable observations (at or above the LLOQ) are never
#' touched. A pure function of the observed values, called from the kernel
#' after interpolation so a per-point LLOQ is available. `none` and `m3` are
#' passthrough (`m3` is scored by the censored path).
#'
#' @param observedValues Numeric vector of observed values (kernel scale).
#' @param lloq Numeric vector of per-point LLOQ, aligned with `observedValues`.
#'   In log scaling this already holds `ln(LLOQ)`. For `blqMethod` values that
#'   substitute (`"lloq"`, `"lloqHalf"`), a length mismatch errors rather than
#'   silently recycling and corrupting `observedValues`.
#' @param blqMethod A `BLQMethods` value: `"none"`, `"lloq"`, `"lloqHalf"`, `"m3"`.
#' @param scaling A `ScalingOptions` value: `"lin"` or `"log"`. Governs the
#'   `lloqHalf` target (`LLOQ/2` in lin, `ln(LLOQ) - ln(2)` in log).
#' @return The observed values, with BLQ observations substituted per
#'   `blqMethod`.
#' @keywords internal
#' @noRd
.applyBlqSubstitution <- function(observedValues, lloq, blqMethod, scaling) {
  target <- switch(
    blqMethod,
    none = return(observedValues),
    m3 = return(observedValues),
    lloq = lloq,
    lloqHalf = if (scaling == "log") lloq - log(2) else lloq / 2,
    ospsuite.utils::validateEnumValue(blqMethod, BLQMethods)
  )
  ospsuite.utils::validateIsSameLength(observedValues, lloq)
  obsBelow <- !is.na(lloq) & !is.na(observedValues) & observedValues < lloq
  observedValues[obsBelow] <- target[obsBelow]
  observedValues
}
