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
#'   run reduced to its first point.
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
