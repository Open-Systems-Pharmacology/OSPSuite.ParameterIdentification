#' Test observed values against their LLOQ
#'
#' The single definition of "below the limit of quantification" (BLQ) in the
#' package, used for removal, substitution and censoring. A value at the LLOQ
#' counts as BLQ, because aggregated data commonly record a BLQ time point at
#' the LLOQ itself.
#'
#' @param yValues Numeric vector of observed values.
#' @param lloq Numeric vector of the LLOQ of each value, on the same scale.
#' @return Logical vector, never `NA`: a value or an LLOQ that is `NA` is not
#'   BLQ.
#' @keywords internal
#' @noRd
.isBlqValues <- function(yValues, lloq) {
  !is.na(lloq) & !is.na(yValues) & yValues <= lloq
}

#' Observations kept by `blqRemove`
#'
#' @param name,xValues,yValues,lloq Data set name, time, value and LLOQ of
#'   each observation.
#' @param blqRemove A `BLQRemoveModes` value. `trailingSingle` keeps, per data
#'   set, the earliest of the BLQ observations after its last quantified
#'   observation, and removes the others of that run. Missing values are
#'   neither quantified nor removed.
#' @return Logical vector, `TRUE` for an observation that enters the
#'   objective, in the order of the input.
#' @keywords internal
#' @noRd
.blqKeepMask <- function(name, xValues, yValues, lloq, blqRemove) {
  isBlq <- .isBlqValues(yValues, lloq)
  switch(
    blqRemove,
    none = rep(TRUE, length(yValues)),
    always = !isBlq,
    trailingSingle = {
      keep <- rep(TRUE, length(yValues))
      groups <- split(seq_along(yValues), addNA(factor(name), ifany = TRUE))
      for (rows in groups) {
        rows <- rows[order(xValues[rows])]
        quantified <- !isBlq[rows] & !is.na(yValues[rows])
        lastQuantified <- max(c(0L, which(quantified)))
        trailing <- rows[seq_along(rows) > lastQuantified & isBlq[rows]]
        keep[trailing[-1]] <- FALSE
      }
      keep
    },
    ospsuite.utils::validateEnumValue(blqRemove, BLQRemoveModes)
  )
}

#' Substitute BLQ observed values by `blqMethod`
#'
#' Replaces the BLQ observed values with the LLOQ (`lloq`) or half the LLOQ
#' (`lloqHalf`). The simulated values are never changed. `none` and `m3`
#' return the values unchanged, `m3` is scored by the censored contribution.
#'
#' @param observedValues Numeric vector of observed values on the scale of the
#'   cost.
#' @param lloq Numeric vector of the LLOQ of each value on the same scale, the
#'   natural log of the LLOQ under log scaling.
#' @param blqMethod A `BLQMethods` value.
#' @param scaling `"lin"` or `"log"`, for half the LLOQ.
#' @return `observedValues` with the BLQ values substituted.
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
  obsBelow <- .isBlqValues(observedValues, lloq)
  observedValues[obsBelow] <- target[obsBelow]
  observedValues
}
