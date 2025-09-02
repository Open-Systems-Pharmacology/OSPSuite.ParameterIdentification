#' Convert units to a data type subset.
#'
#' Convert `data` so all rows use the single unit pair (`xUnit`, `yUnit`)
#' found in rows where the `dataType` column equals the given `dataType`
#' argument. If no unique pair exists, call the converter without explicit
#' targets (no-op if already aligned).
#'
#' @param data A data frame (or a tibble) from `DataCombined$toDataFrame()`.
#' @param dataType Character scalar, either `"simulated"` or `"observed"`.
#'
#' @return List with elements: `data` (converted data.frame) and `units`
#' (list with `xUnit` and `yUnit` when a unique pair is found, otherwise `NULL`)
#'
#' @keywords internal
#' @noRd
.convertUnitsToSource <- function(data, dataType = c("simulated", "observed")) {
  dataType <- match.arg(dataType)

  subsetData <- data[data$dataType == dataType, , drop = FALSE]
  uxAll <- unique(stats::na.omit(subsetData$xUnit))
  uyAll <- unique(stats::na.omit(subsetData$yUnit))

  if (length(uxAll) == 1L && length(uyAll) == 1L) {
    xTarget <- uxAll[[1]]
    yTarget <- uyAll[[1]]
    list(
      data  = ospsuite:::.unitConverter(data, xUnit = xTarget, yUnit = yTarget),
      units = list(xUnit = xTarget, yUnit = yTarget)
    )
  } else {
    # Fallback: generic conversion (no explicit targets)
    list(
      data  = ospsuite:::.unitConverter(data),
      units = NULL
    )
  }
}
