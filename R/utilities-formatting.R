#' Format Parameter Values for Display
#'
#' @description Formats numeric values using scientific notation for very small
#' or large values, and fixed-point otherwise.
#'
#' @param par Numeric vector of parameter values.
#' @return Character vector of formatted values.
#'
#' @keywords internal
#' @noRd
.formatValues <- function(par) {
  vapply(par, function(x) {
    if (is.na(x)) {
      "NA"
    } else if (abs(x) < 0.01 || abs(x) > 1e4) {
      formatC(x, format = "e", digits = 2)
    } else {
      formatC(x, format = "fg", digits = 4, flag = "#")
    }
  }, character(1))
}
