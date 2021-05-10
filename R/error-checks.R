#' Do all quantities have the same dimension?
#'
#' @param quantities Quantities which dimensions are compared
#'
#' @return TRUE if the dimension of all quantities are equal, otherwise FALSE
#' @export
isSameDimension <- function(quantities) {
  dims <- lapply(quantities, function(x) {
    x$dimension
  })

  return(length(unique(dims)) == 1)
}

#' Check if the dimensions of the quantities are equal.
#'
#' @param quantities Quantities which dimensions are compared
#' @export
validateIsSameDimension <- function(quantities) {
  if (!isSameDimension(quantities)) {
    stop(messages$errorDimensionsNotEqual())
  }
}
