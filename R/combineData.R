#' A wrapper function around {ospsuite} DataCombined class
#' @return an object of class {ospsuite}::DataCombined
#'
#' @param
#'
#' @examples
#'
combineData <- function() {
  res <- ospsuite::DataCombined$new()
  res$addDataSets()
}
