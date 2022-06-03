#' Get the simulation container of the entity
#'
#' @param entity Object of type \code{Entity}
#'
#' @return The root container that is the parent of the entity.
getSimulationContainer <- function(entity) {
  ospsuite.utils::validateIsOfType(entity, "Entity")
  if (ospsuite.utils::isOfType(entity, "Container")) {
    if (entity$containerType == "Simulation") {
      return(entity)
    }
  }
  return(getSimulationContainer(entity$parentContainer))
}

#' Convenience function to avoid testing for null. It returns the first object if it is not null otherwise the second object
#' @name OR
#'
#' @param lhs Object that will be returned if not NULL
#' @param rhs Object that will be returned if \code{lhs} is NULL. It maybe well be NULL
#'
#' @return The first parameter if it is not NULL otherwise the second parameter
#' @export
`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) {
    lhs
  } else {
    rhs
  }
}
