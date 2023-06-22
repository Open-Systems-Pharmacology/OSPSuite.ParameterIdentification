#' Remove paths with formulas
#'
#' @description Removes paths to quantities that are defined by an explicit
#' formula in the simulation
#'
#' @param paths List of paths to be filtered
#' @param simulation A `Simulation` object containing the quantities
#' @param stopIfNotFound Boolean. If `TRUE` (default), an error is thrown when
#'   a path is not found in the simulation.
#'
#' @return List of quantity paths that are not defined by explicit formula.
#' @keywords internal
.removeFormulaPaths <- function(paths, simulation, stopIfNotFound = TRUE) {
  unlist(
    lapply(paths, function(path) {
      isFormulaExplicit <- ospsuite::isExplicitFormulaByPath(
        path = enc2utf8(path),
        simulation = simulation,
        stopIfNotFound = stopIfNotFound
      )
      if (isFormulaExplicit) {
        return(NULL)
      }
      return(path)
    }),
    use.names = FALSE
  )
}
