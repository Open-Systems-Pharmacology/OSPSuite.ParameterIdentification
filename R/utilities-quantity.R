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

#' Validate Observed Data Availability in PIOutputMapping
#'
#' Ensures each PIOutputMapping object has observed datasets. Throws an error
#' if no observed data is found.
#'
#' @param object A PIOutputMapping object or a list of PIOutputMapping objects.
#' @keywords internal
.validateOutputMappingHasData <- function(object) {
  mappings <- ospsuite.utils::toList(object)

  for (mapping in mappings) {
    if (inherits(mapping, "PIOutputMapping") &&
      length(mapping$observedDataSets) == 0) {
      quantityPath <- mapping$quantity$fullPath
      rootContainer <- mapping$quantity$call("get_RootContainer")
      rootContainerName <- rootContainer$call("get_Name")

      caller <- deparse(sys.call(-1)[[1]])
      stop(
        messages$errorObservedDataNotFound(
          caller, quantityPath, rootContainerName
        )
      )
    }
  }
}

#' Retrieve molecular weight for the molecule of a quantity
#'
#' @description Gets the molecular weight of a quantity's parent molecule in
#' `kg/µmol`.
#'
#' @param quantity A `Quantity` object.
#' @return Numeric value of the molecular weight in `kg/µmol`.
#' @keywords internal
.getMolWeightFor <- function(quantity) {
  ospsuite.utils::validateIsOfType(quantity, "Quantity")

  rootContainer <- .getParentContainer(quantity, "Simulation")
  moleculeContainer <- .getParentContainer(quantity, "Molecule")

  moleculeName <- moleculeContainer$name
  paramPath <- paste(moleculeName, "Molecular weight", sep = "|")

  task <- ospsuite:::.getNetTask("ContainerTask")
  paramMW <- task$call("AllParametersMatching", rootContainer, paramPath)
  molWeight <- paramMW[[1]]$call("get_Value")

  return(molWeight)
}
