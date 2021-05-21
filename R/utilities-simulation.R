#' Get paths of all state variable quantities of the simulation
#'
#' @param simulation \code{Simulation} object
#' @details List of paths of all molecules in all compartments and all parameters that are
#' state variables.
#'
#' @return A list of paths
#' @import ospsuite
#' @export
getAllStateVariablesPaths <- function(simulation) {
  ospsuite:::validateIsOfType(simulation, type = "Simulation")
  allMoleculesPaths <- ospsuite:::getAllEntityPathsIn(container = simulation, entityType = ospsuite:::Molecule)
  allStateVariableParamsPaths <- ospsuite:::getAllEntityPathsIn(container = simulation, entityType = ospsuite:::Parameter, method = "AllStateVariableParameterPathsIn")
  allQantitiesPaths <- append(allMoleculesPaths, allStateVariableParamsPaths)
  return(allQantitiesPaths)
}

#' Get the steady-state values of species and state variable parameters.
#'
#' @details The steady-state is considered to be the last value of the simulation with sufficiently
#' long simulation time, i.e., where the rates of the processes do not (significantly) change.
#'
#' @param steadyStateTime Simulation time (minutes). Must be long enough for system to reach a steady-state. 1000 by default
#' @param quantitiesPaths List of quantity paths (molecules and/or parameters) for which the steady-state is to be simulated. If \code{NULL} (default), all molecules and state variable parameters are considered.
#' @param simulation \code{Simulation} object
#' @param ignoreIfFormula If \code{TRUE} (default), species and parameters with initial values defined by a formula are not included.
#' @param stopIfNotFound Boolean. If \code{TRUE} (default), an error is thrown when results for certain species were not generated.
#' This may happen when due to numerical problems some values cannot be calculated, though the whole simulation converges. Setting this argument to \code{FALSE} allows
#' to ignore such errors. Check the outputs for empty values when using this option.
#' @param lowerThreshold Numerical value (in µmol). Any steady-state values below this value are considered as numerical noise and replaced by 0. If \code{lowerThreshold} is \code{NULL},
#' no cut-off is applied. Default value is 1e-15.
#'
#' @return A list containing \code{paths} and their \code{values} at the end of the simulation.
#' @import ospsuite rClr
#' @export
getSteadyState <- function(quantitiesPaths = NULL, simulation, steadyStateTime, ignoreIfFormula = TRUE, stopIfNotFound = TRUE, lowerThreshold = 1e-15) {
  ospsuite:::validateIsOfType(simulation, type = "Simulation")
  ospsuite:::validateIsString(object = quantitiesPaths, nullAllowed = TRUE)

  if (steadyStateTime <= 0) {
    stop(messages$steadyStateTimeNotPositive(steadyStateTime))
  }

  # Have to reset both the output intervals and the time points!
  oldOutputIntervals <- simulation$outputSchema$intervals
  oldTimePoints <- simulation$outputSchema$timePoints
  oldOutputSelections <- simulation$outputSelections$allOutputs
  # Set simulation time to the steady-state value.
  ospsuite::setOutputInterval(simulation = simulation, startTime = 0, endTime = steadyStateTime, resolution = 1 / steadyStateTime)
  # If no quantities are explicitly specified, simulate all outputs.
  if (is.null(quantitiesPaths)) {
    quantitiesPaths <- getAllStateVariablesPaths(simulation)
  }
  ospsuite::addOutputs(quantitiesOrPaths = quantitiesPaths, simulation = simulation)
  simulationResults <- ospsuite::runSimulation(simulation)
  allOutputs <- ospsuite::getOutputValues(simulationResults, quantitiesOrPaths = quantitiesPaths, stopIfNotFound = stopIfNotFound, addMetaData = FALSE)

  # Container task is required for checking the "isFormula" property
  task <- ospsuite:::getContainerTask()
  # Get the end values of all outputs
  endValues <- lapply(quantitiesPaths, function(path) {
    # Check if the quantity is defined by an explicit formula
    isFormulaExplicit <- rClr::clrCall(task, "IsExplicitFormulaFromPath", simulation$ref, enc2utf8(path))

    if (ignoreIfFormula && isFormulaExplicit) {
      return(NULL)
    }
    value <- tail(allOutputs$data[path][[1]], 1)
    # Skip if value is NA. This happens if stopIfNotFound = FALSE and some species are not calculated
    if (is.na(value)) {
      return(NULL)
    }
    # If the value is below the cut-off threshold, replace it by 0
    if (!is.null(lowerThreshold) && value < lowerThreshold) {
      value <- 0
    }
    return(value)
  })

  # Get the indices for which the outputs have been calculated
  indices <- which(lengths(endValues) != 0)

  # reset the output intervals
  simulation$outputSchema$clear()
  for (outputInterval in oldOutputIntervals) {
    ospsuite::addOutputInterval(
      simulation = simulation, startTime = outputInterval$startTime$value,
      endTime = outputInterval$endTime$value,
      resolution = outputInterval$resolution$value
    )
  }
  if (length(oldTimePoints) > 0) {
    simulation$outputSchema$addTimePoints(oldTimePoints)
  }
  # Reset output selections
  ospsuite::clearOutputs(simulation)
  for (outputSelection in oldOutputSelections) {
    ospsuite::addOutputs(quantitiesOrPaths = outputSelection$path, simulation = simulation)
  }

  return(list(paths = quantitiesPaths[indices], values = endValues[indices]))
}
