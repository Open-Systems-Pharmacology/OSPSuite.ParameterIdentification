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
#' @param quantitiesPaths List of quantity paths (molecules and/or parameters) for which the steady-state is to be simulated. If \code{NULL} (default), all molecules and state variable parameters are considered. The same list is applied for all simulations.
#' @param simulations \code{Simulation} object or a list of \code{Simulation} objects
#' @param ignoreIfFormula If \code{TRUE} (default), species and parameters with initial values defined by a formula are not included.
#' @param stopIfNotFound Boolean. If \code{TRUE} (default), an error is thrown when results for certain species were not generated.
#' This may happen when due to numerical problems some values cannot be calculated, though the whole simulation converges. Setting this argument to \code{FALSE} allows
#' to ignore such errors. Check the outputs for empty values when using this option.
#' @param lowerThreshold Numerical value (in µmol). Any steady-state values below this value are considered as numerical noise and replaced by 0. If \code{lowerThreshold} is \code{NULL},
#' no cut-off is applied. Default value is 1e-15.
#'
#' @return A named list, where the names are the IDs of the simulations and the entries are lists containing \code{paths} and their \code{values} at the end of the simulation.
#' @import ospsuite rClr hash
#' @export
getSteadyState <- function(quantitiesPaths = NULL, simulations, steadyStateTime, ignoreIfFormula = TRUE, stopIfNotFound = TRUE, lowerThreshold = 1e-15) {
  ospsuite:::validateIsOfType(simulations, type = "Simulation")
  ospsuite:::validateIsString(object = quantitiesPaths, nullAllowed = TRUE)
  simulations <- ospsuite:::toList(simulations)

  if (steadyStateTime <= 0) {
    stop(messages$steadyStateTimeNotPositive(steadyStateTime))
  }

  #First prepare all simulations by setting their outputs and time intervals
  #Create hash maps for the output intervals, time points, and output selections of the simulations in their initial state.
  oldOutputIntervals <- hash::hash()
  oldTimePoints <- hash::hash()
  oldOutputSelections <- hash::hash()
  #If no quantities have been specified, the quantities paths may be different for each simulation and must be stored separately
  quantitiesPathsMap <- hash::hash()
  for (simulation in simulations){
    simId <- simulation$id
    # Have to reset both the output intervals and the time points!
    oldOutputIntervals[[simId]] <- simulation$outputSchema$intervals
    oldTimePoints[[simId]] <- simulation$outputSchema$timePoints
    oldOutputSelections[[simId]] <- simulation$outputSelections$allOutputs
    # Set simulation time to the steady-state value.
    ospsuite::setOutputInterval(simulation = simulation, startTime = 0, endTime = steadyStateTime, resolution = 1 / steadyStateTime)
    # If no quantities are explicitly specified, simulate all outputs.
    if (is.null(quantitiesPaths)) {
      quantitiesPathsMap[[simId]] <- getAllStateVariablesPaths(simulation)
    }
    else {
      quantitiesPathsMap[[simId]] <- quantitiesPaths
    }
    ospsuite::addOutputs(quantitiesOrPaths = quantitiesPathsMap[[simId]], simulation = simulation)
  }

  #Run simulations concurrently
  simulationResults <- ospsuite::runSimulationsConcurrently(simulations = simulations)
  # Container task is required for checking the "isFormula" property
  task <- ospsuite:::getContainerTask()

  #Iterate through simulations and get their outputs
  outputMap <- hash::hash()
  for (simulation in simulations){
    simId <- simulation$id

    allOutputs <- ospsuite::getOutputValues(simulationResults[[simId]], quantitiesOrPaths = quantitiesPathsMap[[simId]], stopIfNotFound = stopIfNotFound, addMetaData = FALSE)


    # Get the end values of all outputs
    endValues <- lapply(quantitiesPathsMap[[simId]], function(path) {
      # Check if the quantity is defined by an explicit formula
      isFormulaExplicit <- rClr::clrCall(task, "IsExplicitFormulaByPath", simulation$ref, enc2utf8(path))

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
    for (outputInterval in oldOutputIntervals[[simId]]) {
      ospsuite::addOutputInterval(
        simulation = simulation, startTime = outputInterval$startTime$value,
        endTime = outputInterval$endTime$value,
        resolution = outputInterval$resolution$value
      )
    }
    if (length(oldTimePoints[[simId]]) > 0) {
      simulation$outputSchema$addTimePoints(oldTimePoints[[simId]])
    }
    # Reset output selections
    ospsuite::clearOutputs(simulation)
    for (outputSelection in oldOutputSelections[[simId]]) {
      ospsuite::addOutputs(quantitiesOrPaths = outputSelection$path, simulation = simulation)
    }

    outputMap[[simId]] <- list(paths = quantitiesPathsMap[[simId]][indices], values = endValues[indices])
  }

  return(outputMap)
}
