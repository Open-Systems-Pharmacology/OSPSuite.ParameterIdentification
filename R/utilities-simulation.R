#' Get the steady-state values of species and state variable parameters.
#'
#' @details The steady-state is considered to be the last value of the
#'   simulation with sufficiently long simulation time, i.e., where the rates of
#'   the processes do not (significantly) change.
#'
#' @param steadyStateTime Simulation time (minutes). Must be long enough for
#'   system to reach a steady-state. 1000 by default
#' @param quantitiesPaths List of quantity paths (molecules and/or parameters)
#'   for which the steady-state will be simulated. If `NULL` (default), all
#'   molecules and state variable parameters are considered. The same list is
#'   applied for all simulations.
#' @param simulations `Simulation` object or a list of `Simulation` objects
#' @param ignoreIfFormula If `TRUE` (default), species and parameters with
#'   initial values defined by a formula are not included.
#' @param stopIfNotFound Boolean. If `TRUE` (default), an error is thrown when
#'   results for certain species were not generated. This may happen when due to
#'   numerical problems some values cannot be calculated, though the whole
#'   simulation converges. Setting this argument to `FALSE` allows to ignore
#'   such errors. Check the outputs for empty values when using this option.
#' @param lowerThreshold Numerical value (in default unit of the output).
#' Any steady-state values below this value are considered as numerical noise
#' and replaced by 0. If `lowerThreshold` is `NULL`, no cut-off is applied.
#' Default value is 1e-15.
#' @param simulationRunOptions Optional instance of a `SimulationRunOptions`
#'  used during the simulation run.
#'
#' @return A named list, where the names are the IDs of the simulations and the
#'   entries are lists containing `paths` and their `values` at the end of the
#'   simulation.
#' @import ospsuite rClr hash
#' @export
getSteadyState <- function(quantitiesPaths = NULL,
                           simulations,
                           steadyStateTime,
                           ignoreIfFormula = TRUE,
                           stopIfNotFound = TRUE,
                           lowerThreshold = 1e-15,
                           simulationRunOptions = NULL) {
  validateIsOfType(simulations, type = "Simulation")
  validateIsString(quantitiesPaths, nullAllowed = TRUE)
  simulations <- toList(simulations)

  if (steadyStateTime <= 0) {
    stop(messages$steadyStateTimeNotPositive(steadyStateTime))
  }

  # First prepare all simulations by setting their outputs and time intervals
  # If no quantities have been specified, the quantities paths may be different
  # for each simulation and must be stored separately
  simulationState <- .storeSimulationState(simulations)
  quantitiesPathsMap <- hash::hash()
  for (simulation in simulations) {
    simId <- simulation$id
    # Set simulation time to the steady-state value.
    ospsuite::clearOutputIntervals(simulation = simulation)
    simulation$outputSchema$addTimePoints(timePoints = steadyStateTime)
    # If no quantities are explicitly specified, simulate all outputs.
    if (is.null(quantitiesPaths)) {
      quantitiesPathsMap[[simId]] <- ospsuite::getAllStateVariablesPaths(simulation)
    } else {
      quantitiesPathsMap[[simId]] <- quantitiesPaths
    }
    ospsuite::clearOutputs(simulation)
    ospsuite::addOutputs(quantitiesOrPaths = quantitiesPathsMap[[simId]], simulation = simulation)
  }

  # Run simulations concurrently
  simulationResults <- ospsuite::runSimulations(
    simulations = simulations,
    simulationRunOptions = simulationRunOptions
  )

  # Iterate through simulations and get their outputs
  outputMap <- hash::hash()
  for (simulation in simulations) {
    simId <- simulation$id
    simResults <- simulationResults[[simId]]

    allOutputs <- ospsuite::getOutputValues(
      simResults,
      quantitiesOrPaths = quantitiesPathsMap[[simId]],
      stopIfNotFound = stopIfNotFound,
      addMetaData = FALSE
    )

    # Get the end values of all outputs
    endValues <- lapply(quantitiesPathsMap[[simId]], function(path) {
      # Check if the quantity is defined by an explicit formula
      isFormulaExplicit <- ospsuite::isExplicitFormulaByPath(
        path = enc2utf8(path),
        simulation = simulation,
        stopIfNotFound = stopIfNotFound
      )

      if (ignoreIfFormula && isFormulaExplicit) {
        return(NULL)
      }
      value <- tail(allOutputs$data[path][[1]], 1)
      # Skip if value is NA. This happens if stopIfNotFound = FALSE and some
      # species are not calculated
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

    # Reset simulation output intervals and output selections
    .restoreSimulationState(simulations, simulationState)
    outputMap[[simId]] <- list(paths = quantitiesPathsMap[[simId]][indices], values = endValues[indices])
  }
  return(outputMap)
}

#' Get the simulation container of the entity
#'
#' @param entity Object of type `Entity`
#' @keywords internal
#'
#' @return The root container that is the parent of the entity.
.getSimulationContainer <- function(entity) {
  ospsuite.utils::validateIsOfType(entity, "Entity")
  if (ospsuite.utils::isOfType(entity, "Container")) {
    if (entity$containerType == "Simulation") {
      return(entity)
    }
  }
  return(.getSimulationContainer(entity$parentContainer))
}

#' Stores current simulation output state
#'
#' @description Stores simulation output intervals, output time points,
#' and output selections in the current state.
#'
#' @param simulations List of `Simulation` objects
#'
#' @return A named list with entries `outputIntervals`, `timePoints`, and
#' `outputSelections`. Every entry is a named list with names being the IDs
#' of the simulations.
#' @keywords internal
.storeSimulationState <- function(simulations) {
  simulations <- c(simulations)
  # Create named vectors for the output intervals, time points, and output
  # selections of the simulations in their initial state. Names are IDs of
  # simulations.
  oldOutputIntervals <-
    oldTimePoints <-
    oldOutputSelections <-
    ids <- vector("list", length(simulations))

  for (idx in seq_along(simulations)) {
    simulation <- simulations[[idx]]
    simId <- simulation$id
    # Have to reset both the output intervals and the time points!
    oldOutputIntervals[[idx]] <- simulation$outputSchema$intervals
    oldTimePoints[[idx]] <- simulation$outputSchema$timePoints
    oldOutputSelections[[idx]] <- simulation$outputSelections$allOutputs
    ids[[idx]] <- simId
  }
  names(oldOutputIntervals) <-
    names(oldTimePoints) <-
    names(oldOutputSelections) <- ids

  return(list(
    outputIntervals = oldOutputIntervals,
    timePoints = oldTimePoints,
    outputSelections = oldOutputSelections
  ))
}


#' Restore simulation output state
#'
#' @inheritParams .storeSimulationState
#' @param simStateList Output of the function `.storeSimulationState`.
#' A named list with entries `outputIntervals`, `timePoints`, and
#' `outputSelections`. Every entry is a named list with names being the IDs of
#' the simulations.
#'
#' @keywords internal
.restoreSimulationState <- function(simulations, simStateList) {
  simulations <- c(simulations)
  for (simulation in simulations) {
    simId <- simulation$id
    # reset the output intervals
    simulation$outputSchema$clear()
    for (outputInterval in simStateList$outputIntervals[[simId]]) {
      ospsuite::addOutputInterval(
        simulation = simulation,
        startTime = outputInterval$startTime$value,
        endTime = outputInterval$endTime$value,
        resolution = outputInterval$resolution$value
      )
    }
    if (length(simStateList$timePoints[[simId]]) > 0) {
      simulation$outputSchema$addTimePoints(simStateList$timePoints[[simId]])
    }
    # Reset output selections
    ospsuite::clearOutputs(simulation)
    for (outputSelection in simStateList$outputSelections[[simId]]) {
      ospsuite::addOutputs(quantitiesOrPaths = outputSelection$path, simulation = simulation)
    }
  }
}
