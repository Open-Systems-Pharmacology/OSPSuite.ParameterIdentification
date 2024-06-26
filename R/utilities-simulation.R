#' Get the simulation container of the entity
#'
#' @param entity Object of type `Entity`
#'
#' @return The root container that is the parent of the entity.
#' @keywords internal
.getSimulationContainer <- function(entity) {
  ospsuite.utils::validateIsOfType(entity, "Entity")
  if (ospsuite.utils::isOfType(entity, "Container")) {
    if (entity$containerType == "Simulation") {
      return(entity)
    }
  }
  return(.getSimulationContainer(entity$parentContainer))
}

#' Validates Matching IDs across Simulation IDs, PI Parameters, and Output Mappings
#'
#' Ensures that every Simulation ID is present and matches with corresponding IDs
#' in PIParameter and OutputMapping instances. This function is crucial for
#' maintaining consistency and preventing mismatches that could disrupt parameter
#' identification processes.
#'
#' @param simulationIds Vector of simulation IDs.
#' @param piParameters List of `PIParameter` instances, from which IDs are extracted
#' and validated against simulationIds.
#' @param outputMappings List of `OutputMapping` instances, from which IDs are
#' extracted and validated against simulationIds.
#'
#' @return TRUE if all IDs match accordingly, otherwise throws an error detailing
#' the mismatch or absence of IDs.
#' @keywords internal
.validateSimulationIds <- function(simulationIds, piParameters, outputMappings) {
  # Extract unique IDs from piParameters assuming up to two levels of list depth
  piParamIds <- lapply(piParameters, function(param) {
    if (is.list(param$parameters)) {
      return(lapply(param$parameters, function(sub_param) {
        .getSimulationContainer(sub_param)$id
      }))
    } else {
      return(.getSimulationContainer(param$parameters[[1]])$id)
    }
  })
  piParamIds <- unique(unlist(piParamIds))

  # Extract unique IDs from outputMappings
  outputMappingIds <- lapply(outputMappings, function(mapping) .getSimulationContainer(mapping$quantity)$id)
  outputMappingIds <- unique(unlist(outputMappingIds))

  # sort IDs before comparison
  simulationIds <- sort(unique(unlist(simulationIds)))
  piParamIds <- sort(piParamIds)
  outputMappingIds <- sort(outputMappingIds)

  # Validate that simulationId is idnetical with piParamIds and outputMappingIds
  if (!identical(simulationIds, piParamIds) || !identical(simulationIds, outputMappingIds)) {
    stop(messages$errorSimulationIdMissing(id))
  }

  return()
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
