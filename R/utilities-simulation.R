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

#' Resolve the variable buckets written by each `PIParameters` group
#'
#' @description Resolves, once, the simulation of every model parameter of the
#'   `PIParameters` groups and whether it is a state variable, so that parameter
#'   values can be applied without walking the model on every evaluation.
#'
#' @param piParameters List of `PIParameters` objects, in the order of the
#'   optimizer values.
#'
#' @return A list named by simulation IDs. Each entry holds `parameterPaths`
#'   and `moleculePaths`, the paths of the variable parameters and of the
#'   state-variable parameters of the simulation in the order in which they
#'   are first written, and `parameterGroups` and `moleculeGroups`, the index
#'   of the `PIParameters` group whose value each path takes. When several
#'   groups contain the same path, the last one wins.
#' @keywords internal
#' @noRd
.resolveParameterTargets <- function(piParameters) {
  targets <- list()
  for (idx in seq_along(piParameters)) {
    for (parameter in piParameters[[idx]]$parameters) {
      simId <- .getSimulationContainer(parameter)$id
      target <- targets[[simId]] %||%
        list(
          parameterPaths = character(),
          parameterGroups = integer(),
          moleculePaths = character(),
          moleculeGroups = integer()
        )
      kind <- if (parameter$isStateVariable) "molecule" else "parameter"
      pathsField <- paste0(kind, "Paths")
      groupsField <- paste0(kind, "Groups")
      position <- match(parameter$path, target[[pathsField]])
      if (is.na(position)) {
        target[[pathsField]] <- c(target[[pathsField]], parameter$path)
        target[[groupsField]] <- c(target[[groupsField]], idx)
      } else {
        target[[groupsField]][[position]] <- idx
      }
      targets[[simId]] <- target
    }
  }
  targets
}

#' Time values of simulation results
#'
#' @description Reads the time values of `SimulationResults` for all their
#'   individuals, in the order of `ospsuite::simulationResultsToDataFrame()`:
#'   the time values of every individual, one individual after the other,
#'   sorted by time with ties kept in this order.
#'
#' @param simulationResults A `SimulationResults` object.
#'
#' @return A list with `individualIds`, `xValues`, the sorted time values in
#'   min, and `order`, the positions of the sorted values among the unsorted
#'   ones.
#' @keywords internal
#' @noRd
.simulatedTimes <- function(simulationResults) {
  individualIds <- simulationResults$allIndividualIds
  timeValues <- rep(simulationResults$timeValues, length(individualIds))
  timeOrder <- order(timeValues, method = "radix")
  list(
    individualIds = individualIds,
    xValues = timeValues[timeOrder],
    order = timeOrder
  )
}

#' Simulated values of a quantity
#'
#' @description Reads the values of a quantity from `SimulationResults` as a
#'   numeric vector, in the order of `.simulatedTimes()`. As in
#'   `ospsuite::simulationResultsToDataFrame()`, the values are in the base
#'   unit of the quantity and missing values are `NA`.
#'
#' @param simulationResults A `SimulationResults` object.
#' @param path Path of the quantity.
#' @param times The result of `.simulatedTimes()` for `simulationResults`.
#'
#' @return A list with `xValues`, the time values in min, and `yValues`.
#' @keywords internal
#' @noRd
.simulatedValues <- function(simulationResults, path, times) {
  values <- simulationResults$getValuesByPath(path, times$individualIds)
  list(xValues = times$xValues, yValues = values[times$order])
}

#' Warning of a failed simulation run
#'
#' @description Whether a warning is the one with which ospsuite reports a
#'   failed simulation run, with the reason given by the simulation engine.
#'   `ospsuite::runSimulationBatches()` raises it in
#'   `.getConcurrentSimulationRunnerResults()`, once per failed run, unless
#'   its silent mode is on. If ospsuite raised it elsewhere, these warnings
#'   would be shown as they are and missing from the reasons of a failure,
#'   which the tests of failed simulations would show.
#'
#' @param condition A warning.
#'
#' @return `TRUE` or `FALSE`.
#' @keywords internal
#' @noRd
.isSimulationFailureWarning <- function(condition) {
  call <- conditionCall(condition)
  if (!is.call(call)) {
    return(FALSE)
  }
  # The name of the function, also when it is called as `ospsuite:::name()`
  functionName <- all.names(call[[1]])
  identical(
    functionName[length(functionName)],
    ".getConcurrentSimulationRunnerResults"
  )
}

#' Error of failed simulations
#'
#' @description The error raised when simulations fail. Its message is that
#'   of `messages$errorSimulationsFailed()`, and it keeps the arguments of the
#'   message, so that the objective functions can log a reason that they
#'   logged before in a shorter form.
#'
#' @param simulationNames The names of all simulations of the task.
#' @param failed The positions of the failed simulations.
#' @param reasons The messages of the simulation engine.
#' @param call The call to report with the error.
#'
#' @return A condition of class `simulationsFailedError`, with the fields
#'   `simulationNames`, `failed` and `reasons`.
#' @keywords internal
#' @noRd
.simulationsFailedError <- function(
  simulationNames,
  failed,
  reasons,
  call = NULL
) {
  errorCondition(
    messages$errorSimulationsFailed(simulationNames, failed, reasons),
    simulationNames = simulationNames,
    failed = failed,
    reasons = reasons,
    class = "simulationsFailedError",
    call = call
  )
}

#' Validates Matching IDs across Simulation IDs, PI Parameters, and Output
#' Mappings
#'
#' Ensures that every Simulation ID is present and matches with corresponding
#' IDs in `PIParameter` and `OutputMapping` instances. This function is crucial
#' for maintaining consistency and preventing mismatches that could disrupt
#' parameter identification processes.
#'
#' @param simulationIds Vector of simulation IDs.
#' @param piParameters List of `PIParameter` instances, from which IDs are
#'   extracted and validated against `simulationIds`.
#' @param outputMappings List of `OutputMapping` instances, from which IDs are
#'   extracted and validated against `simulationIds`.
#'
#' @return TRUE if all IDs match accordingly, otherwise throws an error
#'   detailing the mismatch or absence of IDs.
#' @keywords internal
.validateSimulationIds <- function(
  simulationIds,
  piParameters,
  outputMappings
) {
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
  outputMappingIds <- lapply(outputMappings, function(mapping) {
    .getSimulationContainer(mapping$quantity)$id
  })
  outputMappingIds <- unique(unlist(outputMappingIds))

  # sort IDs before comparison
  simulationIds <- sort(unique(unlist(simulationIds)))
  piParamIds <- sort(piParamIds)
  outputMappingIds <- sort(outputMappingIds)

  # Validate that simulationId is identical with piParamIds and outputMappingIds
  if (
    !identical(simulationIds, piParamIds) ||
      !identical(simulationIds, outputMappingIds)
  ) {
    stop(
      messages$errorSimulationIdMissing(
        simulationIds,
        piParamIds,
        outputMappingIds
      ),
      call. = TRUE
    )
  }

  return()
}

#' Stores current simulation output state
#'
#' @description Stores simulation output intervals, output time points, and
#'   output selections in the current state.
#'
#' @param simulations List of `Simulation` objects
#'
#' @return A named list with entries `outputIntervals`, `timePoints`, and
#'   `outputSelections`. Every entry is a named list with names being the IDs of
#'   the simulations.
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
#' @param simStateList Output of the function `.storeSimulationState`. A named
#'   list with entries `outputIntervals`, `timePoints`, and `outputSelections`.
#'   Every entry is a named list with names being the IDs of the simulations.
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
      ospsuite::addOutputs(
        quantitiesOrPaths = outputSelection$path,
        simulation = simulation
      )
    }
  }
}
