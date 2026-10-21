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
#'   message, so that the objective functions can log a reason of a kind that
#'   they logged before in a shorter form (see `.failureReasonKinds()`).
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

#' Lines of a reason of a failed simulation
#'
#' @param reason A message of the simulation engine.
#'
#' @return The lines of `reason` that are not empty.
#' @keywords internal
#' @noRd
.reasonLines <- function(reason) {
  lines <- strsplit(reason, "\r?\n")[[1]]
  lines[nzchar(trimws(lines))]
}

#' Kinds of the reasons of failed simulations
#'
#' @description The simulation engine gives the time of the failure in the
#'   first line of its message, for example "some variables became negative
#'   when trying to reach t=81", and for negative values the variables that
#'   became negative in the following lines. So the reasons of failures of
#'   one kind differ in their text from one evaluation to the next. The kind
#'   of a reason is its first line with every number replaced by "#".
#'
#' @param reasons The messages of the simulation engine.
#'
#' @return A character vector with the kind of each reason.
#' @keywords internal
#' @noRd
.failureReasonKinds <- function(reasons) {
  firstLines <- vapply(
    reasons,
    function(reason) {
      lines <- .reasonLines(reason)
      if (length(lines) == 0) "" else trimws(lines[[1]], which = "right")
    },
    character(1),
    USE.NAMES = FALSE
  )
  gsub("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?", "#", firstLines)
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
#' @description Stores the output selections, output intervals, and output
#'   time points of simulations in their current state.
#'
#' @param simulations List of `Simulation` objects
#'
#' @return A named list with names being the IDs of the simulations. Every
#'   entry is a list with the paths of the output selections (`outputs`), the
#'   output time points (`timePoints`), the name, start time, end time and
#'   resolution of every output interval (`intervals`), and the output
#'   interval objects (`intervalObjects`).
#' @keywords internal
.storeSimulationState <- function(simulations) {
  simulations <- c(simulations)
  simStateList <- lapply(simulations, function(simulation) {
    c(
      .outputStateValues(simulation),
      list(intervalObjects = simulation$outputSchema$intervals)
    )
  })
  names(simStateList) <- vapply(
    simulations,
    function(simulation) simulation$id,
    character(1)
  )
  simStateList
}

#' Values of the output state of a simulation
#'
#' @param simulation A `Simulation` object
#'
#' @return The entry of one simulation in the output of
#'   `.storeSimulationState()`.
#' @keywords internal
#' @noRd
.outputStateValues <- function(simulation) {
  list(
    outputs = vapply(
      simulation$outputSelections$allOutputs,
      function(output) output$path,
      character(1)
    ),
    timePoints = simulation$outputSchema$timePoints,
    intervals = lapply(simulation$outputSchema$intervals, function(interval) {
      list(
        name = interval$name,
        startTime = interval$startTime$value,
        endTime = interval$endTime$value,
        resolution = interval$resolution$value
      )
    })
  )
}

#' Restore simulation output state
#'
#' @inheritParams .storeSimulationState
#' @param simStateList Output of the function `.storeSimulationState`.
#'
#' @details The output selections and the output schema are restored only if
#'   they changed. The output schema gets its original output interval
#'   objects back, so that interval objects held by the user stay part of the
#'   simulation. If a simulation cannot be restored, a warning names it and
#'   the other simulations are still restored, so that the result or the
#'   error of the calling method is kept.
#'
#' @keywords internal
.restoreSimulationState <- function(simulations, simStateList) {
  simulations <- c(simulations)
  for (simulation in simulations) {
    tryCatch(
      .restoreOutputState(simulation, simStateList[[simulation$id]]),
      error = function(cond) {
        warning(
          messages$warningSimulationStateNotRestored(
            simulation$name,
            conditionMessage(cond)
          ),
          call. = FALSE
        )
      }
    )
  }
}

#' Restore the output state of one simulation
#'
#' @param simulation A `Simulation` object
#' @param savedState The entry of the simulation in the output of
#'   `.storeSimulationState()`.
#' @keywords internal
#' @noRd
.restoreOutputState <- function(simulation, savedState) {
  currentState <- .outputStateValues(simulation)
  schemaFields <- c("timePoints", "intervals")
  if (!identical(currentState[schemaFields], savedState[schemaFields])) {
    .restoreOutputSchema(simulation, savedState)
  }
  if (!identical(currentState$outputs, savedState$outputs)) {
    ospsuite::clearOutputs(simulation)
    for (path in savedState$outputs) {
      ospsuite::addOutputs(quantitiesOrPaths = path, simulation = simulation)
    }
  }
}

#' Restore the output schema of one simulation
#'
#' @description Clears the output schema, which removes the time points, and
#'   adds the original output interval objects and time points back.
#'
#' @inheritParams .restoreOutputState
#' @keywords internal
#' @noRd
.restoreOutputSchema <- function(simulation, savedState) {
  schema <- simulation$outputSchema
  schema$clear()
  intervals <- savedState$intervalObjects
  # The schema rejects a name it already has, but the intervals of the user
  # can share a name. They are added under unique names first.
  for (idx in seq_along(intervals)) {
    intervals[[idx]]$name <- paste("Restored output interval", idx)
    schema$addInterval(intervals[[idx]])
  }
  for (idx in seq_along(intervals)) {
    values <- savedState$intervals[[idx]]
    intervals[[idx]]$name <- values$name
    for (field in c("startTime", "endTime", "resolution")) {
      quantity <- intervals[[idx]][[field]]
      if (!identical(quantity$value, values[[field]])) {
        quantity$value <- values[[field]]
      }
    }
  }
  if (length(savedState$timePoints) > 0) {
    schema$addTimePoints(savedState$timePoints)
  }
}
