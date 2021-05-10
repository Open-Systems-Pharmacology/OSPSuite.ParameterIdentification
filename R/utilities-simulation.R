#' Get all state variable quantities of the simulation
#'
#' @param simulation \code{Simulation} object
#' @param ignoreIfFormula If "TRUE", species and parameters with initial values defined by a formula are not included. Default is "TRUE".
#' @details Returned are instances of all molecules in all compartments and all parameters that are
#' state variables.
#'
#' @return A list of objects of type \code{Quantity}
#' @import ospsuite
#' @export
getAllStateVariables <- function(simulation, ignoreIfFormula = TRUE) {
  ospsuite:::validateIsOfType(simulation, type = "Simulation")

  quantities <- ospsuite::getAllMoleculesMatching("Organism|**", container = simulation)
  # Get all state variable parameters
  allParams <- ospsuite::getAllParametersMatching("**", container = simulation)
  rhsParams <- list()

  rhsParams <- lapply(allParams, function(param) {
    if (param$isStateVariable) {
      if (ignoreIfFormula && param$isFormula) {
        return()
      }
      return(param)
    }
    return()
  })
  quantities <- append(quantities, unlist(rhsParams, use.names = FALSE))
  return(quantities)
}

#' Get the steady-state values of species and state variable parameters.
#'
#' @details The steady-state is considered to be the last value of the simulation with sufficiently
#' long simulation time, i.e., where the rates of the processes do not (significantly) change.
#'
#' @param steadyStateTime Simulation time (minutes). Must be long enough for system to reach a steady-state. 1000 by default
#' @param quantities List of quantities (molecules and/or parameters) for which the steady-state is to be simulated. If \code{NULL} (default),
#' all molecules and state variable parameters are considered.
#' @param simulation Simulation object
#' @param ignoreIfFormula If "TRUE", species and parameters with initial values defined by a formula are not included. Default is "TRUE".
#' @param stopIfNotFound Boolean. If \code{TRUE} (default), an error is thrown when results for certain species were not generated.
#' This may happen when due to numerical problems some values cannot be calculated, though the whole simulation converges. Setting this argument to \code{FALSE} allows
#' to ignore such errors. Check the outputs for empty values when using this option.
#' @param lowerThreshold Numerical value. Any steady-state values below this value are considered as numerical noise and replaced by 0. If \code{lowerThreshold} is \code{NULL},
#' no cut-off is applied. Default value is 1e-15.
#'
#' @return A list containing \code{quantities} and their \code{values} at the end of the simulation.
#' @import ospsuite
#' @export
getSteadyState <- function(quantities = NULL, simulation, steadyStateTime, ignoreIfFormula = TRUE, stopIfNotFound = TRUE, lowerThreshold = 1e-15) {
  ospsuite:::validateIsOfType(simulation, type = "Simulation")
  ospsuite:::validateIsOfType(object = quantities, type = "Quantity", nullAllowed = TRUE)

  if (steadyStateTime <= 0) {
    stop("steadyStateTime must be > 0!")
  }

  # Have to reset both the output intervals and the time points!
  oldOutputIntervals <- simulation$outputSchema$intervals
  oldTimePoints <- simulation$outputSchema$timePoints
  # Set simulation time to the steady-state value.
  ospsuite::setOutputInterval(simulation = simulation, startTime = 0, endTime = steadyStateTime, resolution = 1 / steadyStateTime)
  # If no quantities are explicitly specified, simulate all outputs.
  if (is.null(quantities)) {
    quantities <- getAllStateVariables(simulation, ignoreIfFormula)
  }
  ospsuite::addOutputs(quantities, simulation)

  simulationResults <- ospsuite::runSimulation(simulation)
  allOutputs <- ospsuite::getOutputValues(simulationResults, quantitiesOrPaths = quantities, stopIfNotFound = stopIfNotFound)

  endValues <- lapply(quantities, function(quantity) {
    if (ignoreIfFormula && quantity$isFormula) {
      return(NULL)
    }
    path <- quantity$path
    value <- tail(allOutputs$data[path][[1]], 1)
    # Skip if value is NA. This happens if stopIfNotFound = FALSE and some species are not calculated
    if (is.na(value)) {
      return(NULL)
    }
    # If the value is below the cut-off threshold, replace it by 0
    # if (!is.null(lowerThreshold) && value < lowerThreshold){
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

  return(list(quantities = quantities[indices], values = endValues[indices]))
}
