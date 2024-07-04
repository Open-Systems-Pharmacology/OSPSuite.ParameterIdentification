messages <- ospsuite.utils::messages

messages$errorDimensionsNotEqual <- function() {
  "All quantities must have the same dimension, but they do not!"
}

messages$errorNotAFunction <- function() {
  "The assigned value must be a function with arguments 'xVals' and 'yVals'!"
}

messages$simulationNotSuccessful <- function(values) {
  paste0("Simulation was not successful for parameter values: ", paste0(values, collapse = ", "))
}

messages$profilesNotSupplied <- function() {
  "Supply the result of the calculateOFVProfiles() method as the argument to the plotOFVProfiles() method."
}

messages$plotGridParameterCount <- function(count) {
  paste0("The plotGrid() function requires a data frame with 3 columns, but ", count, " columns were supplied")
}

messages$gridSearchParameterValueSet <- function() {
  "The best parameter value has been set as the starting point."
}

messages$runningOptimizationAlgorithm <- function(name) {
  paste0("Running optimization algorithm: ", name)
}

messages$hessianEstimation <- function() {
  "Post-hoc estimation of Hessian matrix."
}

messages$errorSimulationIdMissing <- function(simulationIds, piParamIds, outputMappingIds) {
  message <- capture.output(cat(
    "Mismatch or missing ID detected.\n",
    "Ensure each Simulation ID matches with corresponding PIParameter and OutputMapping IDs.\n",
    "Simulation IDs: ", paste(simulationIds, collapse = ", "), "\n",
    "PIParameter IDs: ", paste(piParamIds, collapse = ", "), "\n",
    "OutputMapping IDs: ", paste(outputMappingIds, collapse = ", ")
  ))

  return(paste(message, collapse = "\n"))
}
