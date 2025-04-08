messages <- ospsuite.utils::messages

messages$errorDimensionsNotEqual <- function() {
  "All quantities must have the same dimension, but they do not!"
}

messages$errorNoObservedDataSets <- function() {
  "Cannot assign weights: no observed data sets defined."
}

messages$errorWeightsNames <- function() {
  "All weights must be a named list with names matching observed data set names."
}

messages$errorWeightsLengthMismatch <- function(label, expected, actual) {
  sprintf(
    "Weights for '%s' must have length %d matching y-values, but got %d.",
    label, expected, actual
  )
}

messages$warningDataWeightsPresent <- function() {
  "Data weights have already been set. Check if they are still valid after adding new datasets."
}

messages$errorNotAFunction <- function() {
  "The assigned value must be a function with arguments 'xVals' and 'yVals'!"
}

messages$logSimulationError <- function(values, errorCondition) {
  message("Simulation failed for parameter values: ", toString(values))
  message("Error: ", errorCondition$message)
}

messages$simulationError <- function(values) {
  paste0("Returning infinite cost structure due to simulation failure.")
}

messages$initialSimulationError <- function() {
  "Stopping optimization: Initial simulation failed."
}

messages$profilesNotSupplied <- function() {
  "Supply the result of the calculateOFVProfiles() method as the argument to the plotOFVProfiles() method."
}

messages$plotGridParameterCount <- function(count) {
  paste0("The plotGrid() function requires a data frame with 3 columns, but ", count, " columns were supplied")
}

messages$gridSearchParameterValueSet <- function(bestValues) {
  cat(
    "Grid search completed.", "\n",
    "Starting point for the next optimization updated to parameter values: ", "\n",
    paste(signif(bestValues, 4), collapse = " ")
  )
}

messages$logScaleFlagError <- function() {
  "Logarithmic scaling is not available for non-positive parameter values."
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

messages$errorObservedDataNotFound <- function(caller, quantityPath, simulationPath) {
  sprintf(
    "%s: No observed data found for quantity path: \"%s\"\nin simulation: \"%s\"",
    caller, quantityPath, simulationPath
  )
}

messages$errorNoParentContainer <- function(type) {
  paste0(type, " is not a parent container of entity.")
}

messages$errorUnitConversion <- function(quantityName, observedDataName) {
  paste0(
    "Unit conversion failed for quantity '", quantityName,
    "' and observed data '", observedDataName, "'."
  )
}

messages$fixedParamError <- function(error) {
  if (error == "fixed") {
    "All parameters are fixed! Optimization requires at least one free parameter."
  } else if (error == "length") {
    "`fixedParams$idx` and `fixedParams$values` must have the same length."
  }
}

messages$objectiveFnOutputError <- function(field) {
  paste0("Objective function must return a list containing '", field, "'.")
}


messages$optimizationAlgorithm <- function(algorithm, error = FALSE) {
  if (error) {
    paste("Unknown optimization algorithm:", algorithm)
  } else {
    paste0("Starting optimization using '", algorithm, "' algorithm.")
  }
}

messages$ciMethod <- function(method, error = FALSE) {
  if (error) {
    paste("Unknown CI estimation method:", method)
  } else {
    paste0("Starting confidence interval estimation using '", method, "' method.")
  }
}

messages$ciEstimationError <- function(step, errorMessage) {
  paste0("Error during CI estimation step '", step, "': ", errorMessage)
}

messages$statusProfileLikelihood <- function(index, value) {
  paste0("Profiling CI for parameter ", index, ": ", value)
}

messages$plMaxiterWarning <- function(index) {
  paste0(
    "maxIter reached for parameter ", index,
    " without meeting cost threshold. Setting CI to Inf."
  )
}

messages$statusBootstrap <- function(index, nTotal) {
  paste0("Running bootstrap replicate ", index, " of ", nTotal, ".")
}
