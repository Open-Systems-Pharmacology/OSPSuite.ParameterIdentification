messages <- ospsuite.utils::messages

messages$errorDimensionsNotEqual <- function() {
  "All quantities must have the same dimension, but they do not!"
}

messages$errorNotAFunction <- function() {
  "The assigned value must be a function with arguments 'xVals' and 'yVals'!"
}

messages$steadyStateTimeNotPositive <- function(value) {
  paste0("The value of `steadyStateTime` must be > 0, but it is ", value)
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
