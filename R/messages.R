messages <- ospsuite:::messages
messages$errorOSPSuitePISettingNotFound <- function(settingName) {
  paste0("No global setting with the name '", settingName, "' exists. Available global settings are:\n", paste0(names(piEnv), collapse = ", "))
}

messages$errorDimensionsNotEqual <- function() {
  "All quantities must have the same dimension, but they do not!"
}

messages$errorNotAFunction <- function() {
  "The assigned value must be a function with arguments 'xVals' and 'yVals'!"
}

messages$errorNumberOfCoresNotPositive <- function(value) {
  paste0("The number of cores must be greater then zero, but ", value, " has been passed!")
}

messages$errorWrongResidualsScaling <- function(value) {
  paste0("The value for 'residualsScaling' must be 'log' or 'lin', but ", value, " has been passed!")
}

messages$steadyStateTimeNotPositive <- function(value) {
  paste0("The value of `steadyStateTime` must be > 0, but it is ", value)
}
