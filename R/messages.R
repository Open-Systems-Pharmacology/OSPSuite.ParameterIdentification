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
