#' Validate Input Options Against Valid Options
#'
#' This function checks if the provided input options are valid based on a list of valid options.
#' It ensures that each option in `inputOptions` matches the allowed types or values specified
#' in `validOptions`. For options expecting numeric values, it checks if they fall within a
#' specified range. For boolean options, it checks if they are strictly `TRUE` or `FALSE`.
#'
#' @param inputOptions A list of options to validate. Each option's value is checked against
#'   the corresponding entry in `validOptions`.
#' @param validOptions A list that specifies valid values or value types for each option.
#'   It can include specific values, types, or ranges for numeric options.
#'
#' @return
#' - If all input options are valid, the function returns `NULL`.
#' - If any input option is invalid, the function stops and signals an error with a
#'   message detailing the validation failures.
#'
#' @examples
#' validOptions <- list(
#'   algorithmType = c("gradient", "newton"),
#'   enableLogging = c(TRUE, FALSE),
#'   convergenceThreshold = list(type = "numeric", min = 1e-5, max = 0.1)
#' )
#'
#' inputOptions <- list(
#'   algorithmType = "gradient",
#'   enableLogging = TRUE,
#'   convergenceThreshold = 0.005
#' )
#' validateIsOption(inputOptions, validOptions)
#'
#' @export
validateIsOption <- function(inputOptions, validOptions) {
  messages <- list()
  for (optionName in names(inputOptions)) {
    inputOptionValue <- inputOptions[[optionName]]
    validOptionValue <- validOptions[[optionName]]

    if (is.null(validOptionValue)) {
      messages <- c(messages, paste("Unknown option:", optionName))
      next
    }

    if (is.list(validOptionValue) && validOptionValue$type == "numeric") {
      # Handle numeric range validation
      if (!is.numeric(inputOptionValue) || length(inputOptionValue) != 1) {
        messages <- c(messages, paste(optionName, "must be a single numeric value."))
      } else if (inputOptionValue < validOptionValue$min || inputOptionValue > validOptionValue$max) {
        messages <- c(messages, paste(optionName, "must be in the range", validOptionValue$min, "to", validOptionValue$max))
      }
    } else if (all(validOptionValue %in% c(TRUE, FALSE))) {
      # Check for boolean options
      if (!is.logical(inputOptionValue) || length(inputOptionValue) != 1) {
        messages <- c(messages, paste(optionName, "must be a boolean value (TRUE or FALSE)."))
      }
    } else {
      # Handle simple type/value validation
      if (!inputOptionValue %in% validOptionValue) {
        messages <- c(messages, paste("Invalid value for", optionName, ":", inputOptionValue))
      }
    }
  }

  if (length(messages) > 0) {
    stop(paste(messages, collapse = "\n"))
  } else {
    return()
  }
}
