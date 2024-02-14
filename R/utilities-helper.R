#' Enumerate With Values
#'
#' Creates an enumeration from specified values, facilitating value validation.
#'
#' @param enumValues A named list where each name corresponds to a parameter and its allowed values.
#'
#' @return A list where each element is named by the parameter and contains its allowed values.
#' @examples
#' options <- list(
#'   residualWeightingMethod = c("none", "std", "mean", "error"),
#'   robustMethod = c("none", "huber", "bisquare")
#' )
#' myOptions <- enumWithValues(options)
#' @export
enumWithValues <- function(enumValues) {
  myEnum <- list()
  for (param in names(enumValues)) {
    allowedValues <- enumValues[[param]]
    myEnum[[param]] <- allowedValues
  }
  return(myEnum)
}

#' Validate Input Options
#'
#' Validates the specified input options against a set of valid options and their allowed values or types.
#'
#' @param inputOptions A list of input options to validate.
#' @param validOptions A list of valid options with their allowed values or types.
#'
#' @return None; this function stops execution with an error message if validation fails.
#' @examples
#' validOptions <- list(
#'   option1 = c("value1", "value2"),
#'   option2 = "boolean",
#'   option3 = "numeric"
#' )
#' inputOptions <- list(option1 = "value1", option2 = TRUE, option3 = 5)
#' validateOptions(inputOptions, validOptions)
#' @export
validateIsOption <- function(inputOptions, validOptions) {
  for (optionName in names(inputOptions)) {
    # Check if the option name is valid
    if (!optionName %in% names(validOptions)) {
      stop(paste("Invalid option:", optionName))
    }

    allowedValues <- validOptions[[optionName]]
    inputValue <- inputOptions[[optionName]]

    # Handle when allowedValues indicates a type rather than specific values
    if (is.character(allowedValues) && length(allowedValues) == 1) {
      if (allowedValues == "numeric") {
        if (!is.numeric(inputValue)) {
          stop(paste("Option", optionName, "must be numeric."))
        }
      } else if (allowedValues == "boolean") {
        if (!is.logical(inputValue) || length(inputValue) != 1) {
          stop(paste("Option", optionName, "must be a boolean (TRUE or FALSE)."))
        }
      }
    } else if (!is.null(allowedValues) && !(inputValue %in% allowedValues)) {
      # For options with specific allowed values
      stop(paste("Invalid value for", optionName, ". Allowed values are:",
                 paste(allowedValues, collapse = ", "), "."))
    }
  }
}

