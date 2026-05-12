#' @title PKOutputMapping
#' @docType class
#' @description Maps a simulation output quantity to a target PK metric for use
#'   in [`ParameterIdentification`] optimization.
#' @import R6 ospsuite.utils ospsuite
#' @export
#' @format NULL
PKOutputMapping <- R6::R6Class(
  "PKOutputMapping",
  cloneable = TRUE,
  active = list(
    #' @field quantity The `Quantity` object from the simulation. Read-only.
    quantity = function(value) {
      if (missing(value)) {
        private$.quantity
      } else {
        stop(messages$errorPropertyReadOnly("quantity"))
      }
    },

    #' @field simId ID of the root simulation container. Read-only.
    simId = function(value) {
      if (missing(value)) {
        private$.simId
      } else {
        stop(messages$errorPropertyReadOnly("simId"))
      }
    },

    #' @field pkParameter Name of the PK metric to target. Must be one of
    #'   [`ospsuite::StandardPKParameter`].
    pkParameter = function(value) {
      if (missing(value)) {
        private$.pkParameter
      } else {
        ospsuite.utils::validateIsString(value)
        ospsuite.utils::validateIsIncluded(
          value,
          names(ospsuite::StandardPKParameter)
        )
        old <- private$.pkParameter
        private$.pkParameter <- value
        tryCatch(
          private$.recomputeTarget(),
          error = function(e) {
            private$.pkParameter <- old
            stop(e)
          }
        )
      }
    },

    #' @field targetValueInBaseUnit Target value in base units. Computed from
    #'   `targetValue` and `targetUnit`. Read-only.
    targetValueInBaseUnit = function(value) {
      if (missing(value)) {
        private$.targetValueInBaseUnit
      } else {
        stop(messages$errorPropertyReadOnly("targetValueInBaseUnit"))
      }
    },

    #' @field targetValue Numeric target value in units of `targetUnit`.
    #'   Recomputes `targetValueInBaseUnit` on assignment.
    targetValue = function(value) {
      if (missing(value)) {
        private$.targetValue
      } else {
        ospsuite.utils::validateIsNumeric(value)
        ospsuite.utils::validateIsOfLength(value, 1L)
        if (anyNA(value)) {
          stop(messages$errorNAValue("targetValue"))
        }
        if (any(value <= 0)) {
          stop(messages$errorNonPositiveValue("targetValue"))
        }
        private$.targetValue <- value
        private$.recomputeTarget()
      }
    },

    #' @field targetUnit Unit string for `targetValue`. Must be compatible with
    #'   the PK parameter's dimension. Recomputes `targetValueInBaseUnit` on
    #'   assignment.
    targetUnit = function(value) {
      if (missing(value)) {
        private$.targetUnit
      } else {
        ospsuite.utils::validateIsString(value)
        old <- private$.targetUnit
        private$.targetUnit <- value
        tryCatch(
          private$.recomputeTarget(),
          error = function(e) {
            private$.targetUnit <- old
            stop(e)
          }
        )
      }
    }
  ),
  private = list(
    .quantity = NULL,
    .simId = NULL,
    .pkParameter = NULL,
    .targetValueInBaseUnit = NULL,
    .targetValue = NULL,
    .targetUnit = NULL,

    .recomputeTarget = function() {
      if (
        is.null(private$.pkParameter) ||
          is.null(private$.targetValue) ||
          is.null(private$.targetUnit)
      ) {
        return()
      }
      dim <- ospsuite::pkParameterByName(
        private$.pkParameter,
        stopIfNotFound = TRUE
      )$dimension
      private$.targetValueInBaseUnit <- tryCatch(
        ospsuite::toBaseUnit(
          quantityOrDimension = dim,
          values = private$.targetValue,
          unit = private$.targetUnit
        ),
        error = function(e) {
          stop(messages$errorPKMappingUnitConversion(
            private$.pkParameter,
            private$.targetUnit,
            dim,
            e$message
          ))
        }
      )
    }
  ),
  public = list(
    #' @description Initialize a new `PKOutputMapping`.
    #'
    #' @param quantity An ospsuite `Quantity` object from the simulation whose
    #'   PK profile will be analyzed.
    #' @param pkParameter Character string specifying the PK metric to target.
    #'   Must be one of [`ospsuite::StandardPKParameter`] (e.g., `"C_max"`,
    #'   `"AUC_tEnd"`).
    #' @param targetValue Numeric scalar target value in units of `targetUnit`.
    #' @param targetUnit Character string specifying the unit of `targetValue`.
    #'   Must be dimensionally compatible with `pkParameter`.
    #'
    #' @return A new `PKOutputMapping` object.
    #'
    #' @examples
    #' \dontrun{
    #' sim <- ospsuite::loadSimulation("model.pkml")
    #' quantity <- ospsuite::getQuantity(
    #'   "Organism|PeripheralVenousBlood|Drug|Plasma (Peripheral Venous Blood)",
    #'   container = sim
    #' )
    #' mapping <- PKOutputMapping$new(
    #'   quantity = quantity,
    #'   pkParameter = "C_max",
    #'   targetValue = 500,
    #'   targetUnit = "nmol/l"
    #' )
    #' }
    initialize = function(quantity, pkParameter, targetValue, targetUnit) {
      ospsuite.utils::validateIsOfType(quantity, "Quantity")
      ospsuite.utils::validateIsString(pkParameter)
      ospsuite.utils::validateIsIncluded(
        pkParameter,
        names(ospsuite::StandardPKParameter)
      )
      ospsuite.utils::validateIsNumeric(targetValue)
      ospsuite.utils::validateIsOfLength(targetValue, 1L)
      if (anyNA(targetValue)) {
        stop(messages$errorNAValue("targetValue"))
      }
      if (any(targetValue <= 0)) {
        stop(messages$errorNonPositiveValue("targetValue"))
      }
      ospsuite.utils::validateIsString(targetUnit)

      private$.quantity <- quantity
      private$.simId <- .getSimulationContainer(quantity)$id
      private$.pkParameter <- pkParameter
      private$.targetValue <- targetValue
      private$.targetUnit <- targetUnit
      private$.recomputeTarget()
    },

    #' @description Print a summary of the `PKOutputMapping`.
    #'
    #' @return Called for its side effect of printing. Returns `invisible(self)`.
    print = function() {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "Quantity path" = private$.quantity$path,
        "PK parameter" = private$.pkParameter,
        "Target value" = paste(
          format(private$.targetValue, digits = 4),
          private$.targetUnit
        )
      ))
    }
  )
)
