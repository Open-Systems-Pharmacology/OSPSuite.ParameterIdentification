#' @title RDOutputMapping
#' @docType class
#' @description Defines a mapping from a simulated output quantity to a target
#'   PK metric for use in [`ReverseDosimetry`] optimization.
#' @import R6 ospsuite.utils ospsuite
#' @export
#' @format NULL
RDOutputMapping <- R6::R6Class(
  "RDOutputMapping",
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

    #' @field pkParameter Name of the PK parameter to extract from simulation
    #'   results. Must be one of [`ospsuite::StandardPKParameter`]. Note: if changing this to
    #'   a parameter with a different physical dimension than the current one,
    #'   also update `$targetUnit` and `$targetValue` accordingly.
    pkParameter = function(value) {
      if (missing(value)) {
        private$.pkParameter
      } else {
        ospsuite.utils::validateIsString(value)
        ospsuite.utils::validateIsIncluded(
          value,
          names(ospsuite::StandardPKParameter)
        )
        private$.pkParameter <- value
      }
    },

    #' @field targetValue Numeric target value in units of `$targetUnit`.
    #'   Changing this recomputes the internal base-unit representation.
    targetValue = function(value) {
      if (missing(value)) {
        private$.targetValue
      } else {
        ospsuite.utils::validateIsNumeric(value)
        if (anyNA(value)) {
          stop(messages$errorNAValue("targetValue"))
        }
        if (any(value <= 0)) {
          stop(messages$errorNonPositiveValue("targetValue"))
        }
        private$.targetValue <- value
        private$.recomputeTargetInBaseUnit()
      }
    },

    #' @field targetUnit Unit of `$targetValue`. Must be compatible with the
    #'   quantity's dimension. Changing this recomputes the internal base-unit
    #'   representation.
    targetUnit = function(value) {
      if (missing(value)) {
        private$.targetUnit
      } else {
        ospsuite.utils::validateIsString(value)
        private$.targetUnit <- value
        private$.recomputeTargetInBaseUnit()
      }
    },

    #' @field targetValueInBaseUnit Target value converted to base units.
    #'   Computed from `targetValue` and `targetUnit` at construction and
    #'   updated whenever either changes. Read-only (used internally by the
    #'   optimization).
    targetValueInBaseUnit = function(value) {
      if (missing(value)) {
        private$.targetValueInBaseUnit
      } else {
        stop(messages$errorPropertyReadOnly("targetValueInBaseUnit"))
      }
    },

    #' @field simId ID of the root simulation container derived from the
    #'   quantity. Read-only.
    simId = function(value) {
      if (missing(value)) {
        private$.simId
      } else {
        stop(messages$errorPropertyReadOnly("simId"))
      }
    }
  ),
  private = list(
    .quantity = NULL,
    .pkParameter = NULL,
    .targetValue = NULL,
    .targetUnit = NULL,
    .targetValueInBaseUnit = NULL,
    .simId = NULL,

    # Convert targetValue from targetUnit to base units using the PK parameter's
    # dimension (not the quantity's dimension, which differs for e.g. AUC).
    # Stores result in .targetValueInBaseUnit.
    .recomputeTargetInBaseUnit = function() {
      if (
        !is.null(private$.targetValue) &&
          !is.null(private$.targetUnit) &&
          !is.null(private$.quantity) &&
          !is.null(private$.pkParameter)
      ) {
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
            stop(messages$errorRDUnitConversion(
              private$.pkParameter,
              private$.targetUnit,
              dim,
              e$message
            ))
          }
        )
      }
    }
  ),
  public = list(
    #' @description Initialize a new `RDOutputMapping`.
    #'
    #' @param quantity An ospsuite `Quantity` object from the simulation whose
    #'   PK profile will be analyzed (e.g., a plasma concentration output
    #'   obtained via [`ospsuite::getQuantity()`]).
    #' @param pkParameter Character string specifying the PK parameter to
    #'   evaluate. Must be one of [`ospsuite::StandardPKParameter`] (e.g., `"C_max"`,
    #'   `"AUC_tEnd"`).
    #' @param targetValue Numeric value that the PK parameter should match
    #'   (e.g., an in vitro EC50 or a benchmark concentration).
    #' @param targetUnit Character string specifying the unit of `targetValue`.
    #'   Must be dimensionally compatible with the output quantity (see
    #'   **Unit handling** in the class description).
    #'
    #' @return A new `RDOutputMapping` object.
    initialize = function(quantity, pkParameter, targetValue, targetUnit) {
      ospsuite.utils::validateIsOfType(quantity, "Quantity")
      ospsuite.utils::validateIsString(pkParameter)
      ospsuite.utils::validateIsIncluded(
        pkParameter,
        names(ospsuite::StandardPKParameter)
      )
      ospsuite.utils::validateIsNumeric(targetValue)
      if (anyNA(targetValue)) {
        stop(messages$errorNAValue("targetValue"))
      }
      if (any(targetValue <= 0)) {
        stop(messages$errorNonPositiveValue("targetValue"))
      }
      ospsuite.utils::validateIsString(targetUnit)

      private$.quantity <- quantity
      private$.pkParameter <- pkParameter
      private$.targetValue <- targetValue
      private$.targetUnit <- targetUnit
      private$.simId <- .getSimulationContainer(quantity)$id

      private$.recomputeTargetInBaseUnit()
    },

    #' @description Print a summary of the `RDOutputMapping`.
    print = function() {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(
        list(
          "Quantity path" = private$.quantity$path,
          "PK parameter" = private$.pkParameter,
          "Target value" = paste(
            format(private$.targetValue, digits = 4),
            private$.targetUnit
          )
        )
      )
    }
  )
)
