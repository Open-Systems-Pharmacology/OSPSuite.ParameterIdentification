#' @title PIConfiguration
#' @docType class
#' @description Encapsulates configurations such as optimization algorithm
#'   choice, and evaluation settings for parameter identification.
#' @export
#' @format NULL
PIConfiguration <- R6::R6Class(
  "PIConfiguration",
  cloneable = TRUE,
  active = list(
    # #' @field simulateSteadyState Boolean representing whether the simulation
    # #' should be brought to a steady-state first
    # simulateSteadyState = function(value) {
    #   if (missing(value)) {
    #     private$.simulateSteadyState
    #   } else {
    #     validateIsLogical(value)
    #     private$.simulateSteadyState <- value
    #   }
    # },
    # #' @field steadyStateTime Time in minutes to simulate if simulating steady-state. May be NULL
    # steadyStateTime = function(value) {
    #   if (missing(value)) {
    #     private$.steadyStateTime
    #   } else {
    #     validateIsNumeric(value)
    #     if (value < 0) {
    #       stop(paste0("steadyStateTime must be a positive numerical value, but the value is ", value))
    #     }
    #     private$.steadyStateTime <- value
    #   }
    # },
    #' @field printEvaluationFeedback Boolean. If `TRUE`, prints objective
    #'   function value after each evaluation. Default is `FALSE`.
    printEvaluationFeedback = function(value) {
      if (missing(value)) {
        private$.printEvaluationFeedback
      } else {
        ospsuite.utils::validateIsLogical(value)
        private$.printEvaluationFeedback <- value
      }
    },

    #' @field simulationRunOptions Object of `SimulationRunOptions` for
    #'   simulation runs. If `NULL`, default options are used.
    simulationRunOptions = function(value) {
      if (missing(value)) {
        private$.simulationRunOptions
      } else {
        ospsuite.utils::validateIsOfType(
          value,
          "SimulationRunOptions",
          nullAllowed = TRUE
        )
        private$.simulationRunOptions <- value
      }
    },

    #' @field objectiveFunctionOptions Settings for model fit evaluation,
    #'   affecting error metrics and cost calculation. Defaults in
    #'   [`ObjectiveFunctionOptions`]. Partial lists are merged with the current
    #'   settings; only the provided keys are updated and validated.
    objectiveFunctionOptions = function(value) {
      if (missing(value)) {
        private$.objectiveFunctionOptions
      } else {
        ospsuite.utils::validateIsOfType(value, "list")
        if ("objectiveFunctionType" %in% names(value)) {
          stop(messages$errorObjectiveFunctionTypeRemoved())
        }
        if (
          !is.null(value$robustMethod) &&
            value$robustMethod != "none" &&
            private$.objectiveType == "mle"
        ) {
          stop(messages$errorMleRejectsRobust(value$robustMethod))
        }
        resolvedScaleVar <- value$scaleVar %||%
          private$.objectiveFunctionOptions$scaleVar
        resolvedResidualWeightingMethod <- value$residualWeightingMethod %||%
          private$.objectiveFunctionOptions$residualWeightingMethod
        if (
          isTRUE(resolvedScaleVar) &&
            resolvedResidualWeightingMethod == "error" &&
            private$.objectiveType == "mle"
        ) {
          stop(messages$errorMleRejectsScaleVar())
        }
        unknownKeys <- setdiff(names(value), names(ObjectiveFunctionSpecs))
        if (length(unknownKeys) > 0) {
          warning(
            messages$warningUnknownOptions(
              unknownKeys,
              "objectiveFunctionOptions"
            ),
            call. = FALSE
          )
          value <- value[names(value) %in% names(ObjectiveFunctionSpecs)]
        }
        if (length(value) == 0) {
          return(invisible(NULL))
        }
        ospsuite.utils::validateIsOption(
          value,
          ObjectiveFunctionSpecs[names(value)]
        )
        private$.objectiveFunctionOptions <- modifyList(
          private$.objectiveFunctionOptions,
          value
        )
      }
    },

    #' @field objectiveType Scoring mode for the objective function. See
    #'   [`ospsuite.parameteridentification::ObjectiveTypes`]. Defaults to `lsq`.
    objectiveType = function(value) {
      if (missing(value)) {
        private$.objectiveType
      } else {
        ospsuite.utils::validateIsCharacter(value)
        ospsuite.utils::validateEnumValue(value, ObjectiveTypes)
        if (value == "lsq" && private$.blqMethod == "m3") {
          stop(messages$errorLsqStrandsM3())
        }
        robustMethod <- private$.objectiveFunctionOptions$robustMethod
        if (value == "mle" && robustMethod != "none") {
          stop(messages$errorMleRejectsRobust(robustMethod))
        }
        if (
          value == "mle" &&
            isTRUE(private$.objectiveFunctionOptions$scaleVar) &&
            private$.objectiveFunctionOptions$residualWeightingMethod == "error"
        ) {
          stop(messages$errorMleRejectsScaleVar())
        }
        private$.objectiveType <- value
      }
    },

    #' @field blqRemove Mode selecting which BLQ observations enter the
    #'   objective. See [`ospsuite.parameteridentification::BLQRemoveModes`].
    #'   Defaults to `none`.
    blqRemove = function(value) {
      if (missing(value)) {
        private$.blqRemove
      } else {
        ospsuite.utils::validateIsCharacter(value)
        ospsuite.utils::validateEnumValue(value, BLQRemoveModes)
        private$.blqRemove <- value
      }
    },

    #' @field blqMethod Method selecting how retained BLQ observations
    #'   contribute to the cost. See
    #'   [`ospsuite.parameteridentification::BLQMethods`]. Defaults to
    #'   `lloqHalf`.
    blqMethod = function(value) {
      if (missing(value)) {
        private$.blqMethod
      } else {
        ospsuite.utils::validateIsCharacter(value)
        ospsuite.utils::validateEnumValue(value, BLQMethods)
        if (value == "m3" && private$.objectiveType != "mle") {
          stop(messages$errorM3RequiresMle(private$.objectiveType))
        }
        private$.blqMethod <- value
      }
    },

    #' @field blqOptions Named list of parameters consumed by the `m3` BLQ
    #'   method. Defaults in [`ospsuite.parameteridentification::BLQOptions`].
    #'   Partial lists are merged with the current settings; only the provided
    #'   keys are updated and validated. Unknown keys produce a warning and are
    #'   ignored.
    blqOptions = function(value) {
      if (missing(value)) {
        private$.blqOptions
      } else {
        ospsuite.utils::validateIsOfType(value, "list")
        unknownKeys <- setdiff(names(value), names(BLQOptionSpecs))
        if (length(unknownKeys) > 0) {
          warning(
            messages$warningUnknownOptions(unknownKeys, "blqOptions"),
            call. = FALSE
          )
          value <- value[names(value) %in% names(BLQOptionSpecs)]
        }
        if (length(value) == 0) {
          return(invisible(NULL))
        }
        ospsuite.utils::validateIsOption(value, BLQOptionSpecs[names(value)])
        private$.blqOptions <- modifyList(private$.blqOptions, value)
      }
    },

    #' @field algorithm Optimization algorithm name. See
    #'   [`ospsuite.parameteridentification::Algorithms`] for a list of
    #'   supported algorithms. Defaults to `BOBYQA`. Changing the algorithm
    #'   resets `algorithmOptions` to the new algorithm's defaults.
    algorithm = function(value) {
      if (missing(value)) {
        private$.algorithm
      } else {
        ospsuite.utils::validateIsCharacter(value)
        ospsuite.utils::validateEnumValue(value, Algorithms)
        if (value != private$.algorithm) {
          if (!is.null(private$.algorithmOptions)) {
            message(messages$messageOptionsReset(
              "algorithm",
              private$.algorithm,
              value,
              "algorithmOptions"
            ))
          }
          private$.algorithmOptions <- NULL
        }
        private$.algorithm <- value
      }
    },

    #' @field ciMethod Confidence interval estimation method. See
    #'   [`ospsuite.parameteridentification::CIMethods`] for available options.
    #'   Defaults to `hessian`. Changing the method resets `ciOptions` to the
    #'   new method's defaults.
    ciMethod = function(value) {
      if (missing(value)) {
        private$.ciMethod
      } else {
        ospsuite.utils::validateIsCharacter(value)
        ospsuite.utils::validateEnumValue(value, CIMethods)
        if (value != private$.ciMethod) {
          if (!is.null(private$.ciOptions)) {
            message(messages$messageOptionsReset(
              "ciMethod",
              private$.ciMethod,
              value,
              "ciOptions"
            ))
          }
          private$.ciOptions <- NULL
        }
        private$.ciMethod <- value
      }
    },

    #' @field algorithmOptions Named list of user-defined overrides for the
    #'   selected algorithm's settings. Returns `NULL` if no overrides are set;
    #'   in that case algorithm defaults (`AlgorithmOptions_XYZ`) are used
    #'   automatically. Partial lists are merged with previously stored
    #'   overrides. Unknown keys produce a warning and are ignored. Set to
    #'   `NULL` to clear all overrides.
    algorithmOptions = function(value) {
      if (missing(value)) {
        private$.algorithmOptions
      } else {
        if (is.null(value)) {
          private$.algorithmOptions <- NULL
          return(invisible(NULL))
        }
        ospsuite.utils::validateIsOfType(value, "list")
        validKeys <- names(AlgorithmDefaults[[private$.algorithm]])
        unknownKeys <- setdiff(names(value), validKeys)
        if (length(unknownKeys) > 0) {
          warning(
            messages$warningUnknownOptions(unknownKeys, "algorithmOptions"),
            call. = FALSE
          )
          value <- value[names(value) %in% validKeys]
        }
        if (length(value) == 0) {
          return(invisible(NULL))
        }
        private$.algorithmOptions <- modifyList(
          private$.algorithmOptions %||% list(),
          value
        )
      }
    },

    #' @field ciOptions Named list of settings for the selected CI method. Refer
    #'   to [`ospsuite.parameteridentification::CIOptions`] for default settings
    #'   per method (e.g., `CIOptions_XYZ` where `XYZ` corresponds to the method
    #'   name). If `NULL`, CI method's default settings are returned. Partial
    #'   lists are merged with the current settings; only the provided keys are
    #'   validated. Unknown keys produce a warning and are ignored. Set to
    #'   `NULL` to reset to defaults.
    ciOptions = function(value) {
      if (missing(value)) {
        private$.ciOptions
      } else {
        if (is.null(value)) {
          private$.ciOptions <- NULL
          return(invisible(NULL))
        }
        ospsuite.utils::validateIsOfType(value, "list")
        validKeys <- names(CIOptionSpecs[[private$.ciMethod]])
        unknownKeys <- setdiff(names(value), validKeys)
        if (length(unknownKeys) > 0) {
          warning(
            messages$warningUnknownOptions(unknownKeys, "ciOptions"),
            call. = FALSE
          )
          value <- value[names(value) %in% validKeys]
        }
        ospsuite.utils::validateIsOption(
          value,
          CIOptionSpecs[[private$.ciMethod]][names(value)]
        )
        private$.ciOptions <- modifyList(
          private$.ciOptions %||% list(),
          value,
          keep.null = TRUE
        )
      }
    },

    #' @field autoEstimateCI Logical. If `TRUE`, confidence intervals are
    #'   automatically estimated after optimization. If `FALSE`, the step is
    #'   skipped and can be triggered manually by calling the `estimateCI()`
    #'   method on the `ParameterIdentification` object.
    autoEstimateCI = function(value) {
      if (missing(value)) {
        private$.autoEstimateCI
      } else {
        ospsuite.utils::validateIsLogical(value)
        private$.autoEstimateCI <- value
      }
    },

    #' @field modelCostField Read-only field name in the cost object used as the
    #'   optimization target. Currently, only `modelCost` is supported.
    modelCostField = function(value) {
      if (missing(value)) {
        private$.modelCostField
      } else {
        stop(messages$errorPropertyReadOnly("modelCostField"))
      }
    }
  ),
  private = list(
    .simulateSteadyState = NULL,
    .steadyStateTime = NULL,
    .printEvaluationFeedback = NULL,
    .simulationRunOptions = NULL,
    .objectiveFunctionOptions = NULL,
    .objectiveType = NULL,
    .blqRemove = NULL,
    .blqMethod = NULL,
    .blqOptions = NULL,
    .algorithm = NULL,
    .algorithmOptions = NULL,
    .modelCostField = NULL,
    .ciMethod = NULL,
    .ciOptions = NULL,
    .autoEstimateCI = NULL
  ),
  public = list(
    #' @description Initialize a new instance of the class.
    #' @return A new `PIConfiguration` object.
    initialize = function() {
      private$.simulateSteadyState <- FALSE
      private$.steadyStateTime <- 1000
      private$.printEvaluationFeedback <- FALSE
      private$.objectiveFunctionOptions <- ObjectiveFunctionOptions
      private$.objectiveType <- "lsq"
      private$.blqRemove <- "none"
      private$.blqMethod <- "lloqHalf"
      private$.blqOptions <- BLQOptions
      private$.algorithm <- "BOBYQA"
      private$.ciMethod <- "hessian"
      private$.modelCostField <- "modelCost"
      private$.autoEstimateCI <- TRUE
    },

    #' @description Prints a summary of the `PIConfiguration`.
    print = function() {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "Optimization algorithm" = private$.algorithm,
        "Confidence interval method" = private$.ciMethod,
        "Objective type" = private$.objectiveType,
        "Residual weighting method" = private$.objectiveFunctionOptions$residualWeightingMethod,
        "Robust residual calculation method" = private$.objectiveFunctionOptions$robustMethod,
        "BLQ removal mode" = private$.blqRemove,
        "BLQ handling method" = private$.blqMethod,
        "Print feedback after each function evaluation" = private$.printEvaluationFeedback
      ))
      # private$printLine("Simulate to steady-state", private$.simulateSteadyState)
      # private$printLine("Steady-state time [min]", private$.steadyStateTime)
    }
  )
)
