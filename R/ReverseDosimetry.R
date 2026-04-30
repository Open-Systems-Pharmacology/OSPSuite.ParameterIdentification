#' @title ReverseDosimetry
#' @docType class
#' @description Performs reverse dosimetry (QIVIVE) by numerically optimizing
#'   a model parameter to match a target internal PK metric (e.g., C_max
#'   matching an in vitro EC50) in a PBPK simulation. Although designed for
#'   dose estimation, any parameter that drives the target PK metric can be
#'   used.
#' @import R6 ospsuite.utils ospsuite
#' @export
#' @format NULL
ReverseDosimetry <- R6::R6Class(
  "ReverseDosimetry",
  cloneable = FALSE,
  active = list(
    #' @field simulation The `Simulation` object used for optimization.
    #'   Read-only.
    simulation = function(value) {
      if (missing(value)) {
        private$.simulation
      } else {
        stop(messages$errorPropertyReadOnly("simulation"))
      }
    },

    #' @field parameters The `PIParameters` object representing the parameter
    #'   to optimize. Read-only.
    parameters = function(value) {
      if (missing(value)) {
        private$.piParameters
      } else {
        stop(messages$errorPropertyReadOnly("parameters"))
      }
    },

    #' @field outputMappings A list of `RDOutputMapping` objects defining
    #'   target PK metrics. Read-only.
    outputMappings = function(value) {
      if (missing(value)) {
        private$.rdMappings
      } else {
        stop(messages$errorPropertyReadOnly("outputMappings"))
      }
    },

    #' @field configuration The `PIConfiguration` controlling algorithm and
    #'   objective function settings. Replaceable.
    configuration = function(value) {
      if (missing(value)) {
        private$.configuration
      } else {
        ospsuite.utils::validateIsOfType(value, "PIConfiguration")
        private$.configuration <- value
      }
    }
  ),
  private = list(
    # Simulation object
    .simulation = NULL,
    # Named list of simulations keyed by root container ID (single entry for RD)
    .simulations = NULL,
    # SimulationBatch for the optimized parameter
    .simulationBatch = NULL,
    # Named list of variable parameters for the batch: list(simId = list(path = startVal))
    .variableParameters = NULL,
    # PIParameters object
    .piParameters = NULL,
    # List of RDOutputMapping objects
    .rdMappings = NULL,
    # PIConfiguration
    .configuration = NULL,
    # Saved simulation state for restoration after run()
    .savedSimulationState = NULL,
    # Number of objective function evaluations
    .fnEvaluations = 0,

    # Set up simulation batch. Unlike ParameterIdentification, output intervals
    # are preserved so calculatePKAnalyses() receives the full time course.
    # Only output quantities are cleared and re-added.
    .batchInitialization = function() {
      simId <- private$.simulation$root$id

      # Clear existing output selections and re-add only the mapped quantities
      ospsuite::clearOutputs(private$.simulation)
      for (mapping in private$.rdMappings) {
        ospsuite::addOutputs(
          quantitiesOrPaths = mapping$quantity,
          simulation = private$.simulation
        )
      }

      # Register the optimized parameter as a variable parameter
      private$.variableParameters <- list()
      private$.variableParameters[[simId]] <- list()

      for (param in private$.piParameters$parameters) {
        private$.variableParameters[[simId]][[param$path]] <-
          private$.piParameters$startValue
      }

      # Create simulation batch with the optimized parameter as variable
      private$.simulationBatch <- ospsuite::createSimulationBatch(
        simulation = private$.simulation,
        parametersOrPaths = names(private$.variableParameters[[simId]])
      )
    },

    # Run the simulation for a given parameter value and extract PK metrics.
    # Returns a named list: list(pkValues = list(numeric), cost = numeric)
    .evaluate = function(paramValue) {
      simId <- private$.simulation$root$id

      # Update the variable parameter value
      for (param in private$.piParameters$parameters) {
        private$.variableParameters[[simId]][[param$path]] <- paramValue
      }

      # Submit run values to the batch
      private$.simulationBatch$addRunValues(
        parameterValues = unlist(
          private$.variableParameters[[simId]],
          use.names = FALSE
        ),
        initialValues = numeric(0)
      )

      # Run the batch
      batchResults <- ospsuite::runSimulationBatches(
        simulationBatches = list(private$.simulationBatch),
        simulationRunOptions = private$.configuration$simulationRunOptions
      )

      # Extract first (and only) result set
      simResults <- batchResults[[private$.simulationBatch$id]][[1]]

      # Compute PK analyses from the full time course
      pkAnalysis <- ospsuite::calculatePKAnalyses(simResults)

      # Extract PK metric for each mapping and compute squared deviation
      pkValues <- vector("list", length(private$.rdMappings))
      cost <- 0

      for (i in seq_along(private$.rdMappings)) {
        mapping <- private$.rdMappings[[i]]

        pkParam <- tryCatch(
          pkAnalysis$pKParameterFor(
            quantityPath = mapping$quantity$path,
            pkParameter = mapping$pkParameter
          ),
          error = function(e) {
            stop(messages$errorRDPKParameterNotAvailable(
              mapping$pkParameter,
              mapping$quantity$path,
              e$message
            ))
          }
        )

        pkVal <- pkParam$values

        # Store the raw value (in base unit) for user-facing output
        pkValues[[i]] <- pkVal

        # Normalized relative cost: dimensionless, comparable across different
        # PK parameter dimensions (e.g. C_max in µmol/l and AUC in µmol*min/l)
        cost <- cost +
          ((pkVal - mapping$targetValueInBaseUnit) /
            mapping$targetValueInBaseUnit)^2
      }

      return(list(pkValues = pkValues, cost = cost))
    },

    # Objective function: returns scalar cost for the optimizer.
    .objectiveFunction = function(currVals) {
      private$.fnEvaluations <- private$.fnEvaluations + 1

      paramValue <- currVals[[1]]

      result <- tryCatch(
        private$.evaluate(paramValue),
        error = function(cond) {
          messages$logSimulationError(currVals, cond)
          return(NA)
        }
      )

      if (anyNA(result)) {
        if (private$.fnEvaluations == 1) {
          stop(messages$initialSimulationError())
        }
        message(messages$simulationError())
        return(.Machine$double.xmax)
      }

      if (private$.configuration$printEvaluationFeedback) {
        cat(messages$evaluationFeedback(
          private$.fnEvaluations,
          currVals,
          result$cost
        ))
      }

      return(result$cost)
    },

    # Estimate a good starting value by running at the current start value and
    # scaling proportionally to the target(s). Exploits the approximate
    # linearity of many PBPK models.
    .estimateInitialDose = function() {
      startVal <- private$.piParameters$startValue

      result <- tryCatch(
        private$.evaluate(startVal),
        error = function(e) NULL
      )

      if (is.null(result)) {
        return(NULL)
      }

      # For each mapping, estimate: value_needed ≈ target / pk_at_start * start
      estimates <- vapply(
        seq_along(private$.rdMappings),
        function(i) {
          pkVal <- result$pkValues[[i]]
          target <- private$.rdMappings[[i]]$targetValueInBaseUnit
          if (is.finite(pkVal) && pkVal > 0) {
            startVal * (target / pkVal)
          } else {
            NA_real_
          }
        },
        numeric(1)
      )

      validEstimates <- estimates[is.finite(estimates) & estimates > 0]

      if (length(validEstimates) == 0) {
        return(NULL)
      }

      # Use geometric mean for a balanced estimate across mappings
      return(exp(mean(log(validEstimates))))
    },

    # Run the configured optimizer. Returns the raw optimization result list.
    .runAlgorithm = function() {
      startValues <- private$.piParameters$startValue
      lower <- private$.piParameters$minValue
      upper <- private$.piParameters$maxValue

      # Auto-initialization: estimate a better starting dose by exploiting
      # near-linearity of PBPK models
      message(messages$statusRDAutoInit())
      initialGuess <- private$.estimateInitialDose()

      if (!is.null(initialGuess) && is.finite(initialGuess)) {
        startValues <- max(lower, min(upper, initialGuess))
        message(messages$statusRDAutoInitResult(
          startValues,
          private$.piParameters$unit
        ))
      }

      # ciMethod and ciOptions are not forwarded. Wire them here when implementing CI.
      optimizer <- Optimizer$new(configuration = private$.configuration)

      optimResult <- optimizer$run(
        par = startValues,
        fn = function(p, ...) private$.objectiveFunction(p),
        lower = lower,
        upper = upper
      )

      optimResult$startValues <- startValues

      return(optimResult)
    }
  ),
  public = list(
    #' @description Initialize a `ReverseDosimetry` instance.
    #'
    #' @param simulation A [`ospsuite::Simulation`] object loaded via
    #'   [`ospsuite::loadSimulation()`]. The simulation must have appropriate
    #'   output intervals configured for PK analysis.
    #' @param parameters A [`PIParameters`] object wrapping the parameter to
    #'   optimize. Typically a dose parameter, but any model parameter with a
    #'   monotonic effect on the target PK metric can be used. The
    #'   `startValue`, `minValue`, and `maxValue` define the search space.
    #' @param outputMappings A single [`RDOutputMapping`] or a list of
    #'   `RDOutputMapping` objects. Each mapping specifies a simulation output
    #'   quantity, a PK metric (e.g., `"C_max"`), and the target value to
    #'   match. All quantities must belong to the same simulation.
    #' @param configuration (Optional) A [`PIConfiguration`] object.
    #'   Defaults to a new `PIConfiguration` with `BOBYQA` algorithm if omitted.
    #'   The fields `algorithm`, `algorithmOptions`, `simulationRunOptions`, and
    #'   `printEvaluationFeedback` are used. `objectiveFunctionOptions` has no
    #'   effect because reverse dosimetry uses a fixed sum-of-squared-errors
    #'   objective. `ciMethod` and `ciOptions` are reserved for a future
    #'   release.
    #'
    #' @return A new `ReverseDosimetry` object ready to run via `$run()`.
    initialize = function(
      simulation,
      parameters,
      outputMappings,
      configuration = NULL
    ) {
      ospsuite.utils::validateIsOfType(simulation, "Simulation")
      ospsuite.utils::validateIsOfType(parameters, "PIParameters")
      ospsuite.utils::validateIsOfType(outputMappings, "RDOutputMapping")
      ospsuite.utils::validateIsOfType(
        configuration,
        "PIConfiguration",
        nullAllowed = TRUE
      )

      private$.simulation <- simulation
      private$.piParameters <- parameters
      private$.rdMappings <- ospsuite.utils::toList(outputMappings)

      # Default to BOBYQA for single-parameter problems (efficient for 1D)
      if (is.null(configuration)) {
        defaultConfig <- PIConfiguration$new()
        defaultConfig$algorithm <- "BOBYQA"
        private$.configuration <- defaultConfig
      } else {
        private$.configuration <- configuration
      }

      # Validate that all mappings belong to this simulation
      simId <- simulation$root$id
      mappingSimIds <- unique(sapply(private$.rdMappings, `[[`, "simId"))
      if (!all(mappingSimIds == simId)) {
        stop(messages$errorRDSimulationMismatch())
      }

      # Also store simulation in a named list for .storeSimulationState()
      private$.simulations <- list(simulation)
      names(private$.simulations) <- simId
    },

    #' @description Run the reverse dosimetry optimization.
    #'
    #' @details Finds the external dose that minimizes a dimensionless sum of
    #' squared relative errors between the simulated PK metric(s) and their
    #' target(s). Each mapping contributes `((simulated - target) / target)^2`
    #' in base units, so the objective is comparable across PK parameters with
    #' different physical dimensions (e.g., C_max and AUC). The simulation's
    #' output selections are temporarily modified during the run and restored
    #' afterwards.
    #'
    #' @return An [`RDResult`] object.
    run = function() {
      # Save and restore simulation state around the run
      private$.savedSimulationState <- .storeSimulationState(
        private$.simulations
      )

      on.exit({
        .restoreSimulationState(
          private$.simulations,
          private$.savedSimulationState
        )
        ospsuite::clearMemory()
      })

      private$.batchInitialization()
      private$.fnEvaluations <- 0

      optimResult <- private$.runAlgorithm()

      # Apply the optimized value back to the parameter object
      private$.piParameters$setValue(optimResult$par[[1]])

      # Evaluate once more at optimized value to collect achieved PK values
      finalEval <- tryCatch(
        private$.evaluate(optimResult$par[[1]]),
        error = function(e) {
          list(pkValues = as.list(rep(NA_real_, length(private$.rdMappings))))
        }
      )

      return(RDResult$new(
        optimResult = optimResult,
        achievedPKValues = finalEval$pkValues,
        rdMappings = private$.rdMappings,
        piParameters = private$.piParameters,
        configuration = private$.configuration
      ))
    },

    #' @description Print a summary of the `ReverseDosimetry` object.
    print = function() {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "Simulation" = private$.simulation$root$name,
        "Parameter" = private$.piParameters$parameters[[1]]$path,
        "Unit" = private$.piParameters$unit,
        "Start value" = format(private$.piParameters$startValue, digits = 4),
        "Min value" = format(private$.piParameters$minValue, digits = 4),
        "Max value" = format(private$.piParameters$maxValue, digits = 4),
        "Number of output mappings" = length(private$.rdMappings),
        "Algorithm" = private$.configuration$algorithm
      ))
    }
  )
)
