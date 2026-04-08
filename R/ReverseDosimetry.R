#' @title ReverseDosimetry
#' @docType class
#' @description Performs reverse dosimetry (QIVIVE — Quantitative In Vitro to
#'   In Vivo Extrapolation) by finding the external dose that yields a
#'   specified internal PK metric (e.g., C_max matching an in vitro EC50) in
#'   a PBPK simulation.
#'
#' @details
#' Reverse dosimetry is the inverse of forward simulation: instead of
#' computing internal concentrations from a known dose, it finds the external
#' dose that produces a target internal biomarker value. This is done by
#' numerical optimization of the dose parameter using one of the algorithms
#' available in [`PIConfiguration`].
#'
#' **Workflow:**
#' 1. Create a `ReverseDosimetry` instance with the simulation, a
#'    [`PIParameters`] object wrapping the dose parameter, and one or more
#'    [`RDOutputMapping`] objects specifying target PK metrics.
#' 2. Optionally configure the optimizer via a [`PIConfiguration`] object.
#' 3. Call `$run()` to obtain an [`RDResult`].
#'
#' **Auto-initialization**: `$run()` always runs the simulation once at the
#' parameter's start value to estimate an improved initial guess before
#' optimization, exploiting the approximate linearity of many PBPK models.
#' To skip this, manually set `$parameters$startValue` to the desired initial
#' dose before calling `$run()`.
#'
#' **Multiple output mappings**: When multiple [`RDOutputMapping`] objects
#' are supplied, the objective function minimizes the sum of squared
#' differences (in base units) between each achieved PK metric and its
#' target. This balances multiple targets simultaneously.
#'
#' **Simulation outputs**: Unlike [`ParameterIdentification`], this class
#' does **not** strip existing output intervals from the simulation, since
#' `ospsuite::calculatePKAnalyses()` requires the full time course. The
#' simulation outputs are restored to their original state after `$run()`
#' completes.
#'
#' **Confidence intervals (v2)**: CI estimation for reverse dosimetry
#' requires Monte Carlo sampling over PK model parameters and is not yet
#' implemented. See [`RDResult`]`$estimateCI()`.
#'
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

    #' @field parameters The `PIParameters` object representing the dose
    #'   parameter to optimize. Read-only.
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
    # SimulationBatch for the dose parameter
    .simulationBatch = NULL,
    # Named list of variable parameters for the batch: list(simId = list(path = startVal))
    .variableParameters = NULL,
    # PIParameters object (dose parameter)
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

      # Register dose parameter as a variable parameter
      private$.variableParameters <- list()
      private$.variableParameters[[simId]] <- list()

      for (param in private$.piParameters$parameters) {
        private$.variableParameters[[simId]][[param$path]] <-
          private$.piParameters$startValue
      }

      # Create simulation batch with the dose parameter as variable
      private$.simulationBatch <- ospsuite::createSimulationBatch(
        simulation = private$.simulation,
        parametersOrPaths = names(private$.variableParameters[[simId]])
      )
    },

    # Run the simulation for a given dose value and extract PK metrics.
    # Returns a named list: list(pkValues = list(numeric), cost = numeric)
    .evaluate = function(doseValue) {
      simId <- private$.simulation$root$id

      # Update dose parameter value in the variable parameter list
      for (param in private$.piParameters$parameters) {
        private$.variableParameters[[simId]][[param$path]] <- doseValue
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
              mapping$pkParameter, mapping$quantity$path, e$message
            ))
          }
        )

        pkVal <- pkParam$values

        # Store the raw value (in quantity display unit) for user-facing output
        pkValues[[i]] <- pkVal

        # Convert to base units for cost computation
        pkValBase <- tryCatch(
          ospsuite::toBaseUnit(
            quantityOrDimension = mapping$quantity$dimension,
            values = pkVal,
            unit = mapping$quantity$unit
          ),
          error = function(e) pkVal # fallback: compare as-is if conversion fails
        )

        cost <- cost + (pkValBase - mapping$targetValueInBaseUnit)^2
      }

      return(list(pkValues = pkValues, cost = cost))
    },

    # Objective function: returns scalar cost for the optimizer.
    .objectiveFunction = function(currVals) {
      private$.fnEvaluations <- private$.fnEvaluations + 1

      doseValue <- currVals[[1]]

      result <- tryCatch(
        private$.evaluate(doseValue),
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

    # Estimate a good starting dose by running at the current start value and
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

      # For each mapping, estimate: dose_needed ≈ target / pk_at_start * start
      estimates <- vapply(seq_along(private$.rdMappings), function(i) {
        pkValBase <- result$pkValues[[i]]
        targetBase <- private$.rdMappings[[i]]$targetValueInBaseUnit
        if (is.finite(pkValBase) && pkValBase > 0) {
          startVal * (targetBase / pkValBase)
        } else {
          NA_real_
        }
      }, numeric(1))

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
        message(messages$statusRDAutoInitResult(startValues, private$.piParameters$unit))
      }

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
    #' @param parameters A [`PIParameters`] object wrapping the dose
    #'   parameter(s) to optimize. The `startValue`, `minValue`, and
    #'   `maxValue` define the search space for the dose. Use
    #'   [`ospsuite::getParameter()`] to obtain the dose parameter, then
    #'   pass it to `PIParameters$new()`.
    #' @param outputMappings A single [`RDOutputMapping`] or a list of
    #'   `RDOutputMapping` objects. Each mapping specifies a simulation output
    #'   quantity, a PK metric (e.g., `"C_max"`), and the target value to
    #'   match. All quantities must belong to the same simulation.
    #' @param configuration (Optional) A [`PIConfiguration`] object
    #'   specifying the optimization algorithm and options. Defaults to a new
    #'   `PIConfiguration` with `BOBYQA` algorithm if omitted.
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
    #' @details Finds the external dose that minimizes the sum of squared
    #' differences (in base units) between the simulated PK metric(s) and
    #' their target(s). The simulation's output selections are temporarily
    #' modified during the run and restored afterwards.
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

      # Apply the optimized dose back to the parameter object
      private$.piParameters$setValue(optimResult$par[[1]])

      # Evaluate once more at optimized dose to collect achieved PK values
      finalEval <- tryCatch(
        private$.evaluate(optimResult$par[[1]]),
        error = function(e) list(pkValues = vector("list", length(private$.rdMappings)))
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
        "Dose parameter" = private$.piParameters$parameters[[1]]$path,
        "Dose unit" = private$.piParameters$unit,
        "Start value" = format(private$.piParameters$startValue, digits = 4),
        "Min value" = format(private$.piParameters$minValue, digits = 4),
        "Max value" = format(private$.piParameters$maxValue, digits = 4),
        "Number of output mappings" = length(private$.rdMappings),
        "Algorithm" = private$.configuration$algorithm
      ))
    }
  )
)
