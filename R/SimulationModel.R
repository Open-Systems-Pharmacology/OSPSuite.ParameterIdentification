#' @title SimulationModel
#' @docType class
#' @description The simulations of a task, prepared for the evaluation of
#'   parameter values: the simulation batches, the variable parameter and
#'   molecule buckets that the `PIParameters` groups write, the output
#'   selection the batches were built from, and the log of failed
#'   simulations. `ParameterIdentification` builds one model per public call,
#'   and the objectives simulate through it.
#'
#' @keywords internal
#' @noRd
SimulationModel <- R6::R6Class(
  "SimulationModel",
  cloneable = FALSE,
  private = list(
    # Named list of simulations keyed by root container IDs
    .simulations = NULL,
    # List of `PIParameters` objects, in the order of the optimizer values
    .parameters = NULL,
    # `SimulationRunOptions` of the configuration, or `NULL`
    .simulationRunOptions = NULL,
    # Batches for result calculations, named by root container IDs
    .simulationBatches = NULL,
    # For steady state calculations, with different outputs and times, named by
    # root container IDs
    .steadyStateBatches = NULL,
    # Named list by simulation IDs, with paths and start values for variable
    # molecules
    .variableMolecules = NULL,
    # Named list by simulation IDs, detailing paths and start values for
    # variable parameters
    .variableParameters = NULL,
    # Named list by simulation IDs: the paths of the variable parameters and
    # molecules of each simulation, in the order of the variable buckets, and
    # the index of the `PIParameters` group whose value each of them takes
    # (see `.resolveParameterTargets()`)
    .parameterTargets = NULL,
    # Kinds of the reasons of failed simulations that were logged in full
    # (see `logFailure()`)
    .loggedFailureKinds = character(),
    # Number of failures logged
    .loggedFailures = 0,

    # Selects the quantities of the output mappings as the only outputs of the
    # simulations and adds the observed times to the output schemas
    .selectOutputs = function(outputMappings) {
      for (simulation in private$.simulations) {
        ospsuite::clearOutputs(simulation)
      }
      for (outputMapping in outputMappings) {
        simId <- outputMapping$simId
        simulation <- private$.simulations[[simId]]
        ospsuite::addOutputs(
          quantitiesOrPaths = outputMapping$quantity,
          simulation = simulation
        )
        for (dataset in outputMapping$observedDataSets) {
          label <- dataset$name
          xFactor <- outputMapping$dataTransformations$xFactors
          if (length(xFactor) != 1) {
            xFactor <- xFactor[[label]]
          }
          xOffset <- outputMapping$dataTransformations$xOffsets
          if (length(xOffset) != 1) {
            xOffset <- xOffset[[label]]
          }
          xVals <- ospsuite::toBaseUnit(
            ospsuite::ospDimensions$Time,
            values = (dataset$xValues + xOffset) * xFactor,
            unit = dataset$xUnit
          )
          simulation$outputSchema$addTimePoints(xVals)
        }
      }
    },

    # Selects the quantities of the PK mappings as the only outputs of the
    # simulations
    .selectPKOutputs = function(pkMappings) {
      for (simulation in private$.simulations) {
        ospsuite::clearOutputs(simulation)
      }
      for (mapping in pkMappings) {
        ospsuite::addOutputs(
          quantitiesOrPaths = mapping$quantity,
          simulation = private$.simulations[[mapping$simId]]
        )
      }
    },

    .simulationNames = function() {
      vapply(
        private$.simulations,
        function(simulation) simulation$name,
        character(1)
      )
    }
  ),
  active = list(
    # Named list of `Simulation` objects, keyed by root container IDs
    # (read-only)
    simulations = function(value) {
      if (missing(value)) {
        private$.simulations
      } else {
        stop(messages$errorPropertyReadOnly("simulations"))
      }
    },
    # Named list of `SimulationBatch` objects (read-only)
    simulationBatches = function(value) {
      if (missing(value)) {
        private$.simulationBatches
      } else {
        stop(messages$errorPropertyReadOnly("simulationBatches"))
      }
    },
    # Named list of the variable parameter buckets (read-only)
    variableParameters = function(value) {
      if (missing(value)) {
        private$.variableParameters
      } else {
        stop(messages$errorPropertyReadOnly("variableParameters"))
      }
    },
    # Named list of the variable molecule buckets (read-only)
    variableMolecules = function(value) {
      if (missing(value)) {
        private$.variableMolecules
      } else {
        stop(messages$errorPropertyReadOnly("variableMolecules"))
      }
    },
    # The resolved parameter targets (read-only)
    parameterTargets = function(value) {
      if (missing(value)) {
        private$.parameterTargets
      } else {
        stop(messages$errorPropertyReadOnly("parameterTargets"))
      }
    },
    # Number of failures logged with `logFailure()` (read-only)
    loggedFailures = function(value) {
      if (missing(value)) {
        private$.loggedFailures
      } else {
        stop(messages$errorPropertyReadOnly("loggedFailures"))
      }
    }
  ),
  public = list(
    # @param simulations Named list of `Simulation` objects, keyed by root
    #   container IDs.
    # @param parameters List of `PIParameters` objects, in the order of the
    #   optimizer values.
    # @param outputMappings List of `PIOutputMapping` objects whose quantities
    #   and observed times the batches are built for. The simulations are
    #   restored when the constructor exits.
    # @param pkMappings List of `PKOutputMapping` objects whose quantities are
    #   selected instead. `ospsuite::calculatePKAnalyses()` calculates the PK
    #   parameters of the outputs selected in the simulation, so they stay
    #   selected and the caller restores the simulations.
    # @param simulationRunOptions `SimulationRunOptions` for the batches, or
    #   `NULL` for the defaults.
    initialize = function(
      simulations,
      parameters,
      outputMappings = NULL,
      pkMappings = NULL,
      simulationRunOptions = NULL
    ) {
      ospsuite.utils::validateIsOfType(simulations, "Simulation")
      ospsuite.utils::validateIsOfType(parameters, "PIParameters")
      ospsuite.utils::validateIsOfType(
        outputMappings,
        "PIOutputMapping",
        nullAllowed = TRUE
      )
      ospsuite.utils::validateIsOfType(
        pkMappings,
        "PKOutputMapping",
        nullAllowed = TRUE
      )
      private$.simulations <- simulations
      private$.parameters <- parameters
      private$.simulationRunOptions <- simulationRunOptions
      ids <- names(simulations)
      private$.variableMolecules <-
        private$.variableParameters <-
          private$.simulationBatches <-
            private$.steadyStateBatches <- stats::setNames(
              vector("list", length(simulations)),
              ids
            )

      # 2DO: Enable steady-state
      # If steady-state should be simulated, get the set of all state variables for each simulation
      # if (private$.configuration$simulateSteadyState) {
      #   for (simulation in private$.simulations) {
      #     id <- simulation$root$id
      #     moleculePaths <- getAllMoleculePathsIn(container = simulation)
      #     # Only keep molecules that are not defined by formula
      #     moleculePaths <- .removeFormulaPaths(moleculePaths, simulation)
      #     moleculesStartValues <- getQuantityValuesByPath(
      #       quantityPaths = moleculePaths,
      #       simulation = simulation
      #     )
      #     # Save molecule start  values for this simulation ID
      #     private$.variableMolecules[[id]] <- moleculesStartValues
      #     names(private$.variableMolecules[[id]]) <- moleculePaths
      #
      #     variableParametersPaths <- getAllStateVariableParametersPaths(simulation = simulation)
      #     # Only keep parameters that initial values are not defined by formula
      #     variableParametersPaths <- .removeFormulaPaths(variableParametersPaths, simulation)
      #     # If the simulation does not contain any state variable parameters,
      #     # do not try to retrieve the values.
      #     if (!is.null(variableParametersPaths)){
      #       variableParametersValues <- getQuantityValuesByPath(
      #         quantityPaths = variableParametersPaths,
      #         simulation = simulation
      #       )
      #       # Save parameter values for this simulation ID
      #       private$.variableParameters[[id]] <- variableParametersValues
      #       names(private$.variableParameters[[id]]) <- variableParametersPaths
      #     }
      #   }
      # }

      if (is.null(pkMappings)) {
        # A batch copies the output selections and the output schema of its
        # simulation when it is built, so the simulations are restored when
        # the constructor exits, also on an error
        savedState <- .storeSimulationState(simulations)
        on.exit(
          .restoreSimulationState(simulations, savedState),
          add = TRUE
        )
        private$.selectOutputs(outputMappings)
      } else {
        private$.selectPKOutputs(pkMappings)
      }

      # Seed each optimization parameter's start value into its variable
      # bucket, from which the batches below are built
      private$.parameterTargets <- .resolveParameterTargets(parameters)
      self$applyParameterValues(
        vapply(parameters, function(p) p$startValue, numeric(1))
      )

      for (simId in ids) {
        private$.simulationBatches[[simId]] <- ospsuite::createSimulationBatch(
          simulation = simulations[[simId]],
          parametersOrPaths = names(private$.variableParameters[[simId]]),
          moleculesOrPaths = names(private$.variableMolecules[[simId]])
        )
      }

      # 2DO: Enable steady-state
      # If steady-state should be simulated, create new batches for ss simulation
      # Add all state variables to the outputs and set the simulation time to
      # steady state time
      # if (private$.configuration$simulateSteadyState) {
      #   for (simulation in private$.simulations) {
      #     simId <- simulation$root$id
      #     clearOutputIntervals(simulation)
      #     clearOutputs(simulation)
      #
      #     # FIXME: WILL NOT WORK UNTIL https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1029 is fixed!!
      #     simulation$outputSchema$addTimePoints(timePoints = private$.configuration$steadyStateTime)
      #     # If no quantities are explicitly specified, simulate all outputs.
      #     ospsuite::addOutputs(
      #       quantitiesOrPaths = ospsuite::getAllStateVariablesPaths(simulation),
      #       simulation = simulation
      #     )
      #
      #     simBatch <- createSimulationBatch(
      #       simulation = simulation,
      #       parametersOrPaths = names(private$.variableParameters[[simId]]),
      #       moleculesOrPaths = names(private$.variableMolecules[[simId]])
      #     )
      #     private$.steadyStateBatches[[simId]] <- simBatch
      #   }
      # }
    },

    # Applies a vector of optimizer values, one entry per `PIParameters` group,
    # to every underlying model parameter. Values arrive in each group's
    # `$unit`. `addRunValues()` reads base units, so they are converted here.
    # This is the only place that writes into the variable buckets.
    # State-variable (RHS-defined) parameters go into the molecule buckets,
    # all others into the parameter buckets.
    #
    # @param values Vector of parameter values.
    applyParameterValues = function(values) {
      if (length(values) != length(private$.parameters)) {
        stop(messages$errorParameterValuesLengthMismatch(
          length(private$.parameters),
          length(values)
        ))
      }
      baseValues <- vapply(
        seq_along(values),
        function(idx) {
          .toBaseValue(private$.parameters[[idx]], values[[idx]])
        },
        numeric(1)
      )
      for (simId in names(private$.parameterTargets)) {
        target <- private$.parameterTargets[[simId]]
        if (length(target$parameterPaths) > 0) {
          private$.variableParameters[[simId]] <- stats::setNames(
            baseValues[target$parameterGroups],
            target$parameterPaths
          )
        }
        if (length(target$moleculePaths) > 0) {
          private$.variableMolecules[[simId]] <- stats::setNames(
            baseValues[target$moleculeGroups],
            target$moleculePaths
          )
        }
      }
      invisible(self)
    },

    # Applies the parameter values to the simulation batches and runs them.
    # If a simulation whose results are used fails, stops with the names of
    # the simulations that failed and the reasons given by the simulation
    # engine.
    #
    # @param values Vector of parameter values, in the order of the
    #   `PIParameters`.
    # @param usedSimulations The IDs of the simulations whose results are
    #   used, by default all. The failure of another simulation (in PK mode, a
    #   simulation without a PK mapping) does not stop the run, and the
    #   warning of the simulation engine is shown for it, also in silent mode.
    # @param silentMode If `TRUE`, the warnings of the simulation engine for
    #   failed simulations are not shown; their reasons are still part of the
    #   error.
    # @return The result of `ospsuite::runSimulationBatches()`: for each
    #   simulation batch, the list of its `SimulationResults`, named by the
    #   simulation IDs.
    simulate = function(values, usedSimulations = NULL, silentMode = FALSE) {
      usedSimulations <- usedSimulations %||% names(private$.simulationBatches)
      self$applyParameterValues(values)

      ##### 2DO - implement Steady-State when issue in Core is fixed
      # # Simulate steady-states if specified
      # if (configuration$simulateSteadyState) {
      #
      #   steadyStateResults <- vector("list", length(private$.steadyStateBatches))
      #   #Set values for each simulation batch
      #   for (simBatchIdx in seq_along(private$.steadyStateBatches)){
      #     simId <- names(private$.steadyStateBatches)[[simBatchIdx]]
      #     simBatch <- private$.steadyStateBatches[[simBatchIdx]]
      #     resultsId <- simBatch$addRunValues(parameterValues = private$.variableParameters[[simId]],
      #                           initialValues = private$.variableMolecules[[simId]])
      #
      #     names(steadyStateResults)[[simBatchIdx]] <- resultsId
      #   }
      #
      #   # Run steady-state batches
      #   ssResults <- ospsuite::runSimulationBatches(simulationBatches = private$.steadyStateBatches,
      #                        simulationRunOptions = private$.configuration$simulationRunOptions)
      #####

      for (simId in names(private$.simulationBatches)) {
        private$.simulationBatches[[simId]]$addRunValues(
          parameterValues = unlist(
            private$.variableParameters[[simId]],
            use.names = FALSE
          ),
          initialValues = unlist(
            private$.variableMolecules[[simId]],
            use.names = FALSE
          )
        )
      }
      # The simulation engine gives the reason for a failed simulation only
      # in a warning, which the silent mode of `runSimulationBatches()` drops.
      # So these warnings are collected for the error below, and muffled here
      # in silent mode. Any other warning is left as it is.
      engineWarnings <- list()
      simulationResults <- withCallingHandlers(
        ospsuite::runSimulationBatches(
          simulationBatches = private$.simulationBatches,
          simulationRunOptions = private$.simulationRunOptions
        ),
        warning = function(w) {
          if (.isSimulationFailureWarning(w)) {
            engineWarnings[[length(engineWarnings) + 1]] <<- w
            if (silentMode) {
              invokeRestart("muffleWarning")
            }
          }
        }
      )
      # The results come in the order of the batches, named by batch IDs
      names(simulationResults) <- names(private$.simulationBatches)

      # A failed simulation has no results (#299)
      failed <- vapply(
        simulationResults,
        function(results) length(results) == 0 || is.null(results[[1]]),
        logical(1)
      )
      if (any(failed[usedSimulations])) {
        # The message gives the position in the task of a failed simulation
        # whose name other simulations share. The engine does not say which
        # reason belongs to which simulation, so the message names every
        # failed simulation, also one whose results are not used.
        stop(.simulationsFailedError(
          private$.simulationNames(),
          failed = match(
            names(simulationResults)[failed],
            names(private$.simulations)
          ),
          reasons = vapply(engineWarnings, conditionMessage, character(1)),
          call = sys.call()
        ))
      }
      # Without a failed simulation whose results are used, the warnings of
      # the engine are about simulations whose results are not used. They are
      # shown in silent mode, too.
      if (silentMode) {
        for (engineWarning in engineWarnings) {
          warning(engineWarning)
        }
      }
      simulationResults
    },

    # Runs the simulations with the parameter values and reads the simulated
    # values of every output mapping from the results as numeric vectors (see
    # `.simulatedValues()`). An output with missing values is a failed
    # simulation, like one the engine reports.
    #
    # @param values Vector of parameter values.
    # @param outputMappings The output mappings of the evaluation.
    # @return A list with one entry per output mapping, each a list with
    #   `xValues`, the time values in min, and `yValues`, the values in the
    #   base unit of the mapped quantity.
    simulatedOutputs = function(values, outputMappings) {
      # The objective reports a failed simulation itself on every evaluation,
      # so the warning of the simulation engine is not repeated
      simulationResults <- self$simulate(values, silentMode = TRUE)

      # Time values are read once per simulation
      times <- list()
      simulated <- vector("list", length(outputMappings))
      for (idx in seq_along(outputMappings)) {
        simId <- outputMappings[[idx]]$simId
        path <- outputMappings[[idx]]$quantity$path
        resultObject <- simulationResults[[simId]][[1]]
        times[[simId]] <- times[[simId]] %||% .simulatedTimes(resultObject)
        simulated[[idx]] <- .simulatedValues(resultObject, path, times[[simId]])
        if (anyNA(simulated[[idx]]$yValues)) {
          stop(.simulationsFailedError(
            private$.simulationNames(),
            failed = match(simId, names(private$.simulations)),
            reasons = messages$simulatedValuesMissing(path),
            call = sys.call()
          ))
        }
      }
      simulated
    },

    # Logs a failed evaluation. The reason given by the simulation engine can
    # be long (for negative values, it lists every variable that became
    # negative) and comes again on many evaluations, often with another time
    # of the failure in its first line. So a reason is logged in full only at
    # the first failure of its kind in the life of the model (see
    # `.failureReasonKinds()`), and later reasons of that kind with their
    # first line only.
    #
    # @param values Vector of parameter values of the evaluation.
    # @param cond The error of the evaluation.
    logFailure = function(values, cond) {
      private$.loggedFailures <- private$.loggedFailures + 1
      if (inherits(cond, "simulationsFailedError")) {
        reasons <- unique(cond$reasons)
        kinds <- .failureReasonKinds(reasons)
        # Of several reasons of one kind in the same error, only the first is
        # logged in full
        shorten <- kinds %in% private$.loggedFailureKinds | duplicated(kinds)
        private$.loggedFailureKinds <- union(
          private$.loggedFailureKinds,
          kinds
        )
        reasons[shorten] <- messages$shortenedFailureReason(reasons[shorten])
        cond$message <- messages$errorSimulationsFailed(
          cond$simulationNames,
          cond$failed,
          reasons
        )
      }
      messages$logSimulationError(values, cond)
      invisible(self)
    }
  )
)
