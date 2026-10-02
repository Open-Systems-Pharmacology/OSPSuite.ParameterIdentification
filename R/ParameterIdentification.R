#' @title ParameterIdentification
#' @docType class
#' @description Performs parameter estimation by fitting model simulations to
#' observed data. Supports customizable optimization and confidence interval
#' methods.
#' @export
#' @format NULL
ParameterIdentification <- R6::R6Class(
  "ParameterIdentification",
  cloneable = FALSE,
  active = list(
    #' @field simulations A named list of `Simulation` objects, keyed by the IDs
    #'   of their root containers.
    simulations = function(value) {
      if (missing(value)) {
        as.list(private$.simulations)
      } else {
        stop(messages$errorPropertyReadOnly("simulations"))
      }
    },

    #' @field parameters A list of `PIParameters`, each representing a grouped
    #'   set of model parameters to be optimized (read-only).
    parameters = function(value) {
      if (missing(value)) {
        private$.piParameters
      } else {
        stop(messages$errorPropertyReadOnly("parameters"))
      }
    },

    #' @field configuration A `PIConfiguration` object controlling algorithm, CI
    #'   estimation, and objective function options.
    configuration = function(value) {
      if (missing(value)) {
        private$.configuration
      } else {
        ospsuite.utils::validateIsOfType(value, "PIConfiguration")
        private$.configuration <- value
      }
    },

    #' @field outputMappings A list of `PIOutputMapping` objects mapping
    #'   observed datasets to simulated outputs.
    outputMappings = function(value) {
      if (missing(value)) {
        private$.outputMappings
      } else {
        stop(messages$errorPropertyReadOnly("outputMappings"))
      }
    },

    #' @field pkOutputMappings A list of `PKOutputMapping` objects for PK
    #'   metric optimization. `NULL` in standard PI mode. Read-only.
    pkOutputMappings = function(value) {
      if (missing(value)) {
        private$.pkMappings
      } else {
        stop(messages$errorPropertyReadOnly("pkOutputMappings"))
      }
    }
  ),
  private = list(
    # Named list of simulations keyed by root container IDs
    .simulations = NULL,
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
    # the index of the `PIParameters` group whose value each of them takes.
    # Resolved by `.resolveParameterTargets()` when the batches are built.
    .parameterTargets = NULL,
    # List of `PIParameter` objects for optimization
    .piParameters = NULL,
    # List of `PIOutputMapping` objects
    .outputMappings = NULL,
    # `PIConfiguration` object instance
    .configuration = NULL,
    # Indicates if simulation batches need initialization. Used for plotting.
    .needBatchInitialization = TRUE,
    # Stores simulation state if saved during batch creation
    .savedSimulationState = NULL,
    # Observed data of each output mapping in base units, read once per
    # public call and bootstrap sample by `.getObservedData()`
    .observedData = NULL,
    # Kinds of the reasons of failed simulations that the objective functions
    # logged in full in the current public call (see `.logSimulationFailure()`)
    .loggedFailureKinds = character(),
    # Named list by simulation IDs: the observed times, in min, that were
    # added to the output time points when the batches were built
    .outputTimePoints = NULL,
    # Whether the observed times of the current public call were checked
    # against the output time points (see `.checkObservedTimes()`)
    .observedTimesChecked = FALSE,
    # Stores last optimization result
    .lastOptimResult = NULL,
    # Stores full cost summary from the best objective function evaluation
    .bestCostSummary = NULL,
    # Stores full cost summary from last objective function evaluation
    .lastCostSummary = NULL,
    # Number of function evaluations
    .fnEvaluations = 0,
    # List of `PKOutputMapping` objects (PK metric mode)
    .pkMappings = NULL,
    # Flag to indicate if objective function is being called from grid search
    .gridSearchFlag = FALSE,

    .assertNotPKMode = function(methodName) {
      if (!is.null(private$.pkMappings)) {
        stop(messages$errorMethodNotApplicableInPKMode(methodName))
      }
    },
    # Most recently used bootstrap seed to detect when resampling is needed
    .activeBootstrapSeed = NULL,
    # Cached original weights and values of all datasets before bootstrap
    .initialOutputMappingState = NULL,
    # Fitted GPR models for aggregated datasets, used during bootstrap
    # resampling
    .gprModels = NULL,

    # Applies a vector of optimizer values, one entry per `PIParameters` group,
    # to every underlying model parameter. Values arrive in each group's
    # `$unit`. `addRunValues()` reads base units, so they are converted here.
    # This is the only place that writes into the variable buckets.
    # State-variable (RHS-defined) parameters go into the molecule buckets,
    # all others into the parameter buckets.
    .applyParameterValues = function(values) {
      if (length(values) != length(private$.piParameters)) {
        stop(messages$errorParameterValuesLengthMismatch(
          length(private$.piParameters),
          length(values)
        ))
      }
      if (is.null(private$.parameterTargets)) {
        private$.parameterTargets <- .resolveParameterTargets(
          private$.piParameters
        )
      }
      baseValues <- vapply(
        seq_along(values),
        function(idx) {
          .toBaseValue(private$.piParameters[[idx]], values[[idx]])
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
    },

    # Batch Initialization for Simulations
    #
    # Initializes simulation batches, preparing them for parameter
    # identification by clearing outputs, updating schemas with observed data,
    # and setting variable parameters. Optimizes repeated calls by checking
    # initialization necessity.
    .batchInitialization = function() {
      # Every public method that evaluates the objective function starts here.
      # Observed data sets and their transformations can change between two
      # calls, so the observed data are read again. The output time points of
      # the simulations are only set below, when the batches are built at the
      # first call. After a change of the x values of the observed data (for
      # example of `xOffsets` or `xFactors`) or with a new data set, the
      # simulated values at new observed times are therefore interpolated
      # between the output time points of the first call. A new observed time
      # outside the simulated times, or a censored value at a new time with
      # the M3 method, has no simulated value, so the cost of its output
      # mapping is infinite. `.checkObservedTimes()` warns about it at the
      # first evaluation of the call; it has nothing to check when the
      # batches are built below, from the current observed data.
      private$.observedData <- NULL
      private$.observedTimesChecked <- private$.needBatchInitialization
      # The reasons of failed simulations are logged in full again
      private$.loggedFailureKinds <- character()

      # If the flag is already set to FALSE, short-cuts the execution of the
      # function. This way, the function call be called repeatedly with minimal
      # overhead
      if (private$.needBatchInitialization) {
        .savedSimulationState <- .storeSimulationState(private$.simulations)

        # Prepare simulations

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

        # Clear output quantities of all simulations
        for (simulation in private$.simulations) {
          ospsuite::clearOutputs(simulation)
        }

        if (!is.null(private$.pkMappings)) {
          for (mapping in private$.pkMappings) {
            simulation <- private$.simulations[[mapping$simId]]
            ospsuite::addOutputs(
              quantitiesOrPaths = mapping$quantity,
              simulation = simulation
            )
          }
        } else {
          private$.outputTimePoints <- list()
          for (outputMapping in private$.outputMappings) {
            simId <- outputMapping$simId
            simulation <- private$.simulations[[simId]]
            ospsuite::addOutputs(
              quantitiesOrPaths = outputMapping$quantity,
              simulation = simulation
            )
            observedDataSets <- outputMapping$observedDataSets
            transformations <- .transformationsByDataSet(
              outputMapping$dataTransformations,
              names(observedDataSets)
            )
            for (label in names(observedDataSets)) {
              dataset <- observedDataSets[[label]]
              xVals <- ospsuite::toBaseUnit(
                ospsuite::ospDimensions$Time,
                values = (dataset$xValues +
                  transformations$xOffsets[[label]]) *
                  transformations$xFactors[[label]],
                unit = dataset$xUnit
              )
              simulation$outputSchema$addTimePoints(xVals)
              private$.outputTimePoints[[simId]] <- c(
                private$.outputTimePoints[[simId]],
                xVals
              )
            }
          }
        }

        # Seed each optimization parameter's start value into its variable
        # bucket. The paths of the buckets, from which the batches below are
        # built, are resolved again.
        private$.parameterTargets <- NULL
        private$.applyParameterValues(
          vapply(private$.piParameters, function(p) p$startValue, numeric(1))
        )

        # Create simulation batches for identification runs
        for (simulation in private$.simulations) {
          simId <- simulation$root$id
          # Parameters and molecules defined in the previous steps will be
          # variable.
          simBatch <- ospsuite::createSimulationBatch(
            simulation = simulation,
            parametersOrPaths = names(private$.variableParameters[[simId]]),
            moleculesOrPaths = names(private$.variableMolecules[[simId]])
          )
          private$.simulationBatches[[simId]] <- simBatch
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
        private$.needBatchInitialization <- FALSE
      }
    },

    # Aggregate Model Cost Calculation
    #
    # Calculates and aggregates the model cost across all output mappings for
    # parameter estimation. Adjusts the evaluations counter, processes each
    # output mapping's cost via `.mappingCostTerms()` (the steps of
    # `.calculateCostMetrics()` on numeric vectors), and aggregates the
    # results into total cost summary.
    # @param currVals Vector of parameter values for simulation.
    # @param bootstrapSeed Optional bootstrap seed. If given, the output
    #   mappings are resampled for it (see `.getOutputMappings()`).
    # @return Aggregated total cost summary.
    .objectiveFunction = function(currVals, bootstrapSeed = NULL) {
      # Increment function evaluations counter
      private$.fnEvaluations <- private$.fnEvaluations + 1

      outputMappings <- private$.getOutputMappings(bootstrapSeed)

      # The observed data are static within a public call and bootstrap
      # sample: they are read on the first evaluation and reused, so the .NET
      # `DataSet` objects are not read and converted to base units on every
      # evaluation. They are read outside of the `tryCatch()` below, so that
      # an error in the observed data stops the call with its own message
      # instead of being reported as a failed simulation.
      observedData <- private$.getObservedData(outputMappings)

      # Run simulation and catch errors
      simulatedList <- tryCatch(
        private$.simulateOutputs(currVals, outputMappings = outputMappings),
        error = function(cond) {
          private$.logSimulationFailure(currVals, cond)
          return(NA)
        }
      )

      # Handle simulation failure
      if (anyNA(simulatedList)) {
        if (private$.fnEvaluations == 1 && !private$.gridSearchFlag) {
          stop(messages$initialSimulationError())
        } else {
          message(messages$simulationError())
          return(.createErrorCostStructure())
        }
      }

      if (!private$.observedTimesChecked) {
        private$.checkObservedTimes(simulatedList, observedData, outputMappings)
        private$.observedTimesChecked <- TRUE
      }

      # Evaluate cost per output mapping, on the simulated values and the
      # prepared observed data (see `.mappingCostTerms()`)
      costTerms <- vector("list", length(outputMappings))
      validatedScalings <- character()
      for (idx in seq_along(outputMappings)) {
        # Extract cost function options
        costControl <- private$.configuration$objectiveFunctionOptions
        costControl$scaling <- outputMappings[[idx]]$scaling
        # The options of the output mappings differ only in their scaling
        if (!costControl$scaling %in% validatedScalings) {
          ospsuite.utils::validateIsOption(
            options = costControl,
            validOptions = ObjectiveFunctionSpecs
          )
          validatedScalings <- c(validatedScalings, costControl$scaling)
        }

        costTerms[[idx]] <- .mappingCostTerms(
          simulated = simulatedList[[idx]],
          observed = observedData[[idx]],
          dataWeights = outputMappings[[idx]]$dataWeights,
          costControl = costControl,
          index = idx,
          quantityPath = outputMappings[[idx]]$quantity$path
        )
      }

      # Aggregate cost across all output mappings, in one step
      runningCost <- .combineCostTerms(costTerms)
      private$.lastCostSummary <- runningCost

      # Evaluate running cost
      costField <- private$.configuration$modelCostField
      currCost <- runningCost[[costField]]
      bestCost <- if (is.null(private$.bestCostSummary)) {
        Inf
      } else {
        private$.bestCostSummary[[costField]]
      }

      # Only overwrite when strictly better
      if (is.finite(currCost) && currCost < bestCost) {
        private$.bestCostSummary <- runningCost
      }

      #  Optionally print evaluation feedback
      if (private$.configuration$printEvaluationFeedback) {
        cat(
          messages$evaluationFeedback(
            private$.fnEvaluations,
            currVals,
            runningCost[[private$.configuration$modelCostField]]
          )
        )
      }

      return(runningCost)
    },

    .pkObjectiveFunction = function(currVals) {
      private$.fnEvaluations <- private$.fnEvaluations + 1

      pkValues <- tryCatch(
        private$.getPKValues(currVals),
        error = function(cond) {
          if (private$.fnEvaluations == 1) {
            stop(cond)
          }
          private$.logSimulationFailure(currVals, cond)
          return(NA)
        }
      )

      if (any(!vapply(pkValues, is.finite, logical(1)))) {
        message(messages$simulationError())
        return(.Machine$double.xmax)
      }

      cost <- 0
      for (i in seq_along(private$.pkMappings)) {
        stopifnot(length(pkValues[[i]]) == 1L)
        target <- private$.pkMappings[[i]]$targetValueInBaseUnit
        if (target <= 0) {
          stop(messages$errorPKZeroTarget(
            private$.pkMappings[[i]]$pkParameter,
            private$.pkMappings[[i]]$quantity$path
          ))
        }
        cost <- cost + ((pkValues[[i]] - target) / target)^2
      }

      if (private$.configuration$printEvaluationFeedback) {
        cat(messages$evaluationFeedback(private$.fnEvaluations, currVals, cost))
      }

      return(cost)
    },

    # Logs a failed evaluation of an objective function. The reason given by
    # the simulation engine can be long (for negative values, it lists every
    # variable that became negative) and comes again on many evaluations,
    # often with another time of the failure in its first line. So a reason
    # is logged in full only at the first failure of its kind in a public call
    # (see `.failureReasonKinds()` and `.batchInitialization()`), and later
    # reasons of that kind with their first line only.
    #
    # @param currVals Vector of parameter values of the evaluation.
    # @param cond The error of the evaluation.
    .logSimulationFailure = function(currVals, cond) {
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
      messages$logSimulationError(currVals, cond)
    },

    .getPKValues = function(paramValues) {
      # The PK objective function reports a failed simulation itself on every
      # evaluation, so the warning of the simulation engine is not repeated.
      # Only the results of the simulations of the PK mappings are used: the
      # task may have other simulations, whose failure does not fail the
      # evaluation.
      simulationResults <- private$.runSimulations(
        paramValues,
        silentMode = TRUE,
        usedSimulations = unique(vapply(
          private$.pkMappings,
          function(mapping) mapping$simId,
          character(1)
        ))
      )

      lapply(private$.pkMappings, function(mapping) {
        simResult <- simulationResults[[mapping$simId]][[1]]
        pkAnalysis <- ospsuite::calculatePKAnalyses(simResult)
        pkParam <- tryCatch(
          pkAnalysis$pKParameterFor(
            quantityPath = mapping$quantity$path,
            pkParameter = mapping$pkParameter
          ),
          error = function(e) {
            stop(messages$errorPKParameterNotAvailable(
              mapping$pkParameter,
              mapping$quantity$path,
              e$message
            ))
          }
        )
        vals <- pkParam$values
        if (length(vals) == 0L) {
          stop(messages$errorPKParameterNotAvailable(
            mapping$pkParameter,
            mapping$quantity$path
          ))
        }
        if (length(vals) > 1L) {
          stop(messages$errorPKMultiIndividualSimulation(
            mapping$pkParameter,
            mapping$quantity$path,
            length(vals)
          ))
        }
        vals
      })
    },

    # Run Simulations with Parameter Values
    #
    # Applies the parameter values to the simulation batches and runs them.
    # If a simulation whose results are used fails, stops with the names of
    # the simulations that failed and the reasons given by the simulation
    # engine.
    #
    # @param currVals Vector of parameter values, in the order of the
    #   `PIParameters` in the parameters list.
    # @param silentMode If `TRUE`, the warnings of the simulation engine for
    #   failed simulations are not shown; their reasons are still part of the
    #   error.
    # @param usedSimulations The IDs of the simulations whose results are
    #   used, by default all. The failure of another simulation (in PK mode, a
    #   simulation without a PK mapping) does not stop the run, and the
    #   warning of the simulation engine is shown for it, also in silent mode.
    # @return The result of `ospsuite::runSimulationBatches()`: for each
    #   simulation batch, the list of its `SimulationResults`, named by the
    #   simulation IDs.
    .runSimulations = function(
      currVals,
      silentMode = FALSE,
      usedSimulations = names(private$.simulationBatches)
    ) {
      private$.applyParameterValues(currVals)

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

      # Apply initial and parameter values to simulation batches
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
      # Run simulation batches. The simulation engine gives the reason for a
      # failed simulation only in a warning, which the silent mode of
      # `runSimulationBatches()` drops. So these warnings are collected for
      # the error below, and muffled here in silent mode. Any other warning
      # is left as it is.
      engineWarnings <- list()
      simulationResults <- withCallingHandlers(
        ospsuite::runSimulationBatches(
          simulationBatches = private$.simulationBatches,
          simulationRunOptions = private$.configuration$simulationRunOptions
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
        simulationNames <- vapply(
          private$.simulations,
          function(simulation) simulation$name,
          character(1)
        )
        stop(.simulationsFailedError(
          simulationNames,
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

    # Simulation Evaluation with Parameter Values
    #
    # Evaluates simulations using specified parameter values, updating each
    # parameter before simulation runs. Generates `DataCombined` objects for
    # each output mapping, encapsulating both simulated and observed data.
    # Used for plotting; the objective function uses `.simulateOutputs()`.
    #
    # @param currVals Vector of parameter values for simulation.
    # @param bootstrapSeed Optional bootstrap seed. If given, the output
    #   mappings are resampled for it (see `.getOutputMappings()`).
    # @return List of `DataCombined` objects, one per output mapping.
    .evaluate = function(currVals, bootstrapSeed = NULL) {
      outputMappings <- private$.getOutputMappings(bootstrapSeed)
      simulationResults <- private$.runSimulations(currVals)

      obsVsPredList <- vector("list", length(outputMappings))
      for (idx in seq_along(outputMappings)) {
        obsVsPred <- ospsuite::DataCombined$new()
        currOutputMapping <- outputMappings[[idx]]
        # Construct group names out of output path and simulation id
        groupName <- currOutputMapping$quantity$path
        # In each iteration, only one values set per simulation batch is
        # simulated. Therefore we always need the first results entry of the
        # simulation that is the parent of the output quantity.
        resultObject <- simulationResults[[currOutputMapping$simId]][[1]]
        obsVsPred$addSimulationResults(
          resultObject,
          quantitiesOrPaths = currOutputMapping$quantity$path,
          names = groupName,
          groups = groupName
        )
        # Observed data in the same group, with the data transformations of
        # the output mapping
        .addObservedData(obsVsPred, currOutputMapping)
        obsVsPredList[[idx]] <- obsVsPred
      }
      rm(simulationResults)

      return(obsVsPredList)
    },

    # Simulated Values of Every Output Mapping
    #
    # Runs the simulations with the given parameter values and reads the
    # simulated values of every output mapping directly from the simulation
    # results as numeric vectors (see `.simulatedValues()`), without building
    # `DataCombined` objects.
    #
    # @param currVals Vector of parameter values for simulation.
    # @param outputMappings The output mappings of the evaluation (see
    #   `.getOutputMappings()`), by default those of the task.
    # @return A list with one entry per output mapping, each a list with
    #   `xValues`, the time values in min, and `yValues`, the values in the base
    #   unit of the mapped quantity.
    .simulateOutputs = function(
      currVals,
      outputMappings = private$.outputMappings
    ) {
      # The objective function reports a failed simulation itself on every
      # evaluation, so the warning of the simulation engine is not repeated
      simulationResults <- private$.runSimulations(currVals, silentMode = TRUE)

      # Time values are read once per simulation
      times <- list()
      simulated <- vector("list", length(outputMappings))
      for (idx in seq_along(outputMappings)) {
        simId <- outputMappings[[idx]]$simId
        resultObject <- simulationResults[[simId]][[1]]
        times[[simId]] <- times[[simId]] %||% .simulatedTimes(resultObject)
        simulated[[idx]] <- .simulatedValues(
          resultObject,
          outputMappings[[idx]]$quantity$path,
          times[[simId]]
        )
      }
      simulated
    },

    # Observed data of every output mapping, in base units and with the data
    # transformations applied (see `.prepareObservedData()`). They are read on
    # the first call after `.batchInitialization()` or a new bootstrap sample
    # and reused afterwards: reading the values of observed `DataSet` objects
    # from .NET is slow and retains memory on every read (#271).
    #
    # @param outputMappings The output mappings of the current evaluation.
    # @return A list with the prepared observed data of each output mapping.
    .getObservedData = function(outputMappings) {
      if (is.null(private$.observedData)) {
        private$.observedData <- lapply(outputMappings, .prepareObservedData)
      }
      private$.observedData
    },

    # Warns about the output mappings whose observed data have times without
    # simulated values because the output time points of the simulations were
    # set at an earlier call (see `.batchInitialization()` and
    # `.hasUnsimulatedObservedTimes()`). The cost of such a mapping is
    # infinite.
    #
    # @param simulatedList The simulated values of every output mapping (see
    #   `.simulateOutputs()`).
    # @param observedData The observed data of every output mapping (see
    #   `.getObservedData()`).
    # @param outputMappings The output mappings of the evaluation.
    .checkObservedTimes = function(
      simulatedList,
      observedData,
      outputMappings
    ) {
      costControl <- private$.configuration$objectiveFunctionOptions
      affected <- vapply(
        seq_along(outputMappings),
        function(idx) {
          costControl$scaling <- outputMappings[[idx]]$scaling
          .hasUnsimulatedObservedTimes(
            simulated = simulatedList[[idx]],
            observed = observedData[[idx]],
            costControl = costControl,
            outputTimePoints = private$.outputTimePoints[[
              outputMappings[[idx]]$simId
            ]]
          )
        },
        logical(1)
      )
      if (any(affected)) {
        warning(
          messages$warningObservedTimesNotSimulated(
            which(affected),
            vapply(
              outputMappings[affected],
              function(mapping) mapping$quantity$path,
              character(1)
            )
          ),
          call. = FALSE
        )
      }
    },

    # Retrieve Output Mappings with Optional Bootstrap Resampling
    #
    # Returns the list of output mappings used during objective function evaluation.
    # If no bootstrap seed is provided, the original mappings are returned.
    # If a `bootstrapSeed` is provided and differs from the previously active
    # seed, dataset weights and values are resampled accordingly.
    #
    # Strategy rationale:
    # Bootstrap behavior is implemented by modifying dataset weights and, for
    # aggregated data, replacing y-values with synthetic samples generated from
    # GPR models.
    #
    # This method uses lazy initialization and avoids redundant recomputation:
    # - The initial mapping state (weights and values) is extracted only once via
    #   `.extractOutputMappingState()`.
    # - GPR models are prepared for aggregated datasets only once via
    #   `.prepareGPRModels()`.
    # - On each new bootstrap seed, mappings are resampled using
    #   `.resampleAndApplyMappingState()`, updating the internal `.outputMappings`.
    #
    # @param bootstrapSeed Optional integer used for bootstrap resampling. If NULL,
    #   returns the unmodified output mappings.
    # @return A list of `PIOutputMapping` objects, possibly modified with resampled
    #   weights and values.
    .getOutputMappings = function(bootstrapSeed = NULL) {
      if (is.null(bootstrapSeed)) {
        return(private$.outputMappings)
      }

      # First-time bootstrap setup: cache initial state and fit GPR models
      if (is.null(private$.initialOutputMappingState)) {
        private$.initialOutputMappingState <- .extractOutputMappingState(
          private$.outputMappings
        )
      }

      # Trigger resampling only if the seed has changed
      if (
        is.null(private$.activeBootstrapSeed) ||
          private$.activeBootstrapSeed != bootstrapSeed
      ) {
        private$.activeBootstrapSeed <- bootstrapSeed
        private$.outputMappings <- .resampleAndApplyMappingState(
          private$.outputMappings,
          private$.initialOutputMappingState,
          private$.gprModels,
          bootstrapSeed
        )
        # Observed data changed, so they must be read again
        private$.observedData <- NULL
      }

      return(private$.outputMappings)
    },

    # Restore Output Mapping State
    #
    # Restores `outputMappings` to their original state before bootstrap
    # resampling, including both dataset weights and y-values. Clears
    # bootstrap-related state.
    .restoreOutputMappingsState = function() {
      if (!is.null(private$.initialOutputMappingState)) {
        private$.outputMappings <- .applyOutputMappingState(
          private$.outputMappings,
          private$.initialOutputMappingState
        )
      }
      private$.initialOutputMappingState <- NULL
      private$.activeBootstrapSeed <- NULL
      private$.gprModels <- NULL
      # Observed data restored to its original state, so read it again
      private$.observedData <- NULL
    },

    # Apply Identified Parameter Values
    #
    # Assigns the final optimized parameter values back to the respective
    # `PIParameters` objects within the simulation, aligning with their order in
    # the `$parameters` list.
    #
    # @param values Optimized parameter values to be applied.
    .applyFinalValues = function(values) {
      for (idx in seq_along(values)) {
        # The order of the values corresponds to the order of PIParameters in
        # `$parameters` list
        piParameter <- private$.piParameters[[idx]]
        piParameter$setValue(values[[idx]])
      }
    },

    # Execute Optimization Algorithm
    #
    # Runs the optimization algorithm defined in `PIConfiguration` using the
    # `Optimizer`. Uses current parameter bounds and start values, and evaluates
    # the objective function accordingly.
    #
    # @return Optimization results with parameter estimates, elapsed time, and
    #   additional metrics.
    .runAlgorithm = function() {
      startValues <- sapply(private$.piParameters, `[[`, "startValue")
      lower <- sapply(private$.piParameters, `[[`, "minValue")
      upper <- sapply(private$.piParameters, `[[`, "maxValue")

      optimizer <- Optimizer$new(configuration = private$.configuration)

      fn <- if (!is.null(private$.pkMappings)) {
        function(p, ...) private$.pkObjectiveFunction(p)
      } else {
        function(p, ...) private$.objectiveFunction(p, ...)
      }

      optimResult <- optimizer$run(
        par = startValues,
        fn = fn,
        lower = lower,
        upper = upper
      )

      optimResult$startValues <- startValues

      return(optimResult)
    },

    # Estimate Confidence Intervals
    #
    # The steps of `estimateCI()` after its checks. `run()` calls this method,
    # not `estimateCI()`.
    #
    # @param fromRun `TRUE` when `run()` estimates the confidence intervals
    #   after its optimization. The batches are initialized already then, and
    #   nothing can change between the optimization and the estimation, so the
    #   observed data that the optimization read are used again. Otherwise,
    #   the batches are initialized, which reads the observed data again (see
    #   `.batchInitialization()`).
    # @return A `PIResult` object with the confidence intervals.
    .estimateCI = function(fromRun = FALSE) {
      # Store simulation outputs and time intervals to reset them at the end
      # of the run.
      private$.savedSimulationState <- .storeSimulationState(
        private$.simulations
      )
      savedState <- private$.savedSimulationState
      on.exit(
        .restoreSimulationState(private$.simulations, savedState),
        add = TRUE
      )
      # Initialize batches
      if (!fromRun) {
        private$.batchInitialization()
      }
      # Reset function evaluations counter
      private$.fnEvaluations <- 0

      on.exit(private$.restoreOutputMappingsState(), add = TRUE)

      currValues <- sapply(private$.piParameters, `[[`, "currValue")
      lower <- sapply(private$.piParameters, `[[`, "minValue")
      upper <- sapply(private$.piParameters, `[[`, "maxValue")

      if (
        private$.configuration$ciMethod == "bootstrap" &&
          is.null(private$.activeBootstrapSeed)
      ) {
        .classifyObservedData(private$.outputMappings)
        private$.gprModels <- .prepareGPRModels(private$.outputMappings)
      }

      optimizer <- Optimizer$new(configuration = private$.configuration)

      fn <- function(p, ...) private$.objectiveFunction(p, ...)

      ciResult <- optimizer$estimateCI(
        par = currValues,
        fn = fn,
        lower = lower,
        upper = upper,
        resetFn = function() private$.fnEvaluations <- 0
      )

      PIResult$new(
        optimResult = private$.lastOptimResult,
        ciResult = ciResult,
        costDetails = private$.bestCostSummary %||% private$.lastCostSummary,
        configuration = private$.configuration,
        piParameters = private$.piParameters
      )
    }
  ),
  public = list(
    #' @description Initializes a `ParameterIdentification` instance.
    #'
    #' @param simulations A `Simulation` or list of `Simulation` objects to be
    #'   used for parameter estimation. Each simulation must contain the model
    #'   parameters specified in `parameters`. Use
    #'   [`ospsuite::loadSimulation()`] to load simulation files.
    #' @param parameters A `PIParameters` or list of `PIParameters` objects
    #'   specifying the model parameters to optimize. Each `PIParameters` object
    #'   may group one or more underlying model parameters, and its values are
    #'   converted from its `$unit` to the base unit before they are applied to
    #'   the model. See
    #'   [`ospsuite.parameteridentification::PIParameters`] for details.
    #' @param configuration (Optional) A `PIConfiguration` object specifying
    #'   algorithm, CI method, and objective function settings. Defaults to a
    #'   new configuration if omitted. See
    #'   [`ospsuite.parameteridentification::PIConfiguration`] for configuration
    #'   options.
    #' @param outputMappings (Optional) A `PIOutputMapping` or list of
    #'   `PIOutputMapping` objects mapping model outputs to observed data.
    #'   Mutually exclusive with `pkOutputMappings`.
    #' @param pkOutputMappings (Optional) A `PKOutputMapping` or list of
    #'   `PKOutputMapping` objects for PK metric optimization. Mutually
    #'   exclusive with `outputMappings`.
    #'
    #' @return A `ParameterIdentification` object ready to run parameter
    #'   estimation.
    initialize = function(
      simulations,
      parameters,
      outputMappings = NULL,
      pkOutputMappings = NULL,
      configuration = NULL
    ) {
      ospsuite.utils::validateIsOfType(simulations, "Simulation")
      ospsuite.utils::validateIsOfType(parameters, "PIParameters")
      ospsuite.utils::validateIsOfType(
        configuration,
        "PIConfiguration",
        nullAllowed = TRUE
      )

      hasPI <- !is.null(outputMappings)
      hasPK <- !is.null(pkOutputMappings)
      if (hasPI && hasPK) {
        stop(messages$errorPIMixedMappings())
      }
      if (!hasPI && !hasPK) {
        stop(messages$errorPINoMappings())
      }

      private$.configuration <- configuration %||% PIConfiguration$new()

      simulations <- ospsuite.utils::toList(simulations)
      parameters <- ospsuite.utils::toList(parameters)

      ids <- vector("list", length(simulations))
      private$.simulations <- vector("list", length(simulations))
      for (idx in seq_along(simulations)) {
        simulation <- simulations[[idx]]
        private$.simulations[[idx]] <- simulation
        ids[[idx]] <- simulation$root$id
      }
      names(private$.simulations) <- ids

      private$.piParameters <- parameters

      private$.variableMolecules <-
        private$.variableParameters <-
          private$.simulationBatches <-
            private$.steadyStateBatches <- vector("list", length(simulations))

      names(private$.variableMolecules) <-
        names(private$.variableParameters) <-
          names(private$.simulationBatches) <-
            names(private$.steadyStateBatches) <- ids

      if (hasPI) {
        outputMappings <- ospsuite.utils::toList(outputMappings)
        ospsuite.utils::validateIsOfType(outputMappings, "PIOutputMapping")
        .validateOutputMappingHasData(outputMappings)
        .validateSimulationIds(ids, parameters, outputMappings)
        private$.outputMappings <- outputMappings
      } else {
        pkOutputMappings <- ospsuite.utils::toList(pkOutputMappings)
        if (length(pkOutputMappings) == 0L) {
          stop(messages$errorPKMappingsEmpty())
        }
        if (
          any(vapply(pkOutputMappings, inherits, logical(1), "PIConfiguration"))
        ) {
          stop(messages$errorPKMappingsReceivedConfiguration())
        }
        ospsuite.utils::validateIsOfType(pkOutputMappings, "PKOutputMapping")
        mappingSimIds <- unique(sapply(pkOutputMappings, `[[`, "simId"))
        if (!all(mappingSimIds %in% unlist(ids))) {
          stop(messages$errorPKMappingSimulationMismatch())
        }
        private$.pkMappings <- pkOutputMappings
      }
    },

    #' Executes Parameter Identification
    #'
    #' @description Runs parameter identification using the configured
    #'   optimization algorithm. Returns a structured `piResults`object
    #'   containing estimated parameters, diagnostics, and (optionally)
    #'   confidence intervals.
    #'
    #' @return A [`PIResult`] object in standard mode, or a `PKResult` object
    #'   (internal) when `pkOutputMappings` was provided.
    run = function() {
      # Store simulation outputs and time intervals to reset them at the end
      # of the run.
      private$.savedSimulationState <- .storeSimulationState(
        private$.simulations
      )
      savedState <- private$.savedSimulationState
      on.exit(
        .restoreSimulationState(private$.simulations, savedState),
        add = TRUE
      )
      # Every time the user starts an optimization run, new batches should be
      # created, because `simulateSteadyState` flag can change and defines the
      # variables of the batches.
      private$.batchInitialization()
      # Clear previous optimization results and diagnostics
      private$.lastOptimResult <- NULL
      private$.bestCostSummary <- NULL
      private$.lastCostSummary <- NULL
      # Reset function evaluations counter
      private$.fnEvaluations <- 0
      # Reset gridSearchFlag
      private$.gridSearchFlag <- FALSE

      # Run optimization algorithm
      optimResult <- private$.runAlgorithm()
      private$.lastOptimResult <- optimResult

      achievedPKValues <- if (!is.null(private$.pkMappings)) {
        tryCatch(
          private$.getPKValues(optimResult$par),
          error = function(e) {
            warning(messages$warnAchievedPKValuesFailure(e$message))
            as.list(rep(NA_real_, length(private$.pkMappings)))
          }
        )
      } else {
        NULL
      }

      private$.applyFinalValues(values = optimResult$par)
      private$.needBatchInitialization <- FALSE

      if (!is.null(private$.pkMappings)) {
        piResult <- PKResult$new(
          optimResult = optimResult,
          piParameters = private$.piParameters,
          pkMappings = private$.pkMappings,
          achievedPKValues = achievedPKValues
        )
      } else if (private$.configuration$autoEstimateCI) {
        # The steps of `estimateCI()` after its checks, without a new batch
        # initialization, so that the observed data of the optimization are
        # used again. `estimateCI()` itself is not called, so an override of
        # it in a subclass does not change the confidence intervals of `run()`.
        piResult <- private$.estimateCI(fromRun = TRUE)
      } else {
        message(messages$statusAutoEstimateCI())
        piResult <- PIResult$new(
          optimResult = optimResult,
          ciResult = NULL,
          costDetails = private$.bestCostSummary %||% private$.lastCostSummary,
          configuration = private$.configuration,
          piParameters = private$.piParameters
        )
      }

      return(piResult)
    },

    #' Estimate Confidence Intervals
    #'
    #' @description Computes confidence intervals for the optimized parameters
    #'   using the method defined in the associated `PIConfiguration`. Intended
    #'   for advanced use when `autoEstimateCI` was set to `FALSE` during the
    #'   initial run.
    #'
    #' @return The same [`PIResult`] object returned by the `run()` method,
    #'   updated to include confidence interval estimates.
    estimateCI = function() {
      # Stop if executed before optimization
      if (is.null(private$.lastOptimResult)) {
        stop(messages$errorMissingOptimizationResult())
      }

      private$.assertNotPKMode("estimateCI")

      private$.estimateCI()
    },

    #' Plot Parameter Estimation Results
    #'
    #' @description Re-runs model simulations using the current or specified
    #'   parameter values and generates plots comparing predictions to observed
    #'   data.
    #'
    #' @param par Optional parameter values for simulations, in the order of
    #'   `ParameterIdentification$parameters`. Interpreted in each
    #'   `PIParameters$unit` and converted to the base unit before being applied
    #'   to the model. Use current values if `NULL`.
    #' @return A list of `patchwork` objects (one per output mapping), showing:
    #' - Individual time profiles
    #' - Predicted vs. observed values
    #' - Residuals vs. time
    plotResults = function(par = NULL) {
      private$.assertNotPKMode("plotResults")
      simulationState <- NULL
      # If the batches have not been initialized yet (i.e., no run has been
      # performed), this must be done prior to plotting
      private$.batchInitialization()

      # Run evaluate once. If the input argument is missing, run with current
      # values. Otherwise, use the supplied values
      parValues <- unlist(
        lapply(self$parameters, function(x) {
          x$currValue
        }),
        use.names = FALSE
      )
      if (!is.null(par)) {
        parValues <- par
      }
      dataCombined <- private$.evaluate(parValues)

      previousTheme <- ggplot2::theme_get()
      on.exit(ggplot2::theme_set(previousTheme), add = TRUE)
      ggplot2::theme_update(legend.title = ggplot2::element_blank())

      # ospsuite.plots sets explicit guide titles via guide_legend(title = ...),
      # which override theme(legend.title). Strip them with labs(NULL) so the
      # collected legend has no title, and drop redundant aesthetic legends so
      # the three sub-plots can be merged into a single legend.
      stripGuides <- ggplot2::labs(
        colour = NULL,
        fill = NULL,
        shape = NULL,
        linetype = NULL
      )

      multiPlot <- lapply(seq_along(dataCombined), function(idx) {
        scaling <- private$.outputMappings[[idx]]$scaling
        axisScale <- if (scaling == "lin") "linear" else "log"

        # The simulated line (linetype = name) and observed point (shape =
        # name) each carry their own clean, specifically-labelled legend guide.
        # Show those and hide the redundant colour/group guide (whose key mixes
        # line and point). Pin the two guides' order so the collected legend is
        # deterministic, which ggplot otherwise leaves unstable across runs.
        indivTimeProfile <- ospsuite::plotTimeProfile(
          dataCombined[[idx]],
          yScale = axisScale
        ) +
          stripGuides +
          ggplot2::guides(
            colour = "none",
            fill = "none",
            linetype = ggplot2::guide_legend(order = 1),
            shape = ggplot2::guide_legend(order = 2)
          )
        predVsObs <- ospsuite::plotPredictedVsObserved(
          dataCombined[[idx]],
          xyScale = axisScale
        ) +
          stripGuides +
          ggplot2::guides(
            colour = "none",
            fill = "none",
            shape = "none"
          )
        resVsTime <- ospsuite::plotResidualsVsCovariate(
          dataCombined[[idx]],
          xAxis = "time",
          residualScale = axisScale
        ) +
          stripGuides +
          ggplot2::guides(
            colour = "none",
            fill = "none",
            shape = "none",
            linetype = "none"
          )

        patchwork::wrap_plots(
          list(indivTimeProfile, predVsObs, resVsTime),
          ncol = 1
        ) +
          patchwork::plot_layout(guides = "collect") &
          ggplot2::theme(
            legend.position = "top",
            legend.title = ggplot2::element_blank(),
            legend.box = "vertical",
            legend.direction = "horizontal",
            legend.spacing.y = ggplot2::unit(0, "pt"),
            legend.margin = ggplot2::margin(0, 0, 0, 0),
            aspect.ratio = 0.4
          )
      })

      if (!is.null(private$.savedSimulationState)) {
        .restoreSimulationState(
          private$.simulations,
          private$.savedSimulationState
        )
      }

      return(multiPlot)
    },

    #' Perform a Parameter Grid Search
    #'
    #' Generates a grid of parameter combinations, computes the OFV for each,
    #' and optionally sets the best result as the starting point for s
    #' subsequent optimization.
    #'
    #' Note: The resulting grid can be used to explore the parameter space or
    #' initialize better starting values.
    #'
    #' @param lower Numeric vector of parameter lower bounds, defaulting to
    #'   `PIParameters` minimum values. Interpreted in each `PIParameters$unit`.
    #' @param upper Numeric vector of parameter upper bounds, defaulting to
    #'   `PIParameters` maximum values. Interpreted in each `PIParameters$unit`.
    #' @param logScaleFlag Logical scalar or vector; determines if grid points
    #'   are spaced logarithmically. Default is `FALSE`.
    #' @param totalEvaluations Integer specifying the total grid points. Default
    #'   is 50.
    #' @param setStartValue Logical. If `TRUE`, updates `PIParameters` starting
    #'   values to the best grid point. Default is `FALSE`.
    #'
    #' @return A tibble where each row is a parameter combination and the
    #'   corresponding objective function value (`ofv`).
    gridSearch = function(
      lower = NULL,
      upper = NULL,
      logScaleFlag = FALSE,
      totalEvaluations = 50,
      setStartValue = FALSE
    ) {
      ospsuite.utils::validateIsNumeric(lower, nullAllowed = TRUE)
      ospsuite.utils::validateIsNumeric(upper, nullAllowed = TRUE)
      ospsuite.utils::validateIsLogical(logScaleFlag)
      ospsuite.utils::validateIsNumeric(totalEvaluations)
      ospsuite.utils::validateIsLogical(setStartValue)

      private$.assertNotPKMode("gridSearch")
      private$.gridSearchFlag <- TRUE
      private$.batchInitialization()

      nrOfParameters <- length(private$.piParameters)

      # Expand and validate logScaleFlag
      if (length(logScaleFlag) == 1) {
        logScaleFlag <- rep(logScaleFlag, length.out = nrOfParameters)
      }
      ospsuite.utils::validateIsOfLength(logScaleFlag, nrOfParameters)

      # Initialize bounds for parameters
      if (is.null(lower)) {
        lower <- sapply(private$.piParameters, function(x) x$minValue)
      }
      if (is.null(upper)) {
        upper <- sapply(private$.piParameters, function(x) x$maxValue)
      }
      ospsuite.utils::validateIsOfLength(lower, nrOfParameters)
      ospsuite.utils::validateIsOfLength(upper, nrOfParameters)

      # Create parameter grid
      gridSize <- floor(totalEvaluations^(1 / nrOfParameters))
      parameterGrid <- vector("list", length(private$.piParameters))
      names(parameterGrid) <- sapply(
        private$.piParameters,
        function(x) x$parameters[[1]]$path
      )

      for (idx in seq_along(private$.piParameters)) {
        if (logScaleFlag[idx]) {
          if (lower[idx] <= 0 | upper[idx] <= 0) {
            stop(messages$logScaleFlagError())
          }
          # Logarithmic scaling
          parameterGrid[[idx]] <- exp(seq(
            log(lower[idx]),
            log(upper[idx]),
            length.out = gridSize
          ))
        } else {
          # Linear scaling
          parameterGrid[[idx]] <- seq(
            lower[idx],
            upper[idx],
            length.out = gridSize
          )
        }
      }

      ofvGrid <- expand.grid(parameterGrid)

      # Calculate OFV
      ofvGrid[["ofv"]] <- vapply(
        1:nrow(ofvGrid),
        function(i) {
          ofv <- private$.objectiveFunction(as.numeric(ofvGrid[i, ]))
          ofv[[private$.configuration$modelCostField]]
        },
        numeric(1)
      )

      # Restore simulation state if applicable
      if (!is.null(private$.savedSimulationState)) {
        .restoreSimulationState(
          private$.simulations,
          private$.savedSimulationState
        )
      }

      # Set starting point for next round of optimization
      if (setStartValue) {
        bestPoint <- ofvGrid[which.min(ofvGrid[["ofv"]]), ]
        bestValues <- bestPoint[setdiff(names(bestPoint), "ofv")]
        for (idx in seq_along(private$.piParameters)) {
          private$.piParameters[[idx]]$startValue <- bestValues[[idx]]
        }
        message(messages$gridSearchParameterValueSet(bestValues))
      }

      return(tibble::as_tibble(ofvGrid))
    },

    #' Calculate Objective Function Value (OFV) Profiles
    #'
    #' @description
    #' Generates OFV profiles by varying each `PIParameters` independently while
    #' holding the others fixed at `par`. Useful as a post-optimization
    #' diagnostic: around a (local) minimum the OFV is expected to be roughly
    #' convex along each axis.
    #'
    #' @details
    #' For each parameter `i` a one-dimensional grid of `totalEvaluations`
    #' equally spaced points is built between `lower[i]` and `upper[i]`, with
    #' the bounds derived from `par[i]` and `boundFactor`:
    #'
    #' - `par[i] >= 0`: `lower[i] = (1 - boundFactor) * par[i]`,
    #'   `upper[i] = (1 + boundFactor) * par[i]`.
    #' - `par[i] <  0`: bounds are mirrored so that `lower < upper` is preserved.
    #'
    #' The objective function is evaluated along each axis with all other
    #' parameters held at their `par` values. Failed simulations contribute
    #' `Inf` to the corresponding `ofv` cell.
    #'
    #' @param par Numeric vector of parameter values, one for each
    #'   `PIParameters`, interpreted in each `PIParameters$unit`. Defaults to
    #'   current parameter values if `NULL`, not numeric, or of mismatched
    #'   length.
    #' @param boundFactor Numeric scalar. A value of `0.1` (default) means
    #'   bounds extend ±10% around `par` for each parameter.
    #' @param totalEvaluations Integer specifying the number of grid points
    #'   per parameter profile. Default is `20`.
    #'
    #' @return A named list of tibbles, one element per `PIParameters`. List
    #'   names are the parameter paths (taken from `parameters[[1]]$path`).
    #'   Each tibble has two columns:
    #'   - a column named after the parameter path, holding the grid values;
    #'   - `ofv`, holding the matching objective function values.
    #'
    #'   Pass the returned list to `plotOFVProfiles()` to visualize the
    #'   profiles.
    #'
    #' @examples
    #' # piTask is a configured ParameterIdentification instance
    #' # Default: +/-10% around current values, 20 grid points per parameter
    #' # ofvProfiles <- piTask$calculateOFVProfiles()
    #'
    #' # Wider neighborhood, finer grid
    #' # ofvProfiles <- piTask$calculateOFVProfiles(
    #' #   boundFactor = 0.5,
    #' #   totalEvaluations = 50
    #' # )
    #'
    #' # plotOFVProfiles(ofvProfiles)[[1]]
    calculateOFVProfiles = function(
      par = NULL,
      boundFactor = 0.1,
      totalEvaluations = 20
    ) {
      ospsuite.utils::validateIsNumeric(par, nullAllowed = TRUE)
      ospsuite.utils::validateIsNumeric(boundFactor)
      ospsuite.utils::validateIsInteger(totalEvaluations)

      private$.assertNotPKMode("calculateOFVProfiles")

      # Store simulation outputs and time intervals to reset them at the end.
      private$.savedSimulationState <- .storeSimulationState(
        private$.simulations
      )

      private$.gridSearchFlag <- TRUE
      private$.batchInitialization()

      nrOfParameters <- length(private$.piParameters)

      # Set parameter values and bounds
      if (is.null(par) || length(par) != nrOfParameters || !is.numeric(par)) {
        par <- sapply(private$.piParameters, function(x) x$currValue)
      }
      lower <- ifelse(par < 0, (1 + boundFactor) * par, (1 - boundFactor) * par)
      upper <- ifelse(par < 0, (1 - boundFactor) * par, (1 + boundFactor) * par)

      # Default grid (one row per evaluation, one column per parameter).
      # Indexed by position throughout to stay safe when parameter paths
      # collide (e.g. same path in two simulations).
      parameterNames <- sapply(
        private$.piParameters,
        function(x) x$parameters[[1]]$path
      )
      defaultGrid <- matrix(
        par,
        nrow = totalEvaluations,
        ncol = nrOfParameters,
        byrow = TRUE
      )

      # Calculate OFV profile with parameter-specific grid
      profileList <- vector(mode = "list", length = nrOfParameters)
      for (idx in seq_along(private$.piParameters)) {
        parameterName <- parameterNames[idx]

        # Generate and update grid for the current parameter
        grid <- seq(lower[[idx]], upper[[idx]], length.out = totalEvaluations)
        currentGrid <- defaultGrid
        currentGrid[, idx] <- grid

        # Calculate OFV for each grid row
        ofvValues <- numeric(totalEvaluations)
        for (gridIdx in seq_len(totalEvaluations)) {
          ofv <- private$.objectiveFunction(currentGrid[gridIdx, ])
          ofvValues[gridIdx] <- ofv[[private$.configuration$modelCostField]]
        }

        profileList[[idx]] <- tibble::tibble(
          !!parameterName := grid,
          ofv = ofvValues
        )
      }

      names(profileList) <- parameterNames

      # Restore simulation state if applicable
      if (!is.null(private$.savedSimulationState)) {
        .restoreSimulationState(
          private$.simulations,
          private$.savedSimulationState
        )
      }

      return(profileList)
    },

    #' @description Prints a summary of `ParameterIdentification` instance.
    print = function() {
      ospsuite.utils::ospPrintClass(self)
      if (!is.null(private$.pkMappings)) {
        ospsuite.utils::ospPrintItems(list(
          "Number of parameters" = length(private$.piParameters),
          "Number of PK output mappings" = length(private$.pkMappings)
        ))
      } else {
        ospsuite.utils::ospPrintItems(list(
          "Number of parameters" = length(private$.piParameters)
        ))
      }
      ospsuite.utils::ospPrintItems(
        unlist(
          lapply(private$.simulations, function(x) x$sourceFile),
          use.names = FALSE
        ),
        title = "Simulations"
      )
    }
  )
)
