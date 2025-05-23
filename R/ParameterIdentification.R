#' @title ParameterIdentification
#' @docType class
#' @description Executes optimization to find the best parameter values by comparing
#' simulation results against observed data.
#' @import R6 ospsuite.utils
#' @export
#' @format NULL
ParameterIdentification <- R6::R6Class(
  "ParameterIdentification",
  cloneable = FALSE,
  active = list(
    #' @field simulations A named list of simulation objects, keyed by the IDs
    #' of their root containers.
    simulations = function(value) {
      if (missing(value)) {
        as.list(private$.simulations)
      } else {
        stop(messages$errorPropertyReadOnly("simulations"))
      }
    },

    #' @field parameters Read-only list of `PIParameters` objects for optimization.
    parameters = function(value) {
      if (missing(value)) {
        private$.piParameters
      } else {
        stop(messages$errorPropertyReadOnly("parameters"))
      }
    },

    #' @field configuration A `PIConfiguration` object instance.
    configuration = function(value) {
      if (missing(value)) {
        private$.configuration
      } else {
        ospsuite.utils::validateIsOfType(value, "PIConfiguration")
        private$.configuration <- value
      }
    },

    #' @field outputMappings A list of `PIOutputMapping` objects linking
    #' observed data to simulation outputs.
    outputMappings = function(value) {
      if (missing(value)) {
        private$.outputMappings
      } else {
        stop(messages$errorPropertyReadOnly("outputMappings"))
      }
    }
  ),
  private = list(
    # Named list of simulations keyed by root container IDs
    .simulations = NULL,
    # Batches for result calculations, named by root container IDs
    .simulationBatches = NULL,
    # For steady state calculations, with different outputs and times, named by root container IDs
    .steadyStateBatches = NULL,
    # Named list by simulation IDs, with paths and start values for variable molecules
    .variableMolecules = NULL,
    # Named list by simulation IDs, detailing paths and start values for variable parameters
    .variableParameters = NULL,
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
    # Number of function evaluations
    .fnEvaluations = 0,
    # Flag to indicate if objective function is being called from grid search
    .gridSearchFlag = FALSE,
    # Most recently used bootstrap seed to detect when resampling is needed
    .activeBootstrapSeed = NULL,
    # Cached original weights and values of all datasets before bootstrap
    .initialOutputMappingState = NULL,
    # Fitted GPR models for aggregated datasets, used during bootstrap resampling
    .gprModels = NULL,

    # Batch Initialization for Simulations
    #
    # Initializes simulation batches, preparing them for parameter
    # identification by clearing outputs, updating schemas with observed data, and setting
    # variable parameters. Optimizes repeated calls by checking initialization necessity.
    .batchInitialization = function() {
      # If the flag is already set to FALSE, short-cuts the execution of the function
      # This way, the function call be called repeatedly with minimal overhead
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

        # Clear output intervals and output quantities of all simulations
        for (simulation in private$.simulations) {
          ospsuite::clearOutputIntervals(simulation)
          ospsuite::clearOutputs(simulation)
        }

        # Add time points to the output schema that are present in the observed data.
        # Also add output quantities.
        for (outputMapping in private$.outputMappings) {
          # ID of the and the parent simulation of the quantity of the mapping.
          simId <- outputMapping$simId
          simulation <- private$.simulations[[simId]]
          # Add the quantity to the outputs of the simulations.
          ospsuite::addOutputs(quantitiesOrPaths = outputMapping$quantity, simulation = simulation)
          # Add time points present in the observed data of this mapping.
          for (dataset in outputMapping$observedDataSets) {
            # Time values can be stored in units different from the base unit
            # and must be converted to the base unit first.
            label <- dataset$name
            xFactor <- outputMapping$dataTransformations$xFactors
            if (length(xFactor) != 1) {
              xFactor <- xFactor[[label]]
            }
            xOffset <- outputMapping$dataTransformations$xOffsets
            if (length(xOffset) != 1) {
              xOffset <- xOffset[[label]]
            }
            xVals <- ospsuite::toBaseUnit(ospsuite::ospDimensions$Time,
              values = (dataset$xValues + xOffset) * xFactor,
              unit = dataset$xUnit
            )
            simulation$outputSchema$addTimePoints(xVals)
          }
        }

        # Add parameters that will be optimized to the list of variable parameters
        for (piParameter in private$.piParameters) {
          for (parameter in piParameter$parameters) {
            simId <- .getSimulationContainer(parameter)$id
            # Set the current value of this parameter to the start value of the
            # PIParameter.
            private$.variableParameters[[simId]][[parameter$path]] <- piParameter$startValue
          }
        }

        # Create simulation batches for identification runs
        for (simulation in private$.simulations) {
          simId <- simulation$root$id
          # Parameters and molecules defined in the previous steps will be variable.
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
      if (is.null(private$.activeBootstrapSeed) ||
        private$.activeBootstrapSeed != bootstrapSeed) {
        private$.activeBootstrapSeed <- bootstrapSeed
        private$.outputMappings <- .resampleAndApplyMappingState(
          private$.outputMappings,
          private$.initialOutputMappingState,
          private$.gprModels,
          bootstrapSeed
        )
      }

      return(private$.outputMappings)
    },

    # Restore Output Mapping State
    #
    # Restores `outputMappings` to their original state before bootstrap resampling,
    # including both dataset weights and y-values. Clears bootstrap-related state.
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
    },

    # Aggregate Model Cost Calculation
    #
    # Calculates and aggregates the model cost across all output
    # mappings for parameter estimation. Adjusts the evaluations counter, processes
    # each output mapping's cost via `.calculateCostMetrics`, and aggregates the
    # results into total cost summary.
    # @param currVals Vector of parameter values for simulation.
    # @return Aggregated total cost summary.
    .objectiveFunction = function(currVals, bootstrapSeed = NULL) {
      # Increment function evaluations counter
      private$.fnEvaluations <- private$.fnEvaluations + 1

      outputMappings <- private$.getOutputMappings(bootstrapSeed)

      # Run simulation and catch errors
      obsVsPredList <- tryCatch(
        private$.evaluate(currVals, bootstrapSeed = bootstrapSeed),
        error = function(cond) {
          messages$logSimulationError(currVals, cond)
          return(NA)
        }
      )

      # Handle simulation failure
      if (anyNA(obsVsPredList)) {
        if (private$.fnEvaluations == 1 && !private$.gridSearchFlag) {
          stop(messages$initialSimulationError())
        } else {
          message(messages$simulationError())
          return(.createErrorCostStructure(infinite = TRUE))
        }
      }

      if (length(obsVsPredList) != length(outputMappings)) {
        stop(messages$errorObsVsPredListLengthMismatch(
          length(outputMappings), length(obsVsPredList)
        ))
      }

      # Evaluate cost per output mapping
      costSummaryList <- vector("list", length(outputMappings))
      for (idx in seq_along(outputMappings)) {
        # Convert units to base units for unified comparison
        obsVsPredDf <- ospsuite:::.unitConverter(obsVsPredList[[idx]]$toDataFrame())
        # Apply LLOQ handling for LSQ
        if (private$.configuration$objectiveFunctionOptions$objectiveFunctionType == "lsq") {
          # replace values < LLOQ with LLOQ/2 in simulated data
          if (sum(is.finite(obsVsPredDf$lloq)) > 0) {
            lloq <- min(obsVsPredDf$lloq, na.rm = TRUE)
            obsVsPredDf[(obsVsPredDf$dataType == "simulated" &
              obsVsPredDf$yValues < lloq), "yValues"] <- lloq / 2
          }
        }

        # Apply log transformation if requested
        if (outputMappings[[idx]]$scaling == "log") {
          obsVsPredDf <- .applyLogTransformation(obsVsPredDf)
        }

        # Assign weights from PIOutputMapping
        obsVsPredDf$weights <- NA_real_
        if (!is.null(outputMappings[[idx]]$dataWeights)) {
          weights <- outputMappings[[idx]]$dataWeights
          for (dataset in names(weights)) {
            obsVsPredDf$weights[obsVsPredDf$name == dataset] <- weights[[dataset]]
          }
        }

        # Extract cost function options
        costControl <- private$.configuration$objectiveFunctionOptions
        costControl$scaling <- outputMappings[[idx]]$scaling
        ospsuite.utils::validateIsOption(
          options = costControl,
          validOptions = ObjectiveFunctionSpecs
        )

        # Compute cost for current output mapping
        costSummary <- .calculateCostMetrics(
          df = obsVsPredDf,
          objectiveFunctionType = costControl$objectiveFunctionType,
          residualWeightingMethod = costControl$residualWeightingMethod,
          robustMethod = costControl$robustMethod,
          scaleVar = costControl$scaleVar,
          linScaleCV = costControl$linScaleCV,
          logScaleSD = costControl$logScaleSD,
          scaling = costControl$scaling
        )

        costSummary$residualDetails$index <- idx
        costSummaryList[[idx]] <- costSummary
      }

      # Aggregate cost across all output mappings
      runningCost <- Reduce(.summarizeCostLists, costSummaryList)

      #  Optionally print evaluation feedback
      if (private$.configuration$printEvaluationFeedback) {
        cat(
          messages$evaluationFeedback(
            private$.fnEvaluations, currVals,
            runningCost[[private$.configuration$modelCostField]]
          )
        )
      }

      return(runningCost)
    },

    # Apply Identified Parameter Values
    #
    # Assigns the final optimized parameter values back to the respective
    # `PIParameters` objects within the simulation, aligning with their order
    # in the `$parameters` list.
    #
    # @param values Optimized parameter values to be applied.
    .applyFinalValues = function(values) {
      # Iterate through PIParameters

      # THE LOGIC WOULD PROBABLY DEPEND ON THE TYPE OF RESULTS OUTPUT RETURNED
      # BY THE SELECTED METHOD, UNLESS WE USE OUR OWN CONSISTENT RESULTS STRUCTURE
      for (idx in seq_along(values)) {
        # The order of the values corresponds to the order of PIParameters in
        # $parameters list
        piParameter <- private$.piParameters[[idx]]
        piParameter$setValue(values[[idx]])
      }
    },

    # Simulation Evaluation with Parameter Values
    #
    # Evaluates simulations using specified parameter values, updating each
    # parameter before simulation runs. Generates `DataCombined` objects for
    # each output mapping, encapsulating both simulated and observed data.
    #
    # @param currVals Vector of parameter values for simulation.
    # @return List of `DataCombined` objects, one per output mapping.
    .evaluate = function(currVals, bootstrapSeed = NULL) {
      outputMappings <- private$.getOutputMappings(bootstrapSeed)

      obsVsPredList <- vector("list", length(outputMappings))
      # Iterate through the values and update current parameter values
      for (idx in seq_along(currVals)) {
        # The order of the values corresponds to the order of PIParameters in
        # parameters list
        piParameter <- private$.piParameters[[idx]]
        # Update the values of the parameters
        for (parameter in piParameter$parameters) {
          simId <- .getSimulationContainer(parameter)$id
          private$.variableParameters[[simId]][[parameter$path]] <- currVals[[idx]]
        }
      }

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
        simBatch <- private$.simulationBatches[[simId]]
        resultsId <- simBatch$addRunValues(
          parameterValues = unlist(private$.variableParameters[[simId]], use.names = FALSE),
          initialValues = unlist(private$.variableMolecules[[simId]], use.names = FALSE)
        )
      }
      # Run simulation batches
      simulationResults <- ospsuite::runSimulationBatches(
        simulationBatches = private$.simulationBatches,
        simulationRunOptions = private$.configuration$simulationRunOptions
      )

      for (idx in seq_along(outputMappings)) {
        obsVsPred <- ospsuite::DataCombined$new()
        currOutputMapping <- outputMappings[[idx]]
        # Find the simulation that is the parent of the output quantity
        simId <- .getSimulationContainer(currOutputMapping$quantity)$id
        # Find the simulation batch that corresponds to the simulation
        simBatch <- private$.simulationBatches[[simId]]
        # Construct group names out of output path and simulation id
        groupName <- currOutputMapping$quantity$path
        # In each iteration, only one values set per simulation batch is simulated.
        # Therefore we always need the first results entry
        resultObject <- simulationResults[[simBatch$id]][[1]]
        resultId <- names(simulationResults[[simBatch$id]])[[1]]
        obsVsPred$addSimulationResults(resultObject,
          quantitiesOrPaths = currOutputMapping$quantity$path,
          names = resultId, groups = groupName
        )

        obsVsPred$addDataSets(currOutputMapping$observedDataSets, groups = groupName)
        # apply data transformations stored in corresponding outputMapping
        obsVsPred$setDataTransformations(
          forNames = names(outputMappings[[idx]]$observedDataSets),
          xOffsets = outputMappings[[idx]]$dataTransformations$xOffsets,
          xScaleFactors = outputMappings[[idx]]$dataTransformations$xFactors,
          yOffsets = outputMappings[[idx]]$dataTransformations$yOffsets,
          yScaleFactors = outputMappings[[idx]]$dataTransformations$yFactors
        )
        obsVsPredList[[idx]] <- obsVsPred
      }

      return(obsVsPredList)
    },

    # Execute Optimization Algorithm
    #
    # Runs the optimization algorithm defined in `PIConfiguration` using the
    # `Optimizer`. Uses current parameter bounds and start values, and evaluates
    # the objective function accordingly.
    #
    # @return Optimization results with parameter estimates, elapsed time, and
    # additional metrics.
    .runAlgorithm = function() {
      startValues <- sapply(private$.piParameters, `[[`, "startValue")
      lower <- sapply(private$.piParameters, `[[`, "minValue")
      upper <- sapply(private$.piParameters, `[[`, "maxValue")

      optimizer <- Optimizer$new(configuration = private$.configuration)

      optimResult <- optimizer$run(
        par = startValues,
        fn = function(p, ...) private$.objectiveFunction(p, ...),
        lower = lower,
        upper = upper
      )

      return(optimResult)
    }
  ),
  public = list(
    #' @description Initializes a `ParameterIdentification` instance.
    #'
    #' @param simulations An object or a list of objects of class `Simulation`.
    #' Parameters of the simulation object will be varied and the results simulated.
    #' For creating `Simulation` objects, see [`ospsuite::loadSimulation`].
    #' @param parameters An object or a list of objects of class `PIParameters`.
    #' These parameters will be varied. For creating `PIParameters` objects, refer to
    #' [`ospsuite.parameteridentification::PIParameters`].
    #' @param configuration (Optional) `PIConfiguration` for additional settings.
    #' Uses default if omitted. For details on creating a `PIConfiguration` object,
    #' see [`ospsuite.parameteridentification::PIConfiguration`].
    #' @param outputMappings List of objects of the class `PIOutputMapping`. Each
    #' object maps a model output (represented by a `Quantity`) with a set of
    #' observed data given as `XYData` objects. For guidance on creating `PIOutputMapping`
    #' objects, see [`ospsuite.parameteridentification::PIOutputMapping`].
    #' @return A new `ParameterIdentification` object.
    initialize = function(simulations, parameters, outputMappings, configuration = NULL) {
      ospsuite.utils::validateIsOfType(simulations, "Simulation")
      ospsuite.utils::validateIsOfType(parameters, "PIParameters")
      ospsuite.utils::validateIsOfType(outputMappings, "PIOutputMapping")
      ospsuite.utils::validateIsOfType(configuration, "PIConfiguration", nullAllowed = TRUE)

      private$.configuration <- configuration %||% PIConfiguration$new()

      simulations <- ospsuite.utils::toList(simulations)
      parameters <- ospsuite.utils::toList(parameters)
      outputMappings <- ospsuite.utils::toList(outputMappings)

      .validateOutputMappingHasData(outputMappings)

      # We have to use the id of the root container of the simulation instead of
      # the id of the simulation itself, because later we have to find the
      # simulations based on the id of the root when assigning quantities to
      # simulations
      ids <- vector("list", length(simulations))
      private$.simulations <- vector("list", length(simulations))
      for (idx in seq_along(simulations)) {
        simulation <- simulations[[idx]]
        private$.simulations[[idx]] <- simulation
        ids[[idx]] <- simulation$root$id
      }

      .validateSimulationIds(ids, parameters, outputMappings)

      names(private$.simulations) <- ids
      private$.piParameters <- parameters
      private$.outputMappings <- c(outputMappings)

      private$.variableMolecules <-
        private$.variableParameters <-
        private$.simulationBatches <-
        private$.steadyStateBatches <- vector("list", length(simulations))

      names(private$.variableMolecules) <-
        names(private$.variableParameters) <-
        names(private$.simulationBatches) <-
        names(private$.steadyStateBatches) <- ids
    },

    #' Executes Parameter Identification
    #'
    #' @description Initiates parameter identification process.
    #' Upon completion, access optimal parameter values through `PIParameters$currValue`.
    #'
    #' @return Results from the parameter identification algorithm, varying by
    #' algorithm choice.
    run = function() {
      # Store simulation outputs and time intervals to reset them at the end
      # of the run.
      private$.savedSimulationState <- .storeSimulationState(private$.simulations)
      # Every time the user starts an optimization run, new batches should be
      # created, because `simulateSteadyState` flag can change and defines the
      # variables of the batches.
      private$.batchInitialization()
      # Reset function evaluations counter
      private$.fnEvaluations <- 0
      # Reset gridSearchFlag
      private$.gridSearchFlag <- FALSE

      # Run optimization algorithm
      results <- private$.runAlgorithm()
      # Reset simulation output intervals and output selections
      .restoreSimulationState(private$.simulations, private$.savedSimulationState)

      # Apply identified values to the parameter objects. Should be an option?
      private$.applyFinalValues(values = results$par)
      # Since the batches have been initialized already, this will not be
      # required before plotting current results
      private$.needBatchInitialization <- FALSE

      # Trigger .NET gc
      ospsuite::clearMemory()

      return(results)
    },

    #' Estimates Confidence Intervals
    #'
    #' @description Computes confidence intervals for the optimized parameters
    #' using the method specified in `PIConfiguration`.
    #'
    #' @return A list with confidence interval results, including bounds, standard
    #' errors, and coefficient of variation.
    estimateCI = function() {
      # Store simulation outputs and time intervals to reset them at the end
      # of the run.
      private$.savedSimulationState <- .storeSimulationState(private$.simulations)
      # Initialize batches
      private$.batchInitialization()

      on.exit(private$.restoreOutputMappingsState(), add = TRUE)

      currValues <- sapply(private$.piParameters, `[[`, "currValue")
      lower <- sapply(private$.piParameters, `[[`, "minValue")
      upper <- sapply(private$.piParameters, `[[`, "maxValue")

      if (private$.configuration$ciMethod == "bootstrap" &&
        is.null(private$.activeBootstrapSeed)) {
        .classifyObservedData(private$.outputMappings)
        private$.gprModels <- .prepareGPRModels(private$.outputMappings)
      }

      optimizer <- Optimizer$new(configuration = private$.configuration)

      ciResult <- optimizer$estimateCI(
        par = currValues,
        fn = function(p, ...) private$.objectiveFunction(p, ...),
        lower = lower,
        upper = upper
      )

      if (!is.null(private$.savedSimulationState)) {
        .restoreSimulationState(private$.simulations, private$.savedSimulationState)
      }

      # Trigger .NET gc
      ospsuite::clearMemory()

      return(ciResult)
    },

    #' Plots Parameter Estimation Results
    #'
    #' @description Generates plots for each output mapping based on the current
    #' or provided parameter values. Simulations are executed with these parameters
    #' to visualize the estimation results.
    #'
    #' @param par Optional parameter values for simulations, in the order of
    #' `ParameterIdentification$parameters`. Use current values if `NULL`.
    #' @return A list of `ggplot2` plots, one for each `PIOutputMapping`.
    plotResults = function(par = NULL) {
      simulationState <- NULL
      # If the batches have not been initialized yet (i.e., no run has been
      # performed), this must be done prior to plotting
      private$.batchInitialization()

      # Run evaluate once. If the input argument is missing, run with current values.
      # Otherwise, use the supplied values.
      parValues <- unlist(lapply(self$parameters, function(x) {
        x$currValue
      }), use.names = FALSE)
      if (!is.null(par)) {
        parValues <- par
      }
      dataCombined <- private$.evaluate(parValues)

      # Create figures and plot
      plotConfiguration <- ospsuite::DefaultPlotConfiguration$new()
      multiPlot <- lapply(seq_along(dataCombined), function(idx) {
        scaling <- private$.outputMappings[[idx]]$scaling
        plotConfiguration$yAxisScale <- scaling
        plotConfiguration$legendPosition <- NULL
        indivTimeProfile <- ospsuite::plotIndividualTimeProfile(dataCombined[[idx]], plotConfiguration)
        plotConfiguration$legendPosition <- "none"
        plotConfiguration$xAxisScale <- scaling
        obsVsSim <- ospsuite::plotObservedVsSimulated(dataCombined[[idx]], plotConfiguration)
        plotConfiguration$xAxisScale <- "lin"
        plotConfiguration$yAxisScale <- "lin"
        resVsTime <- ospsuite::plotResidualsVsTime(dataCombined[[idx]], plotConfiguration)
        plotGridConfiguration <- ospsuite::PlotGridConfiguration$new()
        plotGridConfiguration$addPlots(list(indivTimeProfile, obsVsSim, resVsTime))
        return(ospsuite::plotGrid(plotGridConfiguration))
      })

      if (!is.null(private$.savedSimulationState)) {
        .restoreSimulationState(private$.simulations, private$.savedSimulationState)
      }

      return(multiPlot)
    },

    #' Calculate Objective Function Values (OFV) on an n-Dimensional Grid
    #'
    #' Generates a grid of parameter combinations, computes the OFV for each,
    #' and optionally sets the best result as the starting point for subsequent
    #' optimizations.
    #'
    #' @param lower Numeric vector of parameter lower bounds, defaulting to
    #' `PIParameter` minimum values.
    #' @param upper Numeric vector of parameter upper bounds, defaulting to
    #' `PIParameter` maximum values.
    #' @param logScaleFlag Logical scalar or vector; determines if grid points
    #' are spaced logarithmically. Default is `FALSE`.
    #' @param totalEvaluations Integer specifying the total grid points. Default
    #' is 50.
    #' @param setStartValue Logical. If `TRUE`, updates `PIParameter` starting
    #' values to the best grid point. Default is `FALSE`.
    #'
    #' @return A tibble with parameter values and their corresponding OFV.
    gridSearch = function(lower = NULL, upper = NULL, logScaleFlag = FALSE,
                          totalEvaluations = 50, setStartValue = FALSE) {
      ospsuite.utils::validateIsNumeric(lower, nullAllowed = TRUE)
      ospsuite.utils::validateIsNumeric(upper, nullAllowed = TRUE)
      ospsuite.utils::validateIsLogical(logScaleFlag)
      ospsuite.utils::validateIsNumeric(totalEvaluations)
      ospsuite.utils::validateIsLogical(setStartValue)

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
        private$.piParameters, function(x) x$parameters[[1]]$path
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
            lower[idx], upper[idx],
            length.out = gridSize
          )
        }
      }

      ofvGrid <- expand.grid(parameterGrid)

      # Calculate OFV
      ofvGrid[["ofv"]] <- vapply(1:nrow(ofvGrid), function(i) {
        ofv <- private$.objectiveFunction(as.numeric(ofvGrid[i, ]))
        ofv[[private$.configuration$modelCostField]]
      }, numeric(1))

      # Restore simulation state if applicable
      if (!is.null(private$.savedSimulationState)) {
        .restoreSimulationState(
          private$.simulations, private$.savedSimulationState
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
    #' Generates OFV profiles by varying each parameter independently while
    #' holding others constant.
    #'
    #' @param par Numeric vector of parameter values, one for each parameter.
    #' Defaults to current parameter values if `NULL`, invalid or mismatched.
    #' @param boundFactor Numeric value. A value of 0.1 means `lower` is 10% below
    #' `par` and `upper` is 10% above `par`. Default is `0.1`.
    #' @param totalEvaluations Integer specifying the total number of grid
    #' points across each parameter profile. Default is 20.
    #'
    #' @return A list of tibbles, one per parameter, with columns for parameter
    #' values and OFVs (`ofv`).
    calculateOFVProfiles = function(par = NULL, boundFactor = 0.1, totalEvaluations = 20) {
      ospsuite.utils::validateIsNumeric(par, nullAllowed = TRUE)
      ospsuite.utils::validateIsNumeric(boundFactor)
      ospsuite.utils::validateIsInteger(totalEvaluations)

      private$.gridSearchFlag <- TRUE
      private$.batchInitialization()

      nrOfParameters <- length(private$.piParameters)

      # Set parameter values and bounds
      if (is.null(par) || length(par) != nrOfParameters || !is.numeric(par)) {
        par <- sapply(private$.piParameters, function(x) x$currValue)
      }
      lower <- ifelse(par < 0, (1 + boundFactor) * par, (1 - boundFactor) * par)
      upper <- ifelse(par < 0, (1 - boundFactor) * par, (1 + boundFactor) * par)

      # Create default grid with constant values for each parameter
      parameterNames <- sapply(
        private$.piParameters, function(x) x$parameters[[1]]$path
      )
      defaultGrid <- matrix(par,
        nrow = totalEvaluations, ncol = nrOfParameters,
        byrow = TRUE
      )
      colnames(defaultGrid) <- parameterNames
      defaultGrid <- tibble::as_tibble(defaultGrid)

      # Calculate ORF profile with parameter-specific grid
      profileList <- vector(mode = "list", length = nrOfParameters)
      for (idx in seq_along(private$.piParameters)) {
        parameterName <- parameterNames[idx]

        # Generate and update grid for the current parameter
        grid <- seq(lower[[idx]], upper[[idx]], length.out = totalEvaluations)
        currentGrid <- defaultGrid
        currentGrid[[parameterName]] <- grid

        # Calculate OFV for each grid row
        ofvValues <- numeric(totalEvaluations)
        for (gridIdx in seq_len(totalEvaluations)) {
          row <- as.numeric(currentGrid[gridIdx, ])
          ofv <- private$.objectiveFunction(row)
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
          private$.simulations, private$.savedSimulationState
        )
      }

      return(profileList)
    },

    #' @description Prints a summary of ParameterIdentification instance.
    print = function() {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "Number of parameters" = length(private$.piParameters)
      ))
      ospsuite.utils::ospPrintItems(unlist(lapply(private$.simulations, function(x) {
        x$sourceFile
      }), use.names = FALSE), title = "Simulations")
      # private$printLine("Simulate to steady-state", private$.configuration$simulateSteadyState)
      # private$printLine("Steady-state time [min]", private$.configuration$steadyStateTime)
    }
  )
)
