#' @title ParameterIdentification
#' @docType class
#' @description Executes optimization to find the best parameter values by comparing
#' simulation results against observed data.
#' @import R6 ospsuite.utils
#' @export
#' @format NULL
ParameterIdentification <- R6::R6Class(
  "ParameterIdentification",
  inherit = ospsuite.utils::Printable,
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

    # Aggregate Model Cost Calculation
    #
    # Calculates and aggregates the model cost across all output
    # mappings for parameter estimation. Adjusts the evaluations counter, processes
    # each output mapping's cost via `calculateCostMetrics`, and aggregates the
    # results into total cost summary.
    # @param currVals Vector of parameter values for simulation.
    # @return Aggregated total cost summary.
    .objectiveFunction = function(currVals) {
      # Increment function evaluations counter
      private$.fnEvaluations <- private$.fnEvaluations + 1

      # Evaluate simulation and handle errors
      obsVsPredList <- tryCatch(
        private$.evaluate(currVals),
        error = function(cond) {
          messages$logSimulationError(currVals, cond)
          return(NA)
        }
      )

      # Check for simulation failure
      if (anyNA(obsVsPredList)) {
        if (private$.fnEvaluations == 1 && !private$.gridSearchFlag) {
          stop(messages$initialSimulationError())
        } else {
          message(messages$simulationError())
          return(.createErrorCostStructure(infinite = TRUE))
        }
      }

      # Calculate error for each output mapping separately
      costSummaryList <- vector("list", length(private$.outputMappings))
      for (idx in seq_along(private$.outputMappings)) {
        # Calling unit converter to unify the units within the DataCombined
        obsVsPredDf <- ospsuite:::.unitConverter(obsVsPredList[[idx]]$toDataFrame())
        # For least squares objective function, values below LLOQ will be set to
        # LLOQ/2 for simulation data and therefore always equal to the observed values
        if (private$.configuration$objectiveFunctionOptions$objectiveFunctionType == "lsq") {
          # replacing values below LLOQ with LLOQ / 2
          if (sum(is.finite(obsVsPredDf$lloq)) > 0) {
            lloq <- min(obsVsPredDf$lloq, na.rm = TRUE)
            obsVsPredDf[(obsVsPredDf$dataType == "simulated" &
              obsVsPredDf$yValues < lloq), "yValues"] <- lloq / 2
          }
        }

        # Transform to log if required.
        if (private$.outputMappings[[idx]]$scaling == "log") {
          obsVsPredDf <- .applyLogTransformation(obsVsPredDf)
        }

        # Calculate model costs
        costControl <- private$.configuration$objectiveFunctionOptions
        costControl$scaling <- private$.outputMappings[[idx]]$scaling
        ospsuite.utils::validateIsOption(
          options = costControl,
          validOptions = ObjectiveFunctionSpecs
        )
        costSummary <- calculateCostMetrics(
          df = obsVsPredDf,
          objectiveFunctionType = costControl$objectiveFunctionType,
          residualWeightingMethod = costControl$residualWeightingMethod,
          robustMethod = costControl$robustMethod,
          scaleVar = costControl$scaleVar,
          linScaleCV = costControl$linScaleCV,
          logScaleSD = costControl$logScaleSD,
          scaling = costControl$scaling
        )

        costSummaryList[[idx]] <- costSummary
      }
      # Summed up over all output mappings
      runningCost <- Reduce(.summarizeCostLists, costSummaryList)

      # Print current error if requested
      if (private$.configuration$printEvaluationFeedback) {
        cat(paste0(
          "fneval ", private$.fnEvaluations, ": parameters ", paste0(signif(currVals, 3), collapse = "; "),
          ", objective function ", signif(runningCost$modelCost, 4), "\n"
        ))
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
    .evaluate = function(currVals) {
      obsVsPredList <- vector("list", length(private$.outputMappings))
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

      for (idx in seq_along(private$.outputMappings)) {
        obsVsPred <- ospsuite::DataCombined$new()
        currOutputMapping <- private$.outputMappings[[idx]]
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
          forNames = names(private$.outputMappings[[idx]]$observedDataSets),
          xOffsets = private$.outputMappings[[idx]]$dataTransformations$xOffsets,
          xScaleFactors = private$.outputMappings[[idx]]$dataTransformations$xFactors,
          yOffsets = private$.outputMappings[[idx]]$dataTransformations$yOffsets,
          yScaleFactors = private$.outputMappings[[idx]]$dataTransformations$yFactors
        )
        obsVsPredList[[idx]] <- obsVsPred
      }

      return(obsVsPredList)
    },

    # Execute Optimization Algorithm
    #
    # Executes the selected optimization algorithm, configured in `PIConfiguration`,
    # on the current parameter set. It returns a comprehensive summary of the
    # optimization results, including parameter estimates, function evaluations,
    # and computation time.
    #
    # @return Optimization results with parameter estimates, elapsed time, and
    # additional metrics.
    .runAlgorithm = function() {
      startValues <- unlist(lapply(self$parameters, function(x) {
        x$startValue
      }), use.names = FALSE)
      lower <- unlist(lapply(self$parameters, function(x) {
        x$minValue
      }), use.names = FALSE)
      upper <- unlist(lapply(self$parameters, function(x) {
        x$maxValue
      }), use.names = FALSE)

      # Depending on the `algorithm` argument in the `PIConfiguration` object, the
      # actual optimization call will use one of the underlying optimization routines
      message(messages$runningOptimizationAlgorithm(private$.configuration$algorithm))

      control <- private$.configuration$algorithmOptions
      if (private$.configuration$algorithm == "HJKB") {
        # Default options
        if (is.null(control)) {
          control <- AlgorithmOptions_HJKB
        }
        time <- system.time({
          results <- dfoptim::hjkb(par = startValues, fn = function(p) {
            private$.objectiveFunction(p)$modelCost
          }, control = control, lower = lower, upper = upper)
        })
      } else if (private$.configuration$algorithm == "BOBYQA") {
        # Default options
        if (is.null(control)) {
          control <- AlgorithmOptions_BOBYQA
        }
        time <- system.time({
          results <- nloptr::bobyqa(x0 = startValues, fn = function(p) {
            private$.objectiveFunction(p)$modelCost
          }, control = control, lower = lower, upper = upper)
        })
      } else if (private$.configuration$algorithm == "DEoptim") {
        # Default options
        if (is.null(control)) {
          control <- AlgorithmOptions_DEoptim
        }
        # passing control arguments by name into the DEoptim.control object, using DEoptim default values where needed
        time <- system.time({
          results <- DEoptim::DEoptim(fn = function(p) {
            private$.objectiveFunction(p)$modelCost
          }, lower = lower, upper = upper, control = control)
          results$par <- results$optim$bestmem
          results$value <- results$optim$bestval
        })
      }
      # at this point, we assume that `private$.configuration$algorithm` contains
      # one of the entries from `ospsuite.parameteridentification::Algorithms`,
      # so one of the `if` conditions above would execute

      results$elapsed <- time[[3]]
      results$algorithm <- private$.configuration$algorithm
      # Add the number of function evaluations (excluding hessian calculation) to the results output
      results$nrOfFnEvaluations <- private$.fnEvaluations

      # Calculate sigma if it has not been calculated previously
      if (is.null(results$sigma)) {
        # For the objective function that represents the deviation = -2 * log(L),
        # results$hessian / 2 is the observed information matrix
        # https://stats.stackexchange.com/questions/27033/
        results$sigma <- tryCatch(
          {
            # Calculate hessian if the selected algorithm does not calculate it by default
            if (is.null(results$hessian)) {
              message(messages$hessianEstimation())
              # The hessian estimation is based on the parameter values
              hessianEpsilon <- pmax(1e-8, pmin(1e-4, 0.1 * abs(results$par)))
              results$hessian <- numDeriv::hessian(func = function(p) {
                private$.objectiveFunction(p)$modelCost
              }, x = results$par, method.args = list(eps = hessianEpsilon))
            }

            fim <- solve(results$hessian / 2)
            sqrt(diag(fim))
          },
          error = function(cond) {
            message("Error calculating confidence intervals.")
            message("Here's the original error message:")
            message(cond$message)
            # Choose a return value in case of error
            NA_real_
          }
        )
      }
      # The 95% confidence intervals are defined by two sigma values away from the
      # point estimate. The coefficient of variation (CV) is the ratio of standard
      # deviation to the point estimate.
      results$lwr <- results$par - qnorm(p = 1 - 0.05 / 2) * results$sigma
      results$upr <- results$par + qnorm(p = 1 - 0.05 / 2) * results$sigma
      results$cv <- results$sigma / abs(results$par) * 100
      return(results)
    }
  ),
  public = list(
    #' @description Initializes a `ParameterIdentification` instance.
    #'
    #' @param simulations An object or a list of objects of class `Simulation`.
    #' Parameters of the simulation object will be varied and the results simulated.
    #' For creating `Simulation` objects, see [`ospsuite::loadSimulation`].
    #' @param parameters An object or a list of objects of class `PIParameter`.
    #' These parameters will be varied. For creating `PIParameter` objects, refer to
    #' [`ospsuite.parameteridentification::PIParameters`].
    #' @param configuration (Optional) `PIConfiguration` for additional settings.
    #' Uses default if omitted. For details on creating a `PIConfiguration` object,
    #' see [`ospsuite.parameteridentification::PIConfiguration`].
    #' @param outputMappings List of objects of the class `PIOutputMapping`. Each
    #' object maps a model output (represented by a `Quantity`) with a set of
    #' observed data given as `XYData` objects. For guidance on creating `PIOutputMapping`
    #' objects, see [`ospsuite.parameteridentification::PIOutputMapping`].
    #' @returns A new `ParameterIdentification` object.
    initialize = function(simulations, parameters, outputMappings, configuration = NULL) {
      ospsuite.utils::validateIsOfType(simulations, "Simulation")
      ospsuite.utils::validateIsOfType(parameters, "PIParameters")
      ospsuite.utils::validateIsOfType(configuration, "PIConfiguration", nullAllowed = TRUE)
      ospsuite.utils::validateIsOfType(outputMappings, "PIOutputMapping")
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
      # Reset gridSearhcFlag
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
        private$.objectiveFunction(as.numeric(ofvGrid[i, ]))$modelCost
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
          ofvValues[gridIdx] <- private$.objectiveFunction(row)$modelCost
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
