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
    # Coefficient of variation for M3 objective function, assumed 20% for linear scale.
    .linScaleCV = 0.2,
    # Standard deviation for log-transformed data, pre-calculated for an assumed
    # CV of 20% for M3 objective function.
    # https://medcraveonline.com/MOJPB/correct-use-of-percent-coefficient-of-variation-cv-formula-for-log-transformed-data.html
    .logScaleSD = NULL,

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
          simId <- .getSimulationContainer(outputMapping$quantity)$id
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
      # Increase function evaluations counter
      private$.fnEvaluations <- private$.fnEvaluations + 1
      # List of DataCombined objects, one for each output mapping
      # If the simulation was not successful, return `Inf` for the objective function value.
      obsVsPredList <- tryCatch(
        {
          private$.evaluate(currVals)
        },
        error = function(cond) {
          message(messages$simulationNotSuccessful(currVals))
          message("Original error message:")
          message(cond$message)

          NA
        }
      )
      # Return an infinite cost structure if the simulation is NA
      failureResponse <- .handleSimulationFailure(obsVsPredList)
      if (!is.null(failureResponse)) {
        return(failureResponse)
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
        costSummary <- calculateCostMetrics(
          df = obsVsPredDf,
          objectiveFunctionType = private$.configuration$objectiveFunctionOptions$objectiveFunctionType,
          residualWeightingMethod = private$.configuration$objectiveFunctionOptions$residualWeightingMethod,
          robustMethod = private$.configuration$objectiveFunctionOptions$robustMethod,
          scaleVar = private$.configuration$objectiveFunctionOptions$scaleVar,
          scaling = private$.outputMappings[[idx]]$scaling,
          linScaleCV = private$.linScaleCV,
          logScaleSD = private$.logScaleSD
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
        # $parameters list
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
              # If the parameter values are close to their bounds, the hessian
              # should be calculated with a smaller epsilon than a default value
              # of 1e-4
              hessianEpsilon <- min(1e-4, 0.1 * abs(results$par - lower), 0.1 * abs(results$par - upper))
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
    #' For creating `Simulation` objects, see \code{\link[ospsuite]{loadSimulation}}.
    #' @param parameters An object or a list of objects of class `PIParameter`.
    #' These parameters will be varied. For creating `PIParameter` objects, refer to
    #' \code{\link[ospsuite.parameteridentification]{PIParameters}}.
    #' @param configuration (Optional) `PIConfiguration` for additional settings.
    #' Uses default if omitted. For details on creating a `PIConfiguration` object,
    #' see \code{\link[ospsuite.parameteridentification]{PIConfiguration}}.
    #' @param outputMappings List of objects of the class `PIOutputMapping`. Each
    #' object maps a model output (represented by a `Quantity`) with a set of
    #' observed data given as `XYData` objects. For guidance on creating `PIOutputMapping`
    #' objects, see \code{\link[ospsuite.parameteridentification]{PIOutputMapping}}.
    #' @returns A new `ParameterIdentification` object.
    initialize = function(simulations, parameters, outputMappings, configuration = NULL) {
      ospsuite.utils::validateIsOfType(simulations, "Simulation")
      ospsuite.utils::validateIsOfType(parameters, "PIParameters")
      ospsuite.utils::validateIsOfType(configuration, "PIConfiguration", nullAllowed = TRUE)
      ospsuite.utils::validateIsOfType(outputMappings, "PIOutputMapping")
      private$.configuration <- configuration %||% PIConfiguration$new()
      private$.logScaleSD <- sqrt(log(1 + private$.linScaleCV^2, base = 10) / log(10))

      simulations <- ospsuite.utils::toList(simulations)
      parameters <- ospsuite.utils::toList(parameters)
      outputMappings <- ospsuite.utils::toList(outputMappings)

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
      # Run optimization algorithm
      # Reset function evaluations counter
      private$.fnEvaluations <- 0
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

    ## Commented out until https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/92 is fixed
    # #' @description
    # #' Calculates the values of the objective function on an n-dimensional grid, where n is the number
    # #' of parameters, and optionally saves the best result as the starting point for next optimization runs.
    # #' @param lower A vector of lower bounds for parameters, with the same length as the number of parameters
    # #' optimized in this parameter identification task. By default, uses the minimal values
    # #' defined in the `PIParameter` objects.
    # #' @param upper A vector of upper bounds for parameters, with the same length as the number of parameters
    # #' optimized in this parameter identification task. By default, uses the maximal values
    # #' defined in the `PIParameter` objects.
    # #' @param logScaleFlag A single logical value or a vector of logical values indicating
    # #' if grid should be evenly spaced on a linear or a logarithmic scale. Defaults to `FALSE`.
    # #' @param totalEvaluations An integer number. The grid will have as many points so that the
    # #' total number of grid points does not exceed `totalEvaluations`. Defaults to `50`.
    # #' @param margin Can be set to a non-zero positive value so that the edges of the grid will be away
    # #' from the exact parameter bounds.
    # #' @param setStartingPoint (logical) If `TRUE`, the best result will be saved as the starting point for
    # #' the next optimization runs. Defaults to `FALSE`.
    # #' @return A tibble with one column for each parameter and one column for the objective function value.
    # #' The tibble will have at most `totalEvaluations` rows.
    # gridSearch = function(lower = NULL, upper = NULL, logScaleFlag = FALSE, totalEvaluations = 50, margin = 0, setStartingPoint = FALSE) {
    #   # If the batches have not been initialized yet (i.e., no run has been
    #   # performed), this must be done prior to plotting
    #   private$.batchInitialization()
    #
    #   nrOfParameters <- length(private$.piParameters)
    #   # logScaleFlag can be specified as a single value (common for all parameters)
    #   # or as a vector of values (one for each parameter)
    #   if (length(logScaleFlag) == 1) {
    #     logScaleFlag <- rep(logScaleFlag, length.out = nrOfParameters)
    #   }
    #   # This will catch the cases where logScaleFlag is a vector longer than 1,
    #   # but does not match the number of parameters
    #   ospsuite.utils::isSameLength(logScaleFlag, private$.piParameters)
    #
    #   # if lower and upper are not supplied, we reuse parameter bounds
    #   # By default, margin = 0, but we can use a non-zero value
    #   # so that the grid does not start exactly at the parameter bound
    #   if (missing(lower)) {
    #     lower <- vector(mode = "list", length = nrOfParameters)
    #     for (idx in seq_along(private$.piParameters)) {
    #       lower[[idx]] <- private$.piParameters[[idx]]$minValue + margin
    #     }
    #   }
    #   if (missing(upper)) {
    #     upper <- vector(mode = "list", length = nrOfParameters)
    #     for (idx in seq_along(private$.piParameters)) {
    #       upper[[idx]] <- private$.piParameters[[idx]]$maxValue - margin
    #     }
    #   }
    #   ospsuite.utils::isSameLength(lower, private$.piParameters)
    #   ospsuite.utils::isSameLength(upper, private$.piParameters)
    #
    #   gridSize <- floor(totalEvaluations^(1 / nrOfParameters))
    #   gridList <- vector(mode = "list", length = nrOfParameters)
    #   for (idx in seq_along(private$.piParameters)) {
    #     if (logScaleFlag[[idx]]) {
    #       grid <- exp(seq(from = log(lower[[idx]]), to = log(upper[[idx]]), length.out = gridSize))
    #     } else {
    #       grid <- seq(from = lower[[idx]], to = upper[[idx]], length.out = gridSize)
    #     }
    #     gridList[[idx]] <- grid
    #     # creating unique column names for the grid
    #     names(gridList)[[idx]] <- paste0("par", idx, ": ", private$.piParameters[[idx]]$parameters[[1]]$path)
    #   }
    #
    #   OFVGrid <- expand.grid(gridList)
    #   # all columns from the OFVGrid are passed in the same order to the objective function
    #   OFVGrid[["ofv"]] <- purrr::pmap_dbl(OFVGrid, function(...) {
    #     private$.targetFunction(c(...))$model
    #   })
    #
    #   if (!is.null(private$.savedSimulationState)) {
    #     .restoreSimulationState(private$.simulations, private$.savedSimulationState)
    #   }
    #
    #   if (setStartingPoint) {
    #     bestPoint <- OFVGrid[which.min(OFVGrid[["ofv"]]), ]
    #     for (idx in seq_along(private$.piParameters)) {
    #       private$.piParameters[[idx]]$startValue <- bestPoint[[idx]]
    #     }
    #     message(messages$gridSearchParameterValueSet())
    #   }
    #
    #   return(tibble::as_tibble(OFVGrid))
    # },

    # #' @description
    # #' Calculates the values of the objective function on all orthogonal lines
    # #' passing through a given point in the parameter space.
    # #' @param par A vector of parameter values, with the same length as the number of parameters.
    # #' If not supplied, the current parameter values are used.
    # #' @param lower A vector of lower bounds for parameters, with the same length as the number of parameters.
    # #' By default, uses 0.9 of the current parameter value.
    # #' @param upper A vector of upper bounds for parameters, with the same length as the number of parameters.
    # #' By default, uses 1.1 of the current parameter value.
    # #' @param totalEvaluations An integer number. The combined profiles will not contain more than `totalEvaluations`
    # #' points. If not supplied, 21 points per parameter are plotted to cover a uniform grid from 0.9 to 1.1.
    # #' @return A list of tibbles, one tibble per parameter, with one column for parameter values
    # #' and one column for the matching objective function values.
    # calculateOFVProfiles = function(par = NULL, lower = NULL, upper = NULL, totalEvaluations = NULL) {
    #   # If the batches have not been initialized yet (i.e., no run has been
    #   # performed), this must be done prior to plotting
    #   private$.batchInitialization()
    #
    #   nrOfParameters <- length(private$.piParameters)
    #
    #   # if par is not supplied, we use the current parameter values
    #   if (missing(par)) {
    #     par <- unlist(lapply(private$.piParameters, function(x) {
    #       x$currValue
    #     }), use.names = FALSE)
    #   }
    #
    #   # if lower and upper are not supplied, we calculate them as 0.9 and 1.1
    #   # of the current parameter values
    #   if (missing(lower)) {
    #     lower <- 0.9 * par
    #   }
    #   if (missing(upper)) {
    #     upper <- 1.1 * par
    #   }
    #
    #   # calculate the grid for each parameter separately
    #   if (missing(totalEvaluations)) {
    #     gridSize <- 21
    #     # creates a grid with values at 0.9, 0.91, 0.92 .. 1.0 .. 1.09, 1.1
    #     # of the current parameter values
    #   } else {
    #     gridSize <- floor(totalEvaluations / nrOfParameters)
    #   }
    #   gridList <- vector(mode = "list", length = nrOfParameters)
    #   for (idx in seq_along(private$.piParameters)) {
    #     gridList[[idx]] <- rep(par[[idx]], gridSize)
    #     names(gridList)[[idx]] <- private$.piParameters[[idx]]$parameters[[1]]$path
    #   }
    #   defaultGrid <- tibble::as_tibble(gridList)
    #
    #   profileList <- vector(mode = "list", length = nrOfParameters)
    #   for (idx in seq_along(private$.piParameters)) {
    #     # the names of the parameters are extracted from the first available path
    #     parameterName <- private$.piParameters[[idx]]$parameters[[1]]$path
    #     grid <- seq(from = lower[[idx]], to = upper[[idx]], length.out = gridSize)
    #     currentGrid <- defaultGrid
    #     currentGrid[[parameterName]] <- grid
    #     # creates a tibble with the column name from the `parameterName` variable
    #     profileList[[idx]] <- tibble::tibble(!!parameterName := grid)
    #     profileList[[idx]][["ofv"]] <- purrr::pmap_dbl(currentGrid, function(...) {
    #       private$.objectiveFunction(c(...))$modelCost
    #     })
    #     names(profileList)[[idx]] <- parameterName
    #   }
    #
    #   if (!is.null(private$.savedSimulationState)) {
    #     .restoreSimulationState(private$.simulations, private$.savedSimulationState)
    #   }
    #
    #   return(profileList)
    # },

    #' @description Prints a summary of ParameterIdentification instance.
    print = function() {
      private$printClass()
      private$printLine("Simulations", unlist(lapply(private$.simulations, function(x) {
        x$sourceFile
      }), use.names = FALSE))
      private$printLine("Number of parameters", length(private$.piParameters))
      # private$printLine("Simulate to steady-state", private$.configuration$simulateSteadyState)
      # private$printLine("Steady-state time [min]", private$.configuration$steadyStateTime)
      private$printLine("Print feedback after each function evaluation", private$.configuration$printEvaluationFeedback)
      invisible(self)
    }
  )
)
