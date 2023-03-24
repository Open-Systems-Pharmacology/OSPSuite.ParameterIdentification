#' @title ParameterIdentification
#' @docType class
#' @description A task to identify optimal parameter values based on simulation
#'   outputs and observed data
#' @import FME ospsuite.utils GenSA
#' @format NULL
#' @export
ParameterIdentification <- R6::R6Class(
  "ParameterIdentification",
  inherit = ospsuite.utils::Printable,
  cloneable = FALSE,
  active = list(
    #' @field simulations Named list with simulation objects, where names are
    #'   IDs of the root container of the simulation
    simulations = function(value) {
      if (missing(value)) {
        as.list(private$.simulations)
      } else {
        stop(messages$errorPropertyReadOnly("simulations"))
      }
    },

    #' @field parameters List of `PIParameters` objects to be optimized. Read-only
    parameters = function(value) {
      if (missing(value)) {
        private$.piParameters
      } else {
        stop(messages$errorPropertyReadOnly("parameters"))
      }
    },

    #' @field configuration An object of `PIConfiguration`
    configuration = function(value) {
      if (missing(value)) {
        private$.configuration
      } else {
        validateIsOfType(value, "PIConfiguration")
        private$.configuration <- value
      }
    },

    #' @field outputMappings List of `PIOutputMapping` objects. Each
    #' mapping assigns a set of observed data to a simulation output
    outputMappings = function(value) {
      if (missing(value)) {
        private$.outputMappings
      } else {
        stop(messages$errorPropertyReadOnly("outputMappings"))
      }
    }
  ),
  private = list(
    # Named list of simulations, with names being the IDs of the root container
    .simulations = NULL,
    # Simulation batches for calculation of results. Names are the IDs of the
    # root container of the underlying simulations.
    .simulationBatches = NULL,
    # Separate batches for calculation of steady state. Required as they have
    # other output paths and simulation time that actual simulations. Names are
    # the IDs of the root container of the underlying simulations.
    .steadyStateBatches = NULL,
    # A named list with names being the IDs of the root container of the
    # simulations. Each entry is a named list, with names being the paths of the
    # variable molecules/parameters, and values being the start values.
    .variableMolecules = NULL,
    .variableParameters = NULL,
    # List of PIParameter objects defining the simulation parameters to be
    # optimized
    .piParameters = NULL,
    # List of PIOutputMapping objects
    .outputMappings = NULL,
    # PIConfiguration
    .configuration = NULL,
    # Flag if simulation batches must be created from simulations. Used for
    # plotting current results.
    .needBatchInitialization = TRUE,
    .iteration = 0,

    # Creates simulation batches from simulations.
    .batchInitialization = function() {
      # Prepare simulations
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
        clearOutputIntervals(simulation)
        clearOutputs(simulation)
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
        simBatch <- createSimulationBatch(
          simulation = simulation,
          parametersOrPaths = names(private$.variableParameters[[simId]]),
          moleculesOrPaths = names(private$.variableMolecules[[simId]])
        )
        private$.simulationBatches[[simId]] <- simBatch
      }

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

    # Calculate the target function that is going to be minimized during
    # parameter estimation. Currently, only an FME-based implementation of
    # sum of squared residuals is supported. This should be extended with
    # M3, MLE and Bayesian posterior functions
    .targetFunction = function(currVals) {
      obsVsPredList <- private$.evaluate(currVals)

      if (tolower(private$.configuration$targetFunctionType) == "lsq") {
        runningCost <- NULL
        # matching output mappings to the results of the .evaluate function
        for (idx in seq_along(private$.outputMappings)) {
          obsVsPredDf <- ospsuite:::.unitConverter(obsVsPredList[[idx]]$toDataFrame())
          simulated <- obsVsPredDf[obsVsPredDf$dataType == "simulated", ]
          observed <- obsVsPredDf[obsVsPredDf$dataType == "observed", ]
          # replacing values below LLOQ with LLOQ / 2
          # get LLOQ of observed

          lloq <- min(observed$lloq, na.rm = TRUE)
          # If no lloq values are specified, min will return Inf.
          if (is.finite(lloq)) {
            simulated[simulated$yValues < lloq, "yValues"] <- lloq / 2
          }
          if (private$.outputMappings[[idx]]$scaling == "lin") {
            modelDf <- data.frame("Time" = simulated$xValues, "Values" = simulated$yValues)
            obsDf <- data.frame("Time" = observed$xValues, "Values" = observed$yValues)
          } else if (private$.outputMappings[[idx]]$scaling == "log") {
            UNITS_EPSILON <- ospsuite::toUnit(
              quantityOrDimension = simulated$yDimension[[1]],
              values = ospsuite::getOSPSuiteSetting("LOG_SAFE_EPSILON"),
              targetUnit = simulated$yUnit[[1]],
              molWeight = 1
            )
            modelDf <- data.frame("Time" = simulated$xValues, "Values" = ospsuite.utils::logSafe(simulated$yValues, epsilon = UNITS_EPSILON, base = exp(1)))
            obsDf <- data.frame("Time" = observed$xValues, "Values" = ospsuite.utils::logSafe(observed$yValues, epsilon = UNITS_EPSILON, base = exp(1)))
          }
          runningCost <- FME::modCost(model = modelDf, obs = obsDf, x = "Time", cost = runningCost)
          runningError <- runningCost$model
        }
        if (private$.configuration$printIterationFeedback) {
          private$.iteration <- private$.iteration + 1
          cat(paste0("iter ", private$.iteration, ": parameters ", paste0(signif(currVals, 3), collapse = "; "), ", target function ", signif(runningError, 3), "\n"))
        }
        return(runningCost)
      }

      # If targetFunctionType did not match any of the implementations, return NA
      warning(paste0(private$.configuration$targetFunctionType, " is not an implemented target function. Cannot run parameter identification."))
      return(NA_real_)
    },

    # Apply final identified values to simulation parameter objects.
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

    #' Evaluate all simulations with given parameter values
    #' @param currVals Numerical vector of the parameter values to be applied
    #' @return An list of objects of `DataCombined` class that includes values simulated
    #' with the given parameters, and corresponding datasets with observed data.
    #' Returns one `DataCombined` object for each output mapping.
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
      #   ssResults <- runSimulationBatches(simulationBatches = private$.steadyStateBatches,
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
      simulationResults <- runSimulationBatches(
        simulationBatches = private$.simulationBatches,
        simulationRunOptions = private$.configuration$simulationRunOptions
      )

      for (idx in seq_along(private$.outputMappings)) {
        obsVsPred <- DataCombined$new()
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

    # Calculate the least-squares measure of discrepancy between observed and predicted data
    .LSQ = function(obsVsPred) {
      residuals <- calculateResiduals(obsVsPred, scaling = "lin")
      return(sum(residuals$residualValues**2, na.rm = TRUE))
    },

    # Runs the optimization algorithm and returns the results produced by the
    # algorithms. Called from public $run() method.
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

      # Quadratic approximation based algorithms (BOBYQA) may end up in a local minimum
      # instead of the global minimum. To find the approximate location of the global
      # minimum, generalized simulated annealing is performed first.
      # It is intended to run for 10 seconds, but in practice, it runs for one iteration
      # of simulated annealing (that takes more than 10 seconds). Visiting and acceptance
      # parameters are set to fast simulated annealing (see doi.org/10.32614/RJ-2013-002).
      SAresults <- GenSA::GenSA(par = startValues, fn = function(p) {
        private$.targetFunction(p)$model
      }, lower = lower, upper = upper, control = list(max.time = 10, verbose = FALSE, simple.function = TRUE, visiting.param = 2, acceptance.param = 1))
      results <- FME::modFit(f = private$.targetFunction, p = SAresults$par, lower = lower, upper = upper, method = "bobyqa")
      results$GenSAcounts <- SAresults$counts
      # Calculation of confidence intervals
      # Sigma values are standard deviations of the estimated parameters. They are
      # extracted from the estimated hessian matrix through the summary function.
      # The 95% confidence intervals are defined by two sigma values away from the
      # point estimate. The coefficient of variation (CV) is the ratio of standard
      # deviation to the point estimate.
      sigma <- as.numeric(summary(results)[["par"]][, "Std. Error"])
      results$lwr <- results$par - 1.96 * sigma
      results$upr <- results$par + 1.96 * sigma
      results$cv <- sigma / results$par * 100
      return(results)
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #'
    #' @param simulations An object or a list of objects of class `Simulation`.
    #' Parameters of the simulation object will be varied and the results simulated
    #' @param parameters An object or a list of objects of class `PIParameter`. These parameters will be varied.
    #' @param configuration Optional. Object of type `PIConfiguration` defining
    #' further options of the parameter identification. If no `PIConfiguration` is passed, a default one
    #' @param outputMappings List of objects of the class `PIOutputMapping`. Each objects
    #' maps a model output (represented by a `Quantity`) with a set of observed data given as `XYData` objects.
    #' is used.
    #' @return A new `ParameterIdentification` object.
    initialize = function(simulations, parameters, outputMappings, configuration = NULL) {
      ospsuite.utils::validateIsOfType(simulations, "Simulation")
      ospsuite.utils::validateIsOfType(parameters, "PIParameters")
      ospsuite.utils::validateIsOfType(configuration, "PIConfiguration", nullAllowed = TRUE)
      ospsuite.utils::validateIsOfType(outputMappings, "PIOutputMapping")
      private$.configuration <- configuration %||% PIConfiguration$new()

      simulations <- toList(simulations)
      parameters <- toList(parameters)
      outputMappings <- toList(outputMappings)

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

    #' @description
    #' Start identification of parameters
    #' @details When the identification if finished, the best identified values of the parameters are accessible via the `currValue`-field of the `PIParameters`-object.
    #'
    #' @return Output of the PI algorithm. Depends on the selected algorithm.
    run = function() {
      # Store simulation outputs and time intervals to reset them at the end
      # of the run.
      simulationState <- .storeSimulationState(private$.simulations)

      # Every time the user starts an optimization run, new batches should be
      # created, because `simulateSteadyState` flag can change and defines the
      # variables of the batches.
      private$.batchInitialization()
      # Run optimization algorithm
      # Reset iteration counter
      private$.iteration <- 0
      results <- private$.runAlgorithm()
      # Reset simulation output intervals and output selections
      .restoreSimulationState(private$.simulations, simulationState)

      # Apply identified values to the parameter objects. Should be an option?
      private$.applyFinalValues(values = results$par)
      # Since the batches have been initialized already, this will not be
      # required before plotting current results
      private$.needBatchInitialization <- FALSE

      # Trigger .NET gc
      ospsuite::clearMemory()

      return(results)
    },

    #' Plot the current results
    #'
    #' @details Runs all simulations with current parameter values and creates
    #' plots of every output mapping
    plotCurrentResults = function() {
      simulationState <- NULL
      # If the batches have not been initialized yet (i.e., no run has been
      # performed), this must be done prior to plotting
      if (private$.needBatchInitialization) {
        # Store simulation outputs and time intervals to reset them at the end
        # of the run.
        simulationState <- .storeSimulationState(private$.simulations)
        private$.batchInitialization()
      }

      # Run evaluate once with the current values of the parameters
      parValues <- unlist(lapply(self$parameters, function(x) {
        x$currValue
      }), use.names = FALSE)
      dataCombined <- private$.evaluate(parValues)

      # Create figures and plot
      plotConfiguration <- DefaultPlotConfiguration$new()
      multiPlot <- lapply(seq_along(dataCombined), function(idx) {
        scaling <- private$.outputMappings[[idx]]$scaling
        plotConfiguration$yAxisScale <- scaling
        plotConfiguration$legendPosition <- NULL
        indivTimeProfile <- plotIndividualTimeProfile(dataCombined[[idx]], plotConfiguration)
        plotConfiguration$legendPosition <- "none"
        obsVsSim <- plotObservedVsSimulated(dataCombined[[idx]], plotConfiguration)
        plotConfiguration$yAxisScale <- "lin"
        resVsTime <- plotResidualsVsTime(dataCombined[[idx]], plotConfiguration)
        plotGridConfiguration <- PlotGridConfiguration$new()
        plotGridConfiguration$addPlots(list(indivTimeProfile, obsVsSim, resVsTime))
        return(plotGrid(plotGridConfiguration))
      })

      # Mark that the batches have been initialized and restore simulation state
      private$.needBatchInitialization <- FALSE
      if (!is.null(simulationState)) {
        .restoreSimulationState(private$.simulations, simulationState)
      }

      return(multiPlot)
    },

    #' @description
    #' Plot the comparison between the simulation (with a set of parameters
    #' supplied as the argument) and the observed data
    #' @param par current vector of parameters
    #' @param config plot configuration to be passed to `{ospsuite}` plotting functions
    plotParameterValues = function(par, config) {
      plotIndividualTimeProfile(private$.evaluate(par), defaultPlotConfiguration = config)
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Simulations", unlist(lapply(private$.simulations, function(x) {
        x$sourceFile
      }), use.names = FALSE))
      private$printLine("Number of parameters", length(private$.piParameters))
      private$printLine("Simulate to steady-state", private$.configuration$simulateSteadyState)
      private$printLine("Steady-state time [min]", private$.configuration$steadyStateTime)
      private$printLine("Print feedback after each iteration", private$.configuration$printIterationFeedback)
      invisible(self)
    }
  )
)
