#' @title ParameterIdentification
#' @docType class
#' @description A task to identify optimal parameter values based on simulation
#'   outputs and observed data
#' @import FME ospsuite.utils
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
        private$.parameters
      } else {
        stop(messages$errorPropertyReadOnly("parameters"))
      }
    },

    #' @field configuration An object of `PIConfiguration`
    configuration = function(value) {
      if (missing(value)) {
        private$.configuration
      } else {
        validateIsOfType(configuration, "PIConfiguration")
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
    .parameters = NULL,
    .outputMappings = NULL,
    .configuration = NULL,
    # Flag if simulation batches must be created from simulations. Used for
    # plotting current results.
    .needBatchInitialization = TRUE,

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
        # The parent simulation of the quantity of the mapping.
        simId <- .getSimulationContainer(outputMapping$quantity)$id
        simulation <- private$.simulations[[simId]]
        # Add the quantity to the outputs of the simulations.
        ospsuite::addOutputs(quantitiesOrPaths = outputMapping$quantity, simulation = simulation)
        # Add time points present in the observed data of this mapping.
        for (observedData in outputMapping$observedData) {
          # Time values can be stored in units different from the base unit
          # and must be converted to the base unit first.
          label <- observedData$name
          xFactor <- outputMapping$xFactors[[label]]
          if (is.null(xFactor)) {
            xFactor <- 1
          }
          xOffset <- outputMapping$xOffsets[[label]]
          if (is.null(xOffset)) {
            xOffset <- 0
          }
          xVals <- ospsuite::toBaseUnit(ospsuite::ospDimensions$Time,
            values = (observedData$xValues + xOffset) * xFactor,
            unit = observedData$xUnit
          )
          simulation$outputSchema$addTimePoints(xVals)
        }
      }

      # Add parameters that will be optimized to the list of variable parameters
      for (piParameter in private$.parameters) {
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

    # Calculate the lsq target function
    .targetFunction = function(currVals) {
      obsVsPred <- private$.evaluate(currVals)
      if (tolower(private$.configuration$targetFunctionType %in% c("lsq", "least squares"))) {
        target <- private$.LSQ(obsVsPred)
      }
      private$.iteration <- private$.iteration + 1
      target <- private$.LSQ(obsVsPred)
      if (private$.configuration$printIterationFeedback) {
        print(paste0("iter ", private$.iteration, ": parameters ", paste0(signif(currVals, 3), collapse = "; "), ", target function ", signif(target, 3)))
      }
      return(target)
    },

    # Perform one iteration of the optimization workflow and calculate the residuals
    # @param currVals Numerical vector of the parameter values to be applied
    # @return Residuals, based on the selected objective function
    #.iterate = function(currVals) {
    #  # Simulate with new parameter values and return the data mappings from which the error will be calculated.
    #  dataMappings <- private$.evaluate(currVals)
    #  return(private$.calculateResiduals(dataMappings))
    #},

    # Apply final identified values to simulation parameter objects.
    .applyFinalValues = function(values) {
      # Iterate through PIParameters

      # THE LOGIC WOULD PROBABLY DEPEND ON THE TYPE OF RESULTS OUTPUT RETURNED
      # BY THE SELECTED METHOD, UNLESS WE USE OUR OWN CONSISTENT RESULTS STRUCTURE
      for (idx in seq_along(values)) {
        # The order of the values corresponds to the order of PIParameters in
        # $parameters list
        piParameter <- private$.parameters[[idx]]
        piParameter$setValue(values[[idx]])
      }
    },

    # Evaluate all simulations with given parameter values
    # @param currVals Numerical vector of the parameter values to be applied
    # @return A list of \code{DataMapping} objects - one DataMapping for one output mapping.
    .evaluate = function(currVals) {
      obsVsPred <- DataCombined$new()
      # Iterate through the values and update current parameter values
      for (idx in seq_along(currVals)) {
        # The order of the values corresponds to the order of PIParameters in
        # $parameters list
        piParameter <- private$.parameters[[idx]]
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
        # Find the simulation that is the parent of the output quantity
        simId <- .getSimulationContainer(private$.outputMappings[[idx]]$quantity)$id
        # Find the simulation batch that corresponds to the simulation
        simBatch <- private$.simulationBatches[[simId]]
        obsVsPred$addSimulationResults(simulationResults[[simBatch$id]][[1]], names = simBatch$id, groups = simId)
        obsVsPred$addDataSets(private$.outputMappings[[idx]]$observedData, names = names(private$.outputMappings[[idx]]$observedData), groups = simId)

      }

      return(obsVsPred)
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

      results <- FME::modFit(f = private$.targetFunction, p = startValues, lower = lower, upper = upper, method = "bobyqa")
    },

    # Calculate residuals between simulated and observed values.
    #
    # @param dataMappingList A \code{DataMapping} or a list of \code{DataMapping} objects.
    #
    # @return Vector of residuals
    .calculateResiduals = function(dataMappingList) {
      dataMappingList <- ospsuite.utils::toList(dataMappingList)
      cost <- NULL
      for (dataMapping in dataMappingList) {
        simulatedResult <- list()

        combinedResults <- lapply(dataMapping$xySeries, function(xySeries) {
          if (xySeries$dataType == XYDataTypes$Simulated) {
            simulatedResult <<- xySeries
            return()
          }
          # Collapse all observed data
          return(list(
            dataPointsX = xySeries$xValuesProcessed(dataMapping$xUnit),
            dataPointsY = xySeries$yValuesProcessed(dataMapping$yUnit),
            dataError = xySeries$yErrorProcessed(dataMapping$yUnit)
          ))
        })

        dataPointsX <- unlist(lapply(combinedResults, function(x) {
          x$dataPointsX
        }), use.names = FALSE)
        dataPointsY <- unlist(lapply(combinedResults, function(x) {
          x$dataPointsY
        }), use.names = FALSE)
        dataError <- unlist(lapply(combinedResults, function(x) {
          x$dataError
        }), use.names = FALSE)

        # Calculate the distance between each point of the observed data to the simulated result
        modelDf <- data.frame("Time" = simulatedResult$xValues, "Values" = simulatedResult$yValues)
        obsDf <- data.frame("Time" = dataPointsX, "Values" = dataPointsY, "Error" = dataError)

        # Only use error if it does not contain 0, otherwise the cost function fails
        err <- "Error"
        if (any(dataError == 0)) {
          err <- NULL
          obsDf$Error <- NULL
        }
        cost <- FME::modCost(model = modelDf, obs = obsDf, x = "Time", cost = cost, err = err)
      }

      if (private$.configuration$printIterationFeedback) {
        print(paste0("Current error: ", cost$model))
      }

      return(cost)
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
      private$.parameters <- parameters
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
      dataMappings <- private$.evaluate(parValues)

      lapply(dataMappings, function(x) {
        x$plot()
      })

      # Mark that the batches have been initialized and restore simulation state
      private$.needBatchInitialization <- FALSE
      if (!is.null(simulationState)) {
        .restoreSimulationState(private$.simulations, simulationState)
      }

      invisible(self)
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Simulations", unlist(lapply(private$.simulations, function(x) {
        x$sourceFile
      }), use.names = FALSE))
      private$printLine("Number of parameters", length(private$.parameters))
      private$printLine("Simulate to steady-state", private$.configuration$simulateSteadyState)
      private$printLine("Steady-state time [min]", private$.configuration$steadyStateTime)
      private$printLine("Print feedback after each iteration", private$.configuration$printIterationFeedback)
      invisible(self)
    }
  )
)
