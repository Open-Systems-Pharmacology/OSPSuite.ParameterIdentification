#' @title ParameterIdentification
#' @docType class
#' @description A task to identify optimal parameter values based on simulation
#'   outputs and observed data
#' @import ospsuite.utils
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
    .fnEvaluations = 0,
    # CV for M3 target function
    # Assume CV of 20% for LQ. From DOI: 10.1023/a:1012299115260
    .cvM3 = 0.2,
    # pre-calculate SD for log-transformed data assuming CV of 20%
    # Used for M3 target function
    # From https://medcraveonline.com/MOJPB/correct-use-of-percent-coefficient-of-variation-cv-formula-for-log-transformed-data.html
    .sdForLogCV = NULL,

    # Creates simulation batches from simulations.
    .batchInitialization = function() {
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

    # Calculate the target function that is going to be minimized during
    # parameter estimation.
    .targetFunction = function(currVals) {
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
      # Returning a list of `Inf`s as otherwise the "Marq" method complains about
      # receiving only single value and not residuals.
      # (I think this would also lead to a failure where only one observed data
      # point is fitted).
      if (any(is.na(obsVsPredList))) {
        out <- list(
          model = Inf,
          minlogp = Inf,
          var = data.frame(
            name           = "Values",
            scale          = 1,
            N              = 1,
            SSR.unweighted = Inf,
            SSR.unscaled   = Inf,
            SSR            = Inf
          ),
          residuals = data.frame(
            name = "Values",
            x = 0,
            obs = 0,
            mod = Inf,
            weight = 1,
            res.unweighted = Inf,
            res = Inf
          )
        )
        class(out) <- "modCost"
        return(out)
      }

      # Error calculated for uncensored values (i.e., above LQ or no LLOQ censoring)
      # Summed up over all output mappings
      unscensoredError <- NULL
      # Error calculated for censored values (i.e., below LQ if LLOQ censoring)
      # Summed up over all output mappings
      censoredError <- NULL

      # Calculate error for each output mapping separately and add them up
      for (idx in seq_along(private$.outputMappings)) {
        # Calling unit converter to unify the units within the DataCombined
        obsVsPredDf <- ospsuite:::.unitConverter(obsVsPredList[[idx]]$toDataFrame())
        # For least squares target function, values below LLOQ will be set to
        # LLOQ/2 for simulation data and therefore always equal to the observed values
        if (private$.configuration$targetFunctionType == "lsq") {
          # replacing values below LLOQ with LLOQ / 2
          if (sum(is.finite(obsVsPredDf$lloq)) > 0) {
            lloq <- min(obsVsPredDf$lloq, na.rm = TRUE)
            obsVsPredDf[(obsVsPredDf$dataType == "simulated" &
              obsVsPredDf$yValues < lloq), "yValues"] <- lloq / 2
          }
        }

        # Transform to log if required.
        if (private$.outputMappings[[idx]]$scaling == "log") {
          UNITS_EPSILON <- ospsuite::toUnit(
            quantityOrDimension = obsVsPredDf$yDimension[[1]],
            values = ospsuite::getOSPSuiteSetting("LOG_SAFE_EPSILON"),
            targetUnit = obsVsPredDf$yUnit[[1]],
            molWeight = 1
          )
          obsVsPredDf$yValues <- ospsuite.utils::logSafe(obsVsPredDf$yValues, epsilon = UNITS_EPSILON, base = exp(1))
          obsVsPredDf$lloq <- ospsuite.utils::logSafe(obsVsPredDf$lloq, epsilon = UNITS_EPSILON, base = exp(1))
        }

        # Extract simulated and observed data
        simulated <- obsVsPredDf[obsVsPredDf$dataType == "simulated", ]
        observed <- obsVsPredDf[obsVsPredDf$dataType == "observed", ]

        # Least squares target function.
        if (private$.configuration$targetFunctionType == "lsq") {
          modelDf <- data.frame("Time" = simulated$xValues, "Values" = simulated$yValues)
          obsDf <- data.frame("Time" = observed$xValues, "Values" = observed$yValues)
        }

        # M3 LLOQ method. Implementation based on DOI: 10.1023/a:1012299115260
        # In particular, equation 6
        if (private$.configuration$targetFunctionType == "m3") {
          # Separate censored and uncensored data
          observed_uncensored <- observed[is.na(observed$lloq) | (observed$yValues > observed$lloq), ]
          observed_censored <- observed[!is.na(observed$lloq) & (observed$yValues <= observed$lloq), ]
          simulated_uncensored <- merge(observed_uncensored[c("xValues", "xUnit", "xDimension")], simulated, by = c("xValues", "xUnit", "xDimension"), all.x = TRUE)
          simulated_censored <- merge(observed_censored[c("xValues", "xUnit", "xDimension")], simulated, by = c("xValues", "xUnit", "xDimension"), all.x = TRUE)

          # Data frames used for calculation of uncensored error
          modelDf <- data.frame("Time" = simulated_uncensored$xValues, "Values" = simulated_uncensored$yValues)
          # 'merge()' produces multiple entries for the same x value when multiple
          # observed data sets are present. Apply 'unique()' to avoid duplication
          # of values and a warning during interpolation.
          modelDf <- unique(modelDf)
          obsDf <- data.frame("Time" = observed_uncensored$xValues, "Values" = observed_uncensored$yValues)

          # sd for untransformed data is defined as CV * mean, while mean is the LQ
          if (private$.outputMappings[[idx]]$scaling == "lin") {
            sd <- abs(private$.cvM3 * observed_censored$lloq)
          } else {
            sd <- private$.sdForLogCV
          }

          # Calculate censored residuals for this output mapping and add it to
          # the total censored residuals vector
          if (nrow(observed_censored) > 0) {
            # First calculate the probabilities
            censoderProbabilities <- pnorm((observed_censored$lloq - simulated_censored$yValues) / sd)
            # Replace zeros by the minimal number to avoid Inf for censoder error
            censoderProbabilities[censoderProbabilities == 0] <- .Machine$double.xmin

            # As desctibed in Equation 6. Calculate a vector of residuals
            censoredErrorVector <- -2 * log(censoderProbabilities,
              base = 10
            )
            # We must take the square root of the censored residuals because modFit
            # expects the unsquared residuals! The total error value is then calculated
            # as squared residuals but as can be seen in Equation 5, M3 method returns
            # already squared values
            censoredErrorVector <- sqrt(censoredErrorVector)

            # Construct the data frame with censored residuals with the same structure
            # as the 'residuals'  df
            censoredError <- rbind(censoredError, data.frame(
              name = "Values",
              x = observed_censored$xValues,
              obs = observed_censored$yValues,
              mod = simulated_censored$yValues,
              weight = 1,
              res.unweighted = censoredErrorVector,
              res = censoredErrorVector
            ))
          }
        }

        # Calculate uncensored error.
        unscensoredError <- modCost(model = modelDf, obs = obsDf, x = "Time", cost = unscensoredError)
      }

      # Total error. Either the uncensored error,
      # or with addition of censored values
      runningCost <- unscensoredError
      if (!is.null(censoredError)) {
        # Add censored error
        totalCost <- runningCost$model + sum(censoredError$res^2)
        # Extend the structure of the results object returned by modCost by
        # the uncensored cost
        # Append the data frame to the $residuals df
        runningCost$residuals <- rbind(runningCost$residuals, censoredError)
        #
        # # Update the total cost 'model'
        runningCost$model <- totalCost
        runningCost$var$N <- length(runningCost$residuals$res)
        runningCost$var$SSR.unweighted <- totalCost
        runningCost$var$SSR.unscaled <- totalCost
        runningCost$var$SSR <- totalCost
        runningCost$minlogp <- -sum(log(pmax(0, dnorm(
          runningCost$residuals$mod, runningCost$residuals$obs,
          1 / runningCost$residuals$weight
        ))))
      }

      # Print current error if requested
      if (private$.configuration$printEvaluationFeedback) {
        cat(paste0(
          "fneval ", private$.fnEvaluations, ": parameters ", paste0(signif(currVals, 3), collapse = "; "),
          ", target function ", signif(runningCost$model, 3), "\n"
        ))
      }
      return(runningCost)
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

      # Depending on the `algorithm` argument in the `PIConfiguration` object, the
      # actual optimization call will use one of the underlying optimization routines
      message(paste0("Running optimization algorithm: ", private$.configuration$algorithm))

      if (private$.configuration$algorithm == "HJKB") {
        time <- system.time({
          results <- dfoptim::hjkb(par = startValues, fn = function(p) {
            private$.targetFunction(p)$model
          }, control = private$.configuration$algorithmOptions, lower = lower, upper = upper)
        })
      } else if (private$.configuration$algorithm == "BOBYQA") {
        time <- system.time({
          results <- nloptr::bobyqa(x0 = startValues, fn = function(p) {
            private$.targetFunction(p)$model
          }, control = private$.configuration$algorithmOptions, lower = lower, upper = upper)
        })
      } else if (private$.configuration$algorithm == "DEoptim") {
        # passing control arguments by name into the DEoptim.control object, using DEoptim default values where needed
        control <- DEoptim::DEoptim.control(VTR = ifelse("VTR" %in% names(private$.configuration$algorithmOptions), private$.configuration$algorithmOptions[["VTR"]], -Inf),
                                            strategy = ifelse("strategy" %in% names(private$.configuration$algorithmOptions), private$.configuration$algorithmOptions[["strategy"]], 2),
                                            bs = ifelse("bs" %in% names(private$.configuration$algorithmOptions), private$.configuration$algorithmOptions[["bs"]], FALSE),
                                            NP = ifelse("NP" %in% names(private$.configuration$algorithmOptions), private$.configuration$algorithmOptions[["NP"]], NA),
                                            itermax = ifelse("itermax" %in% names(private$.configuration$algorithmOptions), private$.configuration$algorithmOptions[["itermax"]], 200),
                                            CR = ifelse("CR" %in% names(private$.configuration$algorithmOptions), private$.configuration$algorithmOptions[["CR"]], 0.5),
                                            F = ifelse("F" %in% names(private$.configuration$algorithmOptions), private$.configuration$algorithmOptions[["F"]], 0.8),
                                            trace = ifelse("trace" %in% names(private$.configuration$algorithmOptions), private$.configuration$algorithmOptions[["trace"]], TRUE),
                                            reltol = ifelse("reltol" %in% names(private$.configuration$algorithmOptions), private$.configuration$algorithmOptions[["reltol"]], sqrt(.Machine$double.eps)),
                                            steptol = ifelse("steptol" %in% names(private$.configuration$algorithmOptions), private$.configuration$algorithmOptions[["steptol"]], ifelse("itermax" %in% names(private$.configuration$algorithmOptions), private$.configuration$algorithmOptions[["itermax"]], 200)))
        time <- system.time({
          results <- DEoptim::DEoptim(fn = function(p) {
            private$.targetFunction(p)$model
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
        # For the target function that represents the deviation = -2 * log(L),
        # results$hessian / 2 is the observed information matrix
        # https://stats.stackexchange.com/questions/27033/
        results$sigma <- tryCatch(
          {
            # Calculate hessian if the selected algorithm does not calculate it by default
            if (is.null(results$hessian)) {
              message("Post-hoc estimation of hessian")
              # If the parameter values are close to their bounds, the hessian
              # should be calculated with a smaller epsilon than a default value
              # of 1e-4
              hessianEpsilon <- min(1e-4, 0.1 * abs(results$par - lower), 0.1 * abs(results$par - upper))
              results$hessian <- numDeriv::hessian(func = function(p) {
                private$.targetFunction(p)$model
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
      results$lwr <- results$par - qnorm(p = 1 - 0.05/2) * results$sigma
      results$upr <- results$par + qnorm(p = 1 - 0.05/2) * results$sigma
      results$cv <- results$sigma / abs(results$par) * 100
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
      private$.sdForLogCV <- sqrt(log(1 + private$.cvM3^2, base = 10) / log(10))

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
    #' @details When the identification if finished, the best identified values of the parameters
    #' are accessible via the `currValue`-field of the `PIParameters`-object.
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
      # Reset function evaluations counter
      private$.fnEvaluations <- 0
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

    #' @param par Values of paramterers to be applied to the simulations.
    #' If `NULL` (default), current parameter values are applied. If custom
    #' values are supplied, the they must be in the same order as `ParameterIdentification$parameters`
    #'
    #' @description
    #' Plot the results of parameter estimation
    #'
    #' @details Runs all simulations with current (default) or supplied
    #' parameter values and creates plots of every output mapping
    plotResults = function(par = NULL) {
      simulationState <- NULL
      # If the batches have not been initialized yet (i.e., no run has been
      # performed), this must be done prior to plotting
      if (private$.needBatchInitialization) {
        # Store simulation outputs and time intervals to reset them at the end
        # of the run.
        simulationState <- .storeSimulationState(private$.simulations)
        private$.batchInitialization()
      }

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
      plotConfiguration <- DefaultPlotConfiguration$new()
      multiPlot <- lapply(seq_along(dataCombined), function(idx) {
        scaling <- private$.outputMappings[[idx]]$scaling
        plotConfiguration$yAxisScale <- scaling
        plotConfiguration$legendPosition <- NULL
        indivTimeProfile <- plotIndividualTimeProfile(dataCombined[[idx]], plotConfiguration)
        plotConfiguration$legendPosition <- "none"
        plotConfiguration$xAxisScale <- scaling
        obsVsSim <- plotObservedVsSimulated(dataCombined[[idx]], plotConfiguration)
        plotConfiguration$xAxisScale <- "lin"
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
    #' Calculates the values of the objective function on a rectangular grid
    #' @param lower A vector of lower bounds for parameters, with the same length as the number of parameters.
    #' By default, uses the minimal values supported for parameters.
    #' @param upper A vector of upper bounds for parameters, with the same length as the number of parameters.
    #' By default, uses the maximal values supported for parameters.
    #' @param logScaleFlag A single logical value or a vector of logical values indicating
    #' if grid should be evenly spaced on a linear or a logarithmic scale. Defaults to `FALSE`.
    #' @param totalEvaluations An integer number. The grid will have as many points so that the
    #' total number of grid points does not exceed `totalEvaluations`. Defaults to `50`.
    #' @param margin Can be set to a non-zero positive value so that the edges of the grid will be away
    #' from the exact parameter bounds.
    #' @param setStartingPoint If `TRUE`, the best parameter values will be set as the starting point
    calculateGrid = function(lower = NA, upper = NA, logScaleFlag = FALSE, totalEvaluations = 50, margin = 0, setStartingPoint = FALSE) {
      # If the batches have not been initialized yet (i.e., no run has been
      # performed), this must be done prior to plotting
      if (private$.needBatchInitialization) {
        # Store simulation outputs and time intervals to reset them at the end
        # of the run.
        simulationState <- .storeSimulationState(private$.simulations)
        private$.batchInitialization()
      }

      np <- length(private$.piParameters)
      # logScaleFlag can be specified as a single value (common for all parameters)
      # or as a vector of values (one for each parameter)
      if (length(logScaleFlag) != np) {
        logScaleFlag <- rep(logScaleFlag, length.out = np)
      }

      # if lower and upper are not supplied, we reuse parameter bounds
      # By default, margin = 0, but we can use a non-zero value
      # so that the grid does not start exactly at the parameter bound
      if (missing(lower)) {
        lower <- vector(mode = "list", length = np)
        for (idx in seq_along(private$.piParameters)) {
          lower[[idx]] <- private$.piParameters[[idx]]$minValue + margin
        }
      }
      if (missing(upper)) {
        upper <- vector(mode = "list", length = np)
        for (idx in seq_along(private$.piParameters)) {
          upper[[idx]] <- private$.piParameters[[idx]]$maxValue - margin
        }
      }

      gridSize <- floor(totalEvaluations^(1/np))
      gridList <- vector(mode = "list", length = np)
      for (idx in seq_along(private$.piParameters)) {
        # the names of the parameters are extracted from the first available path
        parameterName <- private$.piParameters[[idx]]$parameters[[1]]$path
        if (logScaleFlag[[idx]]) {
          grid <- exp(seq(from = log(lower[[idx]]), to = log(upper[[idx]]), length.out = gridSize))
        } else {
          grid <- seq(from = lower[[idx]], to = upper[[idx]], length.out = gridSize)
        }
        gridList[[idx]] <- grid
        names(gridList)[[idx]] <- parameterName
      }

      OFVGrid <- expand.grid(gridList)
      # all columns from the OFVGrid are passed in the same order to the objective function
      OFVGrid[["ofv"]] <- purrr::pmap_dbl(OFVGrid, function(...) {
        private$.targetFunction(c(...))$model
      })

      # Mark that the batches have been initialized and restore simulation state
      # Note: we can't use `simulationState` variable in the condition
      # because it might be not defined
      if (private$.needBatchInitialization){
        .restoreSimulationState(private$.simulations, simulationState)
        private$.needBatchInitialization <- FALSE
      }

      if (setStartingPoint) {
        # set the best parameter values as the starting point
        bestPoint <- OFVGrid[which.min(OFVGrid[["ofv"]]), ]
        for (idx in seq_along(private$.piParameters)) {
          private$.piParameters[[idx]]$startValue <- bestPoint[[idx]]
        }
        message("Set the best parameter values as the starting point.")
      }

      return(tibble::as_tibble(OFVGrid))
    },

    #' @description
    #' Calculates the values of the objective function on all orthogonal lines
    #' passing through a given point in the parameter space.
    calculateProfiles = function(par = NA, lower = NA, upper = NA, totalEvaluations = NA) {
      # If the batches have not been initialized yet (i.e., no run has been
      # performed), this must be done prior to plotting
      if (private$.needBatchInitialization) {
        # Store simulation outputs and time intervals to reset them at the end
        # of the run.
        simulationState <- .storeSimulationState(private$.simulations)
        private$.batchInitialization()
      }

      np <- length(private$.piParameters)

      # if par is not supplied, we use the current parameter values
      if (missing(par)) {
        par <- unlist(lapply(private$.piParameters, function(x) {
          x$currValue
        }), use.names = FALSE)
      }

      # if lower and upper are not supplied, we calculate them as 0.9 and 1.1
      # of the current parameter values
      if (missing(lower)) {
        lower <- 0.9 * par
      }
      if (missing(upper)) {
        upper <- 1.1 * par
      }

      # calculate the grid for each parameter separately
      if (missing(totalEvaluations)) {
        gridSize <- 21
        # creates a grid with values at 0.9, 0.91, 0.92 .. 1.0 .. 1.09, 1.1
        # of the current parameter values
      } else {
        gridSize <- floor(totalEvaluations / np)
      }
      gridList <- vector(mode = "list", length = np)
      for (idx in seq_along(private$.piParameters)) {
        gridList[[idx]] <- rep(par[[idx]], gridSize)
        names(gridList)[[idx]] <- private$.piParameters[[idx]]$parameters[[1]]$path
      }
      defaultGrid <- tibble::as_tibble(gridList)

      profileList <- vector(mode = "list", length = np)
      for (idx in seq_along(private$.piParameters)) {
        # the names of the parameters are extracted from the first available path
        parameterName <- private$.piParameters[[idx]]$parameters[[1]]$path
        grid <- seq(from = lower[[idx]], to = upper[[idx]], length.out = gridSize)
        currentGrid <- defaultGrid
        currentGrid[[parameterName]] <- grid
        # creates a tibble with the column name from the `parameterName` variable
        profileList[[idx]] <- tibble::tibble(!!parameterName := grid)
        profileList[[idx]][["ofv"]] <- purrr::pmap_dbl(currentGrid, function(...) {
          private$.targetFunction(c(...))$model
        })
        names(profileList)[[idx]] <- parameterName
      }

      # Mark that the batches have been initialized and restore simulation state
      # Note: we can't use `simulationState` variable in the condition
      # because it might be not defined
      if (private$.needBatchInitialization){
        .restoreSimulationState(private$.simulations, simulationState)
        private$.needBatchInitialization <- FALSE
      }

      return(profileList)
    },

    #' @description
    #' Plot the profiles of the objective function calculated by the calculateProfiles method.
    plotOFVProfiles = function(profiles) {
      plotList <- vector(mode = "list", length = length(profiles))
      for (idx in seq_along(profiles)) {
        parameterName <- names(profiles)[[idx]]
        plotList[[idx]] <- ggplot2::ggplot(data = profiles[[idx]],
                                           ggplot2::aes(x = .data[[parameterName]], y = .data[["ofv"]])) +
          ggplot2::theme_bw() +
          ggplot2::geom_point(ggplot2::aes(col = 1/ofv)) +
          ggplot2::labs(x = parameterName, y = "OFV") +
          ggplot2::scale_color_viridis_c() +
          ggplot2::guides(col = "none")
      }
      return(plotList)
    },

    #' @description
    #' Plot a heatmap of the objective function calculated by the calculateGrid method
    plotOFVGrid = function(grid) {
      # This plot only makes sense for 2-parametric problems
      stopifnot(length(private$.piParameters) == 2)
      xParameterName <- names(grid)[[1]]
      yParameterName <- names(grid)[[2]]
      plot <- ggplot2::ggplot(data = grid,
                              ggplot2::aes(x = .data[[xParameterName]], y = .data[[yParameterName]])) +
        ggplot2::theme_bw() +
        ggplot2::geom_contour_filled(ggplot2::aes(z = 1/ofv)) +
        ggplot2::geom_point(x = private$.piParameters[[1]]$currValue,
                            y = private$.piParameters[[2]]$currValue,
                            size = 3,
                            color = "white") +
        ggplot2::labs(x = xParameterName, y = yParameterName) +
        ggplot2::scale_fill_viridis_d() +
        ggplot2::guides(fill = "none")
      return(plot)
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
      private$printLine("Print feedback after each function evaluation", private$.configuration$printEvaluationFeedback)
      invisible(self)
    }
  )
)
