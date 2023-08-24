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

          return(NA)
        }
      )
      # Returning a list of `Inf`s as otherwise the "Marq" method complains about
      # receiving only single value and not residuals.
      # (I think this would also lead to a failure where only one observed data
      # point is fitted).
      if (any(is.na(obsVsPredList))) {
        return(c(Inf, Inf))
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
      if (private$.configuration$printIterationFeedback) {
        # Current total error is the sum of squared residuals
        private$.iteration <- private$.iteration + 1
        cat(paste0(
          "iter ", private$.iteration, ": parameters ", paste0(signif(currVals, 3), collapse = "; "),
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

      # Depending on the method argument in the `PIConfiguration` object, the
      # actual optimization call will use one of the underlying optimization routines
      message(paste0("Running optimization algorithm: ", private$.configuration$algorithm))
      results <- NULL
      if (private$.configuration$algorithm %in% c("bobyqa", "Marq")) {
        time <- system.time(results <- FME::modFit(f = private$.targetFunction, p = startValues, lower = lower, upper = upper, method = private$.configuration$algorithm, control = private$.configuration$algorithmOptions))

        # Sigma values are standard deviations of the estimated parameters. They are
        # extracted from the estimated hessian matrix through the summary function.
        # The 95% confidence intervals are defined by two sigma values away from the
        # point estimate. The coefficient of variation (CV) is the ratio of standard
        # deviation to the point estimate.
        sigma <- as.numeric(summary(results)[["par"]][, "Std. Error"])
        results$lwr <- results$par - 1.96 * sigma
        results$upr <- results$par + 1.96 * sigma
        results$cv <- sigma / abs(results$par) * 100

        results$elapsed <- time[[3]]
      }
      if (private$.configuration$algorithm %in% c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN")) {
        time <- system.time(results <- optim(par = startValues, fn = function(p) {private$.targetFunction(p)$model}, lower = lower, upper = upper, method = private$.configuration$algorithm, control = private$.configuration$algorithmOptions, hessian = TRUE))

        # For the target function that represents the deviation = -2 * log(L),
        # results$hessian / 2 is the observed information matrix
        # https://stats.stackexchange.com/questions/27033/
        fim <- solve(results$hessian / 2)
        sigma <- sqrt(diag(fim))
        results$lwr <- results$par - 1.96 * sigma
        results$upr <- results$par + 1.96 * sigma
        results$cv <- sigma / abs(results$par) * 100

        results$elapsed <- time[[3]]
      }
      if (private$.configuration$algorithm == "minqa") {
        # "minqa" class overrides printing, so we remove it with "unclass"
        time <- system.time(results <- unclass(minqa::bobyqa(par = startValues, fn = function(p) {private$.targetFunction(p)$model}, control = private$.configuration$algorithmOptions, lower = lower, upper = upper)))

        message("Post-hoc estimation of hessian")
        # The call to minqa::bobyqa does not return an estimated hessian, so
        # we run one iteration of optim to return the hessian
        results$value <- results$fval
        optim_results <- optim(results$par, function(p) {private$.targetFunction(p)$model}, method = "L-BFGS-B", control = list(maxit = 1), lower = lower, upper = upper, hessian = TRUE)
        fim <- solve(optim_results$hessian / 2)
        sigma <- sqrt(diag(fim))
        results$lwr <- results$par - 1.96 * sigma
        results$upr <- results$par + 1.96 * sigma
        results$cv <- sigma / abs(results$par) * 100

        results$elapsed <- time[[3]]
      }
      if (private$.configuration$algorithm == "NMKB") {
        time <- system.time(results <- dfoptim::nmkb(par = startValues, fn = function(p) {private$.targetFunction(p)$model}, control = private$.configuration$algorithmOptions, lower = lower, upper = upper))
        # The call to dfoptim::nmkb does not return an estimated hessian, so
        # we run one iteration of optim to return the hessian
        message("Post-hoc estimation of hessian")
        optim_results <- optim(results$par, function(p) {private$.targetFunction(p)$model}, method = "L-BFGS-B", control = list(maxit = 1), lower = lower, upper = upper, hessian = TRUE)
        fim <- solve(optim_results$hessian / 2)
        sigma <- sqrt(diag(fim))
        results$lwr <- results$par - 1.96 * sigma
        results$upr <- results$par + 1.96 * sigma
        results$cv <- sigma / abs(results$par) * 100
        results$elapsed <- time[[3]]
      }
      if (private$.configuration$algorithm == "HJKB") {
        time <- system.time(results <- dfoptim::hjkb(par = startValues, fn = function(p) {private$.targetFunction(p)$model}, control = private$.configuration$algorithmOptions, lower = lower, upper = upper))
        # The call to dfoptim::hjkb does not return an estimated hessian, so
        # we run one iteration of optim to return the hessian
        message("Post-hoc estimation of hessian")
        optim_results <- optim(results$par, function(p) {private$.targetFunction(p)$model}, method = "L-BFGS-B", control = list(maxit = 1), lower = lower, upper = upper, hessian = TRUE)
        fim <- solve(optim_results$hessian / 2)
        sigma <- sqrt(diag(fim))
        results$lwr <- results$par - 1.96 * sigma
        results$upr <- results$par + 1.96 * sigma
        results$cv <- sigma / abs(results$par) * 100
        results$elapsed <- time[[3]]
      }
      if (private$.configuration$algorithm == "nloptr:BOBYQA") {
        time <- system.time(results <- nloptr::bobyqa(x0 = startValues, fn = function(p) {private$.targetFunction(p)$model}, control = private$.configuration$algorithmOptions, lower = lower, upper = upper))
        # The call to nloptr::bobyqa does not return an estimated hessian, so
        # we run one iteration of optim to return the hessian
        message("Post-hoc estimation of hessian")
        optim_results <- optim(results$par, function(p) {private$.targetFunction(p)$model}, method = "L-BFGS-B", control = list(maxit = 1), lower = lower, upper = upper, hessian = TRUE)
        fim <- solve(optim_results$hessian / 2)
        sigma <- sqrt(diag(fim))
        results$lwr <- results$par - 1.96 * sigma
        results$upr <- results$par + 1.96 * sigma
        results$cv <- sigma / abs(results$par) * 100
        results$elapsed <- time[[3]]
      }
      if (private$.configuration$algorithm == "nloptr:NM") {
        time <- system.time(results <- nloptr::neldermead(x0 = startValues, fn = function(p) {private$.targetFunction(p)$model}, control = private$.configuration$algorithmOptions, lower = lower, upper = upper))
        # The call to nloptr::neldermead does not return an estimated hessian, so
        # we run one iteration of optim to return the hessian
        message("Post-hoc estimation of hessian")
        optim_results <- optim(results$par, function(p) {private$.targetFunction(p)$model}, method = "L-BFGS-B", control = list(maxit = 1), lower = lower, upper = upper, hessian = TRUE)
        fim <- solve(optim_results$hessian / 2)
        sigma <- sqrt(diag(fim))
        results$lwr <- results$par - 1.96 * sigma
        results$upr <- results$par + 1.96 * sigma
        results$cv <- sigma / abs(results$par) * 100
        results$elapsed <- time[[3]]
      }
      if (private$.configuration$algorithm == "solnp") {
        time <- system.time(results <- Rsolnp::solnp(pars = startValues, fun = function(p) {private$.targetFunction(p)$model}, control = private$.configuration$algorithmOptions))
        # The call to solnp does not return an estimated hessian, so
        # we run one iteration of optim to return the hessian
        message("Post-hoc estimation of hessian")
        results$par <- results$pars
        results$value <- private$.targetFunction(results$par)$model
        optim_results <- optim(results$par, function(p) {private$.targetFunction(p)$model}, method = "L-BFGS-B", control = list(maxit = 1), lower = lower, upper = upper, hessian = TRUE)
        fim <- solve(optim_results$hessian / 2)
        sigma <- sqrt(diag(fim))
        results$lwr <- results$par - 1.96 * sigma
        results$upr <- results$par + 1.96 * sigma
        results$cv <- sigma / abs(results$par) * 100
        results$elapsed <- time[[3]]
      }
      if (private$.configuration$algorithm == "DEoptim") {
        time <- system.time(results <- DEoptim::DEoptim(fn = function(p) {private$.targetFunction(p)$model}, lower = lower, upper = upper, control = DEoptim::DEoptim.control(itermax = 1)))
        # The call to DEoptim does not return an estimated hessian, so
        # we run one iteration of optim to return the hessian
        results$par <- results$optim$bestmem
        results$value <- results$optim$bestval
        message("Post-hoc estimation of hessian")
        optim_results <- optim(results$par, function(p) {private$.targetFunction(p)$model}, method = "L-BFGS-B", control = list(maxit = 1), lower = lower, upper = upper, hessian = TRUE)
        fim <- solve(optim_results$hessian / 2)
        sigma <- sqrt(diag(fim))
        results$lwr <- results$par - 1.96 * sigma
        results$upr <- results$par + 1.96 * sigma
        results$cv <- sigma / abs(results$par) * 100
        results$elapsed <- time[[3]]
      }
      if (private$.configuration$algorithm == "PSoptim") {
        time <- system.time(results <- pso::psoptim(par = startValues, fn = function(p) {private$.targetFunction(p)$model}, lower = lower, upper = upper, control = private$.configuration$algorithmOptions))
        # The call to psoptim does not return an estimated hessian, so
        # we run one iteration of optim to return the hessian
        message("Post-hoc estimation of hessian")
        optim_results <- optim(results$par, function(p) {private$.targetFunction(p)$model}, method = "L-BFGS-B", control = list(maxit = 1), lower = lower, upper = upper, hessian = TRUE)
        fim <- solve(optim_results$hessian / 2)
        sigma <- sqrt(diag(fim))
        results$lwr <- results$par - 1.96 * sigma
        results$upr <- results$par + 1.96 * sigma
        results$cv <- sigma / abs(results$par) * 100
        results$elapsed <- time[[3]]
      }
      if (private$.configuration$algorithm == "GenOUD") {
        boundary_domains = matrix(c(lower, upper), ncol = 2)
        time <- system.time(results <- rgenoud::genoud(fn = function(p) {private$.targetFunction(p)$model}, nvars = length(lower), pop.size = 20, Domains = boundary_domains, boundary.enforcement = TRUE, max.generations = 10, hard.generation.limit = TRUE))
        # The call to GenOUD does not return an estimated hessian, so
        # we run one iteration of optim to return the hessian
        message("Post-hoc estimation of hessian")
        optim_results <- optim(results$par, function(p) {private$.targetFunction(p)$model}, method = "L-BFGS-B", control = list(maxit = 1), lower = lower, upper = upper, hessian = TRUE)
        fim <- solve(optim_results$hessian / 2)
        sigma <- sqrt(diag(fim))
        results$lwr <- results$par - 1.96 * sigma
        results$upr <- results$par + 1.96 * sigma
        results$cv <- sigma / abs(results$par) * 100
        results$elapsed <- time[[3]]
      }
      if (is.null(results)) {
        warning("Parameter identification stopped: ", private$.configuration$algorithm, " is not a recognized optimization algorithm")
        return(NULL)
      }
      results$algorithm <- private$.configuration$algorithm
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
