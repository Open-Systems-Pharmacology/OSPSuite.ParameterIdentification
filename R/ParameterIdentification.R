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
    # The `SimulationModel` of the current public call, built in
    # `.batchInitialization()`
    .model = NULL,
    # Mirrored from `.model` for the tests until they are rewritten (step 9)
    .simulationBatches = NULL,
    .variableMolecules = NULL,
    .variableParameters = NULL,
    .parameterTargets = NULL,
    # List of `PIParameter` objects for optimization
    .piParameters = NULL,
    # List of `PIOutputMapping` objects
    .outputMappings = NULL,
    # `PIConfiguration` object instance
    .configuration = NULL,
    # The `PIObjective` of the current public call and bootstrap sample,
    # built on the first evaluation (see `.objectiveFor()`)
    .objective = NULL,
    # The `PIObservations` of `.objective`
    .data = NULL,
    # The prepared observed data of `.objective`, mirrored after every
    # evaluation for the tests that read them until the tests are rewritten
    .observedData = NULL,
    # Stores last optimization result
    .lastOptimResult = NULL,
    # Cost details of the best evaluation of the last optimization
    .fitCostDetails = NULL,
    # Clones of the `PIParameters` at the end of the last optimization, so
    # that the results keep the units and bounds of the fit
    .fitParameters = NULL,
    # Stores full cost summary from the best objective function evaluation
    .bestCostSummary = NULL,
    # Stores full cost summary from last objective function evaluation
    .lastCostSummary = NULL,
    # Number of function evaluations
    .fnEvaluations = 0,
    # Number of evaluations of `run()` whose simulations failed
    .failedEvaluations = 0,
    # List of `PKOutputMapping` objects (PK metric mode)
    .pkMappings = NULL,

    # Batch Initialization for Simulations
    #
    # Every public method that evaluates the objective function starts here.
    # The simulations, the observed data sets and their transformations can
    # change between two calls, so the observed data are read again and the
    # model is built again from the current simulations and observed times.
    # The model restores the simulations of the user when it is built.
    .batchInitialization = function() {
      private$.objective <- NULL
      private$.data <- NULL
      private$.observedData <- NULL
      private$.model <- SimulationModel$new(
        simulations = private$.simulations,
        parameters = private$.piParameters,
        outputMappings = private$.outputMappings,
        pkMappings = private$.pkMappings,
        simulationRunOptions = private$.configuration$simulationRunOptions
      )
      private$.mirrorModel()
    },

    # The objective of the current public call
    #
    # Built on the first evaluation after `.batchInitialization()`, which
    # reads the observed data once (#271), on the model of the call.
    #
    # @return The `PKObjective` object with PK mappings, otherwise the
    #   `PIObjective` object.
    .objectiveFor = function() {
      if (is.null(private$.objective) && !is.null(private$.pkMappings)) {
        private$.objective <- PKObjective$new(
          model = private$.model,
          pkMappings = private$.pkMappings
        )
      }
      if (is.null(private$.objective)) {
        private$.data <- PIObservations$new(private$.outputMappings)
        private$.objective <- PIObjective$new(
          model = private$.model,
          data = private$.data,
          objectiveFunctionOptions = private$.configuration$objectiveFunctionOptions
        )
      }
      private$.objective
    },

    # The objective of the current public call, if it supports a feature
    #
    # @param feature `"supportsCI"` or `"supportsPlots"`.
    # @param methodName The public method, for the error.
    # @return The objective, or an error when it does not support the feature.
    .objectiveSupporting = function(feature, methodName) {
      objective <- private$.objectiveFor()
      if (!objective[[feature]]) {
        stop(messages$errorMethodNotApplicableInPKMode(methodName))
      }
      objective
    },

    # The evaluation closure around `evaluate()` of the objective
    #
    # Counts the evaluations and the failed evaluations, keeps the last and
    # the best cost summary and prints the evaluation feedback. The
    # `Optimizer` receives this closure as its objective function.
    #
    # @param objective A `PIObjective` or `PKObjective` object.
    # @return `function(currVals)` that returns the cost summary of the
    #   evaluation of the parameter values, a `modelCost` object.
    .evaluationClosure = function(objective) {
      function(currVals) {
        # Increment function evaluations counter
        private$.fnEvaluations <- private$.fnEvaluations + 1

        loggedFailures <- private$.model$loggedFailures
        runningCost <- objective$evaluate(currVals)
        private$.observedData <- objective$observedData
        private$.mirrorModel()

        # The objective logs a failed simulation with the model
        if (private$.model$loggedFailures > loggedFailures) {
          private$.failedEvaluations <- private$.failedEvaluations + 1
          return(runningCost)
        }

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
      }
    },

    # The objective function value of a fresh evaluation closure
    #
    # Every public call that evaluates the objective on a grid counts its
    # evaluations from 1.
    #
    # @param objective A `PIObjective` object.
    # @return `function(values)` that returns the configured cost field of the
    #   evaluation, one numeric.
    .ofvFunction = function(objective) {
      private$.fnEvaluations <- 0
      private$.failedEvaluations <- 0
      evaluate <- private$.evaluationClosure(objective)
      costField <- private$.configuration$modelCostField
      function(values) evaluate(values)[[costField]]
    },

    # Shims for the tests until they are rewritten against the new classes
    # (step 9). The buckets of the model are mirrored after every simulation
    # because the tests read them from the facade.
    .mirrorModel = function() {
      private$.simulationBatches <- private$.model$simulationBatches
      private$.variableParameters <- private$.model$variableParameters
      private$.variableMolecules <- private$.model$variableMolecules
      private$.parameterTargets <- private$.model$parameterTargets
    },
    .applyParameterValues = function(values) {
      private$.model$.__enclos_env__$private$.applyParameterValues(values)
      private$.mirrorModel()
    },
    # The `DataCombined` objects of the plots, one per output mapping
    .evaluate = function(currVals) {
      simulationResults <- private$.model$simulate(currVals)
      private$.mirrorModel()
      .observedVsPredicted(simulationResults, private$.outputMappings)
    },
    .runSimulations = function(
      currVals,
      silentMode = FALSE,
      usedSimulations = NULL
    ) {
      private$.model$simulate(currVals, usedSimulations, silentMode)
    },
    .simulateOutputs = function(
      currVals,
      outputMappings = private$.outputMappings
    ) {
      private$.model$simulatedOutputs(currVals, outputMappings)
    },
    .pkObjectiveFunction = function(currVals) {
      private$.evaluationClosure(private$.objectiveFor())(currVals)$modelCost
    },
    .getPKValues = function(paramValues) {
      private$.objectiveFor()$pkValues(paramValues)
    },
    # `test-pid-aux.R` logs on a task whose batches are not initialized
    .logSimulationFailure = function(currVals, cond) {
      if (is.null(private$.model)) {
        private$.batchInitialization()
      }
      private$.model$logFailure(currVals, cond)
    },
    # The evaluation closure of the objective, on the data of `bootstrapSeed`
    .objectiveFunction = function(currVals, bootstrapSeed = NULL) {
      objective <- private$.objectiveFor()
      if (!is.null(bootstrapSeed)) {
        objective <- objective$withData(private$.data$resample(bootstrapSeed))
      }
      private$.evaluationClosure(objective)(currVals)
    },
    # The output mappings the objective of `bootstrapSeed` evaluates, those of
    # the task without a seed (the frozen oracle reads the observed data from
    # them)
    .getOutputMappings = function(bootstrapSeed = NULL) {
      if (is.null(bootstrapSeed)) {
        return(private$.outputMappings)
      }
      private$.objectiveFor()
      private$.data$resample(bootstrapSeed)$outputMappings
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

    # Only a failed simulation at the start point stops the run. Later failures
    # get the infinite cost.
    # Why: the start point is the one the user vouched for. A success there
    # indirectly indicates that the setup runs elsewhere.
    # Limitation: it does not prove the model is sound, and the start point
    # itself can lie in a region where the simulation fails.
    #
    # @param values Vector of parameter values, one per `PIParameters`.
    .checkStartSimulation = function(values) {
      tryCatch(
        if (is.null(private$.pkMappings)) {
          private$.model$simulatedOutputs(values, private$.outputMappings)
        } else {
          private$.objectiveFor()$pkValues(values)
        },
        simulationsFailedError = function(cond) {
          stop(errorCondition(
            messages$initialSimulationError(conditionMessage(cond)),
            simulationNames = cond$simulationNames,
            failed = cond$failed,
            reasons = cond$reasons,
            class = c("initialSimulationError", "simulationsFailedError"),
            call = NULL
          ))
        }
      )
      invisible()
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

      private$.checkStartSimulation(startValues)

      optimizer <- Optimizer$new(configuration = private$.configuration)

      optimResult <- optimizer$run(
        par = startValues,
        fn = private$.evaluationClosure(private$.objectiveFor()),
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
    #   after its optimization. Nothing can change between the optimization
    #   and the estimation then, so the start values are not checked again.
    #   The caller initializes the batches (see `.batchInitialization()`).
    # @return A `PIResult` object with the confidence intervals, centred on
    #   the estimates of the last `run()`.
    .estimateCI = function(fromRun = FALSE) {
      # Reset function evaluations counter
      private$.fnEvaluations <- 0

      currValues <- private$.lastOptimResult$par
      lower <- sapply(private$.fitParameters, `[[`, "minValue")
      upper <- sapply(private$.fitParameters, `[[`, "maxValue")

      # After `run()`, the estimate was evaluated by the optimizer, and a stop
      # would discard the finished fit
      if (!fromRun) {
        private$.checkStartSimulation(currValues)
      }

      if (private$.configuration$ciMethod == "bootstrap") {
        .validateSubjectCounts(private$.outputMappings)
        .classifyObservedData(private$.outputMappings)
      }

      optimizer <- Optimizer$new(configuration = private$.configuration)

      objective <- private$.objectiveFor()

      ciResult <- optimizer$estimateCI(
        par = currValues,
        fn = private$.evaluationClosure(objective),
        lower = lower,
        upper = upper,
        # Each replicate evaluates the objective on its resampled data, with
        # the evaluation counter starting at 0
        replicate = function(seed) {
          private$.fnEvaluations <- 0
          private$.evaluationClosure(
            objective$withData(private$.data$resample(seed))
          )
        }
      )
      if (private$.configuration$ciMethod == "bootstrap") {
        ciResult$details$subjectCounts <- lapply(
          private$.outputMappings,
          function(mapping) {
            aggregated <- Filter(
              .isAggregatedWithErrors,
              mapping$observedDataSets
            )
            mapping$subjectCount[names(aggregated)]
          }
        )
      }

      PIResult$new(
        optimResult = private$.lastOptimResult,
        ciResult = ciResult,
        costDetails = private$.fitCostDetails,
        configuration = private$.configuration,
        piParameters = private$.fitParameters
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
        .validateSimulationIds(ids, parameters)
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
      # A bootstrap after the run needs the subject counts of the aggregated
      # data: check them before the optimization and before the previous
      # results are cleared
      if (
        is.null(private$.pkMappings) &&
          private$.configuration$autoEstimateCI &&
          private$.configuration$ciMethod == "bootstrap"
      ) {
        .validateSubjectCounts(private$.outputMappings)
      }
      private$.batchInitialization()
      # Clear previous optimization results and diagnostics
      private$.lastOptimResult <- NULL
      private$.fitCostDetails <- NULL
      private$.fitParameters <- NULL
      private$.bestCostSummary <- NULL
      private$.lastCostSummary <- NULL
      # Reset function evaluations counter
      private$.fnEvaluations <- 0
      private$.failedEvaluations <- 0

      # Run optimization algorithm
      optimResult <- private$.runAlgorithm()
      if (private$.failedEvaluations > private$.fnEvaluations / 2) {
        warning(messages$warningMostEvaluationsFailed(
          private$.failedEvaluations,
          private$.fnEvaluations
        ))
      }
      private$.lastOptimResult <- optimResult
      # The confidence interval evaluations below also go through the
      # evaluation closure, so the fit is recorded before they start
      private$.fitCostDetails <- private$.bestCostSummary %||%
        private$.lastCostSummary
      private$.fitParameters <- lapply(
        private$.piParameters,
        function(parameter) parameter$clone()
      )

      achievedPKValues <- if (!is.null(private$.pkMappings)) {
        tryCatch(
          private$.objectiveFor()$pkValues(optimResult$par),
          error = function(e) {
            warning(messages$warnAchievedPKValuesFailure(e$message))
            as.list(rep(NA_real_, length(private$.pkMappings)))
          }
        )
      } else {
        NULL
      }

      private$.applyFinalValues(values = optimResult$par)

      if (!is.null(private$.pkMappings)) {
        piResult <- PKResult$new(
          optimResult = optimResult,
          piParameters = private$.piParameters,
          pkMappings = private$.pkMappings,
          achievedPKValues = achievedPKValues,
          costDetails = private$.fitCostDetails,
          configuration = private$.configuration
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
          costDetails = private$.fitCostDetails,
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
    #'   initial run. The bootstrap method redraws the means of aggregated data
    #'   sets and needs the number of subjects behind them, set with
    #'   `PIOutputMapping$setSubjectCount()`. `vignette("confidence-intervals")`
    #'   explains the resampling.
    #'
    #' @return The same [`PIResult`] object returned by the `run()` method,
    #'   updated to include confidence interval estimates.
    estimateCI = function() {
      private$.batchInitialization()
      private$.objectiveSupporting("supportsCI", "estimateCI")
      if (is.null(private$.lastOptimResult)) {
        stop(messages$errorMissingOptimizationResult())
      }
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
      private$.batchInitialization()
      private$.objectiveSupporting("supportsPlots", "plotResults")

      # Simulate once, with the current values unless `par` is given
      parValues <- unlist(
        lapply(self$parameters, function(x) {
          x$currValue
        }),
        use.names = FALSE
      )
      if (!is.null(par)) {
        parValues <- par
      }
      simulationResults <- private$.model$simulate(parValues)
      private$.mirrorModel()
      .plotObsVsPred(simulationResults, private$.outputMappings)
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

      private$.batchInitialization()
      objective <- private$.objectiveSupporting("supportsPlots", "gridSearch")

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

      parameterNames <- vapply(
        private$.piParameters,
        function(x) x$parameters[[1]]$path,
        character(1)
      )
      ofvGrid <- .ofvGrid(
        private$.ofvFunction(objective),
        lower,
        upper,
        logScaleFlag,
        totalEvaluations,
        parameterNames
      )

      # Set starting point for next round of optimization
      if (setStartValue) {
        bestValues <- unlist(
          ofvGrid[which.min(ofvGrid[["ofv"]]), seq_along(private$.piParameters)],
          use.names = FALSE
        )
        for (idx in seq_along(private$.piParameters)) {
          private$.piParameters[[idx]]$startValue <- bestValues[[idx]]
        }
        message(messages$gridSearchParameterValueSet(bestValues))
      }

      return(ofvGrid)
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

      private$.batchInitialization()
      objective <- private$.objectiveSupporting(
        "supportsPlots",
        "calculateOFVProfiles"
      )

      nrOfParameters <- length(private$.piParameters)

      if (is.null(par) || length(par) != nrOfParameters || !is.numeric(par)) {
        par <- sapply(private$.piParameters, function(x) x$currValue)
      }
      parameterNames <- vapply(
        private$.piParameters,
        function(x) x$parameters[[1]]$path,
        character(1)
      )

      .ofvProfiles(
        private$.ofvFunction(objective),
        par,
        boundFactor,
        totalEvaluations,
        parameterNames
      )
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
