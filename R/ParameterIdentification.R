ParameterIdentification <- R6::R6Class(
  classname = "ParameterIdentification",
  inherit = ospsuite.utils::Printable,
  cloneable = FALSE,
  active = list(
    #' @field models Named list with ospsuite::Simulation objects, where names
    #' are IDs of the root container of the simulation
    models = function(value) {
      if (missing(value)) {
        as.list(private$.models)
      } else {
        stop(messages$errorPropertyReadOnly("models"))
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
        validateIsOfType(value, "PIConfiguration")
        private$.configuration <- value
      }
    },

    #' @field data An object of ospsuite::DataSet class, containing observed
    #' data. Used to run parameter identification
    data = function(value) {
      if (missing(value)) {
        private$.data
      } else {
        validateIsOfType(value, "DataSet")
        private$.data <- value
      }
    },

    #' @field mapping A named list of strings with names corresponding to
    #' datasets and values corresponding to model names
    mapping = function(value) {
      if (missing(value)) {
        private$.mapping
      } else {
        validateIsOfType(value, "list")
        private$.mapping <- value
      }
    },

    #' @field quantities A named list of objects of class Quantity
    #' with names corresponding to datasets and values corresponding to
    #' quantities in PK-Sim models
    quantities = function(value) {
      if (missing(value)) {
        private$.quantities
      } else {
        validateIsOfType(value, "Quantity")
        private$.quantities <- value
      }
    }
  ),
  private = list(
    .models = NULL,
    .parameters = NULL,
    .data = NULL,
    .configuration = NULL,
    .stateVariables = NULL,
    .mapping = NULL,
    .quantities = NULL,
    .iteration = NULL,

    .targetFunction = function(p) {
      #' @param p Vector of parameters
      obsVsPred <- private$.evaluate(p)
      if (tolower(private$.configuration$targetFunction) %in% c("lsq", "least squares")) {
        return(private$.LSQ(obsVsPred))
      }
      if (tolower(private$.configuration$targetFunction) %in% c("mle", "likelihood", "maximal likelihood estimation")) {
        return(private$.MLE(obsVsPred))
      }
      warning("Target function not recognized by ParameterIdentification class, NA returned")
      return(NA_real_)
    },

    .evaluate = function(p) {
      obsVsPred <- DataCombined$new()
      obsVsPred$addDataSets(private$.data, names = names(data), groups = names(data))
      for (i in seq_along(p)) {
        parameter <- private$.parameters[[i]]
        parameter$setValue(p[[i]])
      }
      for (item in private$.data) {
        if (!is.null(private$.mapping[[item$name]])) {
          model <- private$.models[[private$.mapping[[item$name]]]]
          simulationResult <- ospsuite::runSimulation(model)
          obsVsPred$addSimulationResults(simulationResult, names = model$name, groups = item$name)
          obsVsPred$setGroups(names = c(item$name, model$name), groups = c(item$name, item$name)) # ask Indra why this line is needed?
        }
      }
      return (obsVsPred)
    },

    .convertToBaseUnits = function(dimension, value, unit) {
      return(ospsuite::toBaseUnit(ospsuite::ospDimensions[[dimension]], value, unit))
    },

    .LSQ = function(combinedData) {
      sum_residuals <- combinedData$toDataFrame() %>%
        mutate(xValues_base = pmap_dbl(list(xDimension, xValues, xUnit), .convertToBaseUnits),
               yValues_base = pmap_dbl(list(yDimension, yValues, yUnit), .convertToBaseUnits)) %>%
        select(group, dataType, xValues_base, yValues_base) %>%
        filter(!is.na(group)) %>%
        spread(key = dataType, value = yValues_base) %>%
        filter(!is.na(observed) & !is.na(simulated)) %>%
        mutate(residual = (observed - simulated)^2) %>%
        pull(residual) %>%
        sum()
      if (private$.configuration$printIterationFeedback) {
        print(paste0("iter ", private$.iteration, ": target function ", signif(sum_residuals, 3)))
      }
      private$.iteration = private$.iteration + 1
      return(sum_residuals)
    }

  ),
  public = list(
    #' @description
    #' Initialize a new instance of the PI class
    #'
    #' @param models An object or a list of objects of class `Simulation`.
    #' Parameters of the simulation object will be optimized to match the data
    #' @param parameters An object or a list of objects of class `PIParameter`.
    #' These parameters will be optimized
    #' @param configuration Optional. Object of type `PIConfiguration` defining
    #' further options of the parameter identification. If no `PIConfiguration`
    #' is passed, a default one is used
    #' @param data An object of class `DataSet`
    #' @param mapping A named list of strings with names corresponding to
    #' observed data names and values corresponding to simulation names
    #' @param quantities A named list of objects of class `Quantity` with names
    #' corresponding to observed data names
    #' @return A new `ParameterIdentification` object.
    initialize = function(data, models, parameters, configuration, mapping, quantities) {
      ospsuite.utils::validateIsOfType(models, "Simulation")
      ospsuite.utils::validateIsOfType(data, "DataSet")
      ospsuite.utils::validateIsOfType(parameters, "PIParameters")
      ospsuite.utils::validateIsOfType(configuration, "PIConfiguration")
      for (name in names(mapping)) {
        stopifnot(name %in% names(data))
      }
      for (name in names(quantities)) {
        stopifnot(name %in% names(data))
      }
      for (name in mapping) {
        stopifnot(name %in% names(models))
      }
      private$.configuration <- configuration %||% PIConfiguration$new()
      private$.models <- models
      private$.data <- data
      private$.parameters <- parameters
      private$.mapping <- mapping
      private$.quantities <- quantities
      private$.iteration <- 0
    },
    #' @description
    #' Run the parameter identification routine
    #'
    #' @return nothing, but updates values of PIParameter objects
    run = function() {
      for (model in private$.models) {
        ospsuite::clearOutputIntervals(model)
        ospsuite::clearOutputs(model)
      }
      for (item in private$.data) {
        if (!is.null(private$.mapping[[item$name]])) {
          model <- private$.models[[private$.mapping[[item$name]]]]
          xVals <- ospsuite::toBaseUnit(ospsuite::ospDimensions$Time,
                                        values = item$xValues, # need transformation here?
                                        unit = item$xUnit
          )
          model$outputSchema$addTimePoints(xVals)
        }
        if (!is.null(private$.quantities[[item$name]])) {
          model <- private$.models[[private$.mapping[[item$name]]]]
          ospsuite::addOutputs(quantitiesOrPaths = private$.quantities[[item$name]],
                               simulation = model)
        }
      }
      startValues <- unlist(lapply(private$.parameters, function(x) {
        x$startValue
      }), use.names = FALSE)
      lower <- unlist(lapply(private$.parameters, function(x) {
        x$minValue
      }), use.names = FALSE)
      upper <- unlist(lapply(private$.parameters, function(x) {
        x$maxValue
      }), use.names = FALSE)
      results <- FME::modFit(f = private$.targetFunction, p = startValues,
                             lower = lower, upper = upper, method = "bobyqa")
      return(results)
    }
  )
)
