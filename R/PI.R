PI <- R6::R6Class(
  classname = "PI",
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
        validateIsOfType(configuration, "PIConfiguration")
        private$.configuration <- value
      }
    },

    #' @field data An object of ospsuite::DataSet class, containing observed
    #' data. Used to run parameter identification
    data = function(value) {
      if (missing(value)) {
        private$.data
      } else {
        validateIsOfType(data, "DataSet")
        private$.data <- value
      }
    }
  ),
  private = list(
    .models = NULL,
    .parameters = NULL,
    .data = NULL,
    .configuration = NULL,
    .stateVariables = NULL,

    .targetFunction = function(p) {
      #' @param p Vector of parameters
      obsVsPred <- private$.evaluate(p)
      if (tolower(private$.configuration$targetFunction) %in% c("lsq", "least squares")) {
        return(private$.LSQ(obsVsPred))
      }
      if (tolower(private$.configuration$targetFunction) %in% c("mle", "likelihood", "maximal likelihood estimation")) {
        return(private$.MLE(obsVsPred))
      }
      warning("Target function not recognized by PI, NA returned")
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
          # change model parameters here
          simulationResult <- ospsuite::runSimulation(model)
          obsVsPred$addSimulationResults(simulationResult, names = model$name, groups = item$name)
        }
      }
      return (obsVsPred)
    },

    .convertToBaseUnits <- function(dimension, value, unit) {
      return(ospsuite::toBaseUnit(ospsuite::ospDimensions[[dimension]], value, unit))
    }

    .LSQ <- function(combinedData) {
      return(combinedData$toDataFrame() %>%
        mutate(xValues_base = pmap_dbl(list(xDimension, xValues, xUnit), .convertToBaseUnits),
               yValues_base = pmap_dbl(list(yDimension, yValues, yUnit), .convertToBaseUnits)) %>%
        select(group, dataType, xValues_base, yValues_base) %>%
        spread(key = dataType, value = yValues_base) %>%
        filter(!is.na(observed) & !is.na(simulated)) %>%
        mutate(residual = (observed - simulated)^2) %>%
        pull(residual) %>%
        sum())
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
    }

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


      results <- FME::modFit(f = private$.targetFunction,
                             p = private$.startValues,
                             lower = ,
                             upper = ,
                             method = ,
                             control = list())
    }
  )
)
