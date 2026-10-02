# Reference results of the objective function (#303), with public data only.
#
# Evaluates `.objectiveFunction()` and the public methods of
# `ParameterIdentification` for a set of tasks and settings, and either stores
# the results or compares them with stored results. Use it to check that a
# change of the evaluation keeps every result identical:
#
#   Rscript tests/dev/reference-objective-function.R save <file>
#   Rscript tests/dev/reference-objective-function.R compare <ref> [<file>]
#
# Run it from the package root. Set the environment variable PI_LIB to load
# the package from another library, for example a build of a development
# branch; store the reference with a build of the base commit. `compare`
# reports, for every case, whether the results are identical, the largest
# absolute and relative difference of their numeric parts, and differences in
# the errors, warnings and messages. Cases marked "expected to change" document
# an intended change of behavior. Cases that the reference does not have are
# listed, but not compared. `compare` exits with status 1 if the results or
# the conditions of a case that is not expected to change differ. Run times
# are left out.

piLib <- Sys.getenv("PI_LIB")
if (nzchar(piLib)) {
  .libPaths(c(piLib, .libPaths()))
}
suppressPackageStartupMessages({
  library(ospsuite)
  library(ospsuite.parameteridentification)
})
piNamespace <- asNamespace("ospsuite.parameteridentification")

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1) args[[1]] else ""
if (!mode %in% c("save", "compare") || length(args) < 2) {
  stop(
    "Usage: reference-objective-function.R save <file> | ",
    "compare <reference> [<file>]"
  )
}

# ---- fixtures ----------------------------------------------------------------

testDataPath <- function(fileName) file.path("tests", "data", fileName)
aciclovirFile <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
plasmaPath <- paste0(
  "Organism|PeripheralVenousBlood|Aciclovir|",
  "Plasma (Peripheral Venous Blood)"
)
unboundPath <- paste0(
  "Organism|PeripheralVenousBlood|Aciclovir|",
  "Plasma Unbound (Peripheral Venous Blood)"
)
lipophilicityPath <- "Aciclovir|Lipophilicity"
clearancePath <- paste0(
  "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|",
  "Renal Clearances-TS-Aciclovir|TSspec"
)
stateVariablePath <- "Organism|Lumen|Stomach|Liquid"

newAciclovir <- function() {
  loadSimulation(aciclovirFile, loadFromCache = FALSE, addToCache = FALSE)
}

loadExcelData <- function(filePath, sheets, namingPattern, errors = TRUE) {
  importerConfiguration <- createImporterConfigurationForFile(filePath)
  importerConfiguration$sheets <- sheets
  importerConfiguration$namingPattern <- namingPattern
  if (!errors) {
    importerConfiguration$errorColumn <- NULL
  }
  loadDataSetsFromExcel(filePath, importerConfiguration)
}

# Aggregated data in mg/l (mean and arithmetic SD) for a molar output
laskinData <- function(name = NULL, lloq = NULL) {
  dataSet <- loadExcelData(
    testDataPath("AciclovirLaskinData.xlsx"),
    "Laskin 1982.Group A",
    "{Source}.{Sheet}"
  )[[1]]
  if (!is.null(name)) {
    dataSet$name <- name
  }
  if (!is.null(lloq)) {
    dataSet$LLOQ <- lloq
  }
  dataSet
}

# Five individual profiles in mg/l, without errors
individualData <- function() {
  loadExcelData(
    testDataPath("AciclovirDataIndividuals.xlsx"),
    "Aciclovir.Synthetic",
    "{Source}.{Sheet}.{Subject Id}",
    errors = FALSE
  )
}

syntheticData <- function(
  name,
  xValues,
  yValues,
  yUnit,
  yDimension,
  yErrorValues = NULL,
  yErrorType = NULL
) {
  dataSet <- DataSet$new(name = name)
  dataSet$xDimension <- ospDimensions$Time
  dataSet$xUnit <- "h"
  dataSet$yDimension <- yDimension
  dataSet$yUnit <- yUnit
  dataSet$setValues(
    xValues = xValues,
    yValues = yValues,
    yErrorValues = yErrorValues
  )
  if (!is.null(yErrorType)) {
    dataSet$yErrorType <- yErrorType
  }
  dataSet
}

piParameter <- function(
  simulations,
  path,
  start,
  min,
  max,
  unit = NULL
) {
  parameter <- PIParameters$new(
    parameters = lapply(c(simulations), function(s) getParameter(path, s))
  )
  if (!is.null(unit)) {
    parameter$unit <- unit
  }
  # Start, then max, then min: the bound setters validate against each other
  parameter$startValue <- start
  parameter$maxValue <- max
  parameter$minValue <- min
  parameter
}

outputMapping <- function(simulation, path, dataSets, scaling = "lin") {
  mapping <- PIOutputMapping$new(
    quantity = getQuantity(path, container = simulation)
  )
  mapping$addObservedDataSets(dataSets)
  mapping$scaling <- scaling
  mapping
}

piConfiguration <- function(
  objectiveFunctionOptions = NULL,
  algorithm = "BOBYQA",
  algorithmOptions = NULL,
  ciMethod = "hessian",
  ciOptions = NULL
) {
  configuration <- PIConfiguration$new()
  configuration$autoEstimateCI <- FALSE
  configuration$printEvaluationFeedback <- FALSE
  configuration$simulationRunOptions <- SimulationRunOptions$new(
    numberOfCores = 1,
    showProgress = FALSE
  )
  if (!is.null(objectiveFunctionOptions)) {
    configuration$objectiveFunctionOptions <- objectiveFunctionOptions
  }
  if (algorithm != configuration$algorithm) {
    configuration$algorithm <- algorithm
  }
  if (!is.null(algorithmOptions)) {
    configuration$algorithmOptions <- algorithmOptions
  }
  if (ciMethod != configuration$ciMethod) {
    configuration$ciMethod <- ciMethod
  }
  if (!is.null(ciOptions)) {
    configuration$ciOptions <- ciOptions
  }
  configuration
}

newTask <- function(simulations, parameters, mappings, configuration = NULL) {
  ParameterIdentification$new(
    simulations = simulations,
    parameters = parameters,
    outputMappings = mappings,
    configuration = configuration %||% piConfiguration()
  )
}

privateOf <- function(task) task$.__enclos_env__$private

# Evaluates the objective function for each parameter set, in this order, on
# the same task
evaluateObjective <- function(task, parameterSets, bootstrapSeed = NULL) {
  private <- privateOf(task)
  private$.batchInitialization()
  lapply(parameterSets, function(par) {
    private$.objectiveFunction(par, bootstrapSeed = bootstrapSeed)
  })
}

# A task with one Aciclovir simulation, its lipophilicity as the parameter
# and the Laskin data mapped to the plasma concentration
aciclovirTask <- function(
  scaling = "lin",
  objectiveFunctionOptions = NULL,
  dataSets = laskinData()
) {
  sim <- newAciclovir()
  newTask(
    simulations = sim,
    parameters = piParameter(sim, lipophilicityPath, -0.097, -10, 10),
    mappings = outputMapping(sim, plasmaPath, dataSets, scaling = scaling),
    configuration = piConfiguration(objectiveFunctionOptions)
  )
}
lipophilicitySets <- list(-0.097, 0.2, -0.4, -0.097)

withoutRunTimes <- function(result) {
  result$elapsed <- NULL
  result$ciElapsed <- NULL
  result
}

# ---- cases -----------------------------------------------------------------
# Each case returns a list of results. `expectChange` marks cases whose
# results are expected to differ from the base commit.

cases <- list()

cases$lin <- function() evaluateObjective(aciclovirTask(), lipophilicitySets)
cases$log <- function() {
  evaluateObjective(aciclovirTask("log"), lipophilicitySets)
}

weightedTask <- function(scaling) {
  task <- aciclovirTask(scaling)
  task$outputMappings[[1]]$setDataWeights(
    stats::setNames(
      list(seq(0.5, 2, length.out = 11)),
      names(task$outputMappings[[1]]$observedDataSets)
    )
  )
  task
}
cases$weightsLin <- function() {
  evaluateObjective(weightedTask("lin"), lipophilicitySets)
}
cases$weightsLog <- function() {
  evaluateObjective(weightedTask("log"), lipophilicitySets)
}

transformedTask <- function(scaling) {
  task <- aciclovirTask(scaling)
  task$outputMappings[[1]]$setDataTransformations(
    xOffsets = 0.1,
    xFactors = 1.05,
    yOffsets = 0.05,
    yFactors = 0.9
  )
  task
}
cases$transformationsLin <- function() {
  evaluateObjective(transformedTask("lin"), lipophilicitySets)
}
cases$transformationsLog <- function() {
  evaluateObjective(transformedTask("log"), lipophilicitySets)
}

# Two data sets on one mapping, with data set weights
twoDataSets <- function() {
  dataSet1 <- laskinData(name = "dataSet1")
  dataSet2 <- syntheticData(
    "dataSet2",
    xValues = dataSet1$xValues[-11],
    yValues = 1.5 * dataSet1$yValues[-11],
    yUnit = dataSet1$yUnit,
    yDimension = dataSet1$yDimension
  )
  dataSet2$molWeight <- dataSet1$molWeight
  list(dataSet1, dataSet2)
}
cases$dataSets <- function() {
  task <- aciclovirTask(dataSets = twoDataSets())
  task$outputMappings[[1]]$setDataWeights(
    list(dataSet1 = 2, dataSet2 = seq(1, 0.1, length.out = 10))
  )
  evaluateObjective(task, lipophilicitySets)
}
cases$dataSetsLog <- function() {
  evaluateObjective(
    aciclovirTask("log", dataSets = twoDataSets()),
    lipophilicitySets
  )
}
# Transformations for single data sets fail on the base commit (#311), in the
# batch initialization or in the evaluation. They now apply to the labeled
# data sets
cases$labelledTransformationOne <- structure(
  function() {
    task <- aciclovirTask(dataSets = twoDataSets())
    task$outputMappings[[1]]$setDataTransformations(
      labels = "dataSet2",
      xOffsets = 0.2,
      yFactors = 0.8
    )
    evaluateObjective(task, lipophilicitySets[1])
  },
  expectChange = TRUE
)
# The base commit reports the error of the data transformations as a failed
# simulation ("Initial simulation failed."). The values now apply to the
# labeled data sets
cases$labelledTransformationAll <- structure(
  function() {
    task <- aciclovirTask(dataSets = twoDataSets())
    task$outputMappings[[1]]$setDataTransformations(
      labels = c("dataSet1", "dataSet2"),
      xOffsets = c(0, 0.2),
      yFactors = c(1, 0.8)
    )
    evaluateObjective(task, lipophilicitySets[1])
  },
  expectChange = TRUE
)

# LLOQ: three observations are below 0.5 mg/l
cases$lloqLsqLin <- function() {
  evaluateObjective(
    aciclovirTask(dataSets = laskinData(lloq = 0.5)),
    lipophilicitySets
  )
}
cases$lloqLsqLog <- function() {
  evaluateObjective(
    aciclovirTask("log", dataSets = laskinData(lloq = 0.5)),
    lipophilicitySets
  )
}
cases$lloqM3Lin <- function() {
  evaluateObjective(
    aciclovirTask(
      objectiveFunctionOptions = list(
        objectiveFunctionType = "m3",
        linScaleCV = 0.2
      ),
      dataSets = laskinData(lloq = 0.5)
    ),
    lipophilicitySets
  )
}
cases$lloqM3Log <- function() {
  evaluateObjective(
    aciclovirTask(
      "log",
      objectiveFunctionOptions = list(
        objectiveFunctionType = "m3",
        logScaleSD = 0.086
      ),
      dataSets = laskinData(lloq = 0.5)
    ),
    lipophilicitySets
  )
}

# Error weighting: arithmetic SD (some are missing) and geometric SD
cases$errorArithmetic <- function() {
  evaluateObjective(
    aciclovirTask(
      objectiveFunctionOptions = list(residualWeightingMethod = "error")
    ),
    lipophilicitySets
  )
}
cases$errorGeometric <- function() {
  laskin <- laskinData()
  dataSet <- syntheticData(
    "geometric",
    xValues = laskin$xValues,
    yValues = laskin$yValues,
    yUnit = laskin$yUnit,
    yDimension = laskin$yDimension,
    yErrorValues = seq(1.1, 2, length.out = 11),
    yErrorType = DataErrorType$GeometricStdDev
  )
  dataSet$molWeight <- laskin$molWeight
  evaluateObjective(
    aciclovirTask(
      "log",
      objectiveFunctionOptions = list(residualWeightingMethod = "error"),
      dataSets = dataSet
    ),
    lipophilicitySets
  )
}

cases$huber <- function() {
  evaluateObjective(
    aciclovirTask(objectiveFunctionOptions = list(robustMethod = "huber")),
    lipophilicitySets
  )
}
cases$bisquareLog <- function() {
  evaluateObjective(
    aciclovirTask(
      "log",
      objectiveFunctionOptions = list(robustMethod = "bisquare")
    ),
    lipophilicitySets
  )
}
cases$scaleVar <- function() {
  evaluateObjective(
    aciclovirTask(objectiveFunctionOptions = list(scaleVar = TRUE)),
    lipophilicitySets
  )
}

# Two outputs of one simulation, one of them on a log scale, with molar data
cases$outputs <- function() {
  sim <- newAciclovir()
  unbound <- syntheticData(
    "unbound",
    xValues = c(0.5, 1, 2, 4, 8, 12),
    yValues = c(9, 8, 6, 3, 1.2, 0.5),
    yUnit = "µmol/l",
    yDimension = ospDimensions$`Concentration (molar)`
  )
  task <- newTask(
    simulations = sim,
    parameters = list(
      piParameter(sim, lipophilicityPath, -0.097, -10, 10),
      piParameter(sim, clearancePath, 0.941241, 0.01, 100)
    ),
    mappings = list(
      outputMapping(sim, plasmaPath, laskinData()),
      outputMapping(sim, unboundPath, unbound, scaling = "log")
    )
  )
  evaluateObjective(
    task,
    list(c(-0.097, 0.941241), c(0.3, 0.5), c(-0.4, 2), c(-0.097, 0.941241))
  )
}

# `PIParameters` grouped over two simulations, and a non-base unit (#300)
cases$groupedNonBaseUnit <- function() {
  sims <- list(newAciclovir(), newAciclovir())
  task <- newTask(
    simulations = sims,
    parameters = list(
      piParameter(sims, lipophilicityPath, -0.097, -10, 10),
      piParameter(sims, clearancePath, 6, 0.6, 60, unit = "1/h")
    ),
    mappings = list(
      outputMapping(sims[[1]], plasmaPath, laskinData()),
      outputMapping(sims[[2]], plasmaPath, individualData(), scaling = "log")
    )
  )
  evaluateObjective(
    task,
    list(c(-0.097, 6), c(0.3, 30), c(-0.4, 1), c(-0.097, 6))
  )
}

# A state-variable parameter (#280) together with a constant parameter
cases$stateVariable <- function() {
  sim <- newAciclovir()
  task <- newTask(
    simulations = sim,
    parameters = list(
      piParameter(sim, stateVariablePath, 0.045, 0.01, 0.1),
      piParameter(sim, lipophilicityPath, -0.097, -10, 10)
    ),
    mappings = outputMapping(sim, plasmaPath, laskinData())
  )
  evaluateObjective(
    task,
    list(c(0.045, -0.097), c(0.06, 0.2), c(0.02, -0.4), c(0.045, -0.097))
  )
}

# Bootstrap: aggregated data (values resampled from a GPR model) and
# individual data (weights resampled), then the restored mappings
bootstrapCase <- function(dataSets) {
  task <- aciclovirTask(dataSets = dataSets)
  private <- privateOf(task)
  private$.batchInitialization()
  piNamespace$.classifyObservedData(private$.outputMappings)
  # Fitting the GPR models draws random numbers that the bootstrap seed does
  # not control
  set.seed(2203)
  private$.gprModels <- piNamespace$.prepareGPRModels(private$.outputMappings)
  results <- list()
  for (seed in c(11L, 12L, 11L)) {
    results[[length(results) + 1]] <- private$.objectiveFunction(
      -0.2,
      bootstrapSeed = seed
    )
  }
  private$.restoreOutputMappingsState()
  results[[length(results) + 1]] <- private$.objectiveFunction(-0.2)
  results
}
cases$bootstrapAggregated <- function() bootstrapCase(laskinData())
cases$bootstrapIndividual <- function() bootstrapCase(individualData())

# Other models: Midazolam (molar data) and two Clarithromycin simulations
# (mass data) with parameters grouped over both
cases$midazolam <- function() {
  sim <- loadSimulation(
    testDataPath("Midazolam_Smith_1981_iv_5mg.pkml"),
    loadFromCache = FALSE,
    addToCache = FALSE
  )
  data <- loadExcelData(
    testDataPath("Midazolam_Smith_1981.xlsx"),
    "Smith1981",
    "{Source}.{Sheet}"
  )
  task <- newTask(
    simulations = sim,
    parameters = list(
      piParameter(sim, "Midazolam|Lipophilicity", 3.9, -10, 10),
      piParameter(
        sim,
        "Midazolam-CYP3A4-Patki et al. 2003 rCYP3A4|kcat",
        320,
        0,
        3200
      )
    ),
    mappings = outputMapping(
      sim,
      paste0(
        "Organism|PeripheralVenousBlood|Midazolam|",
        "Plasma (Peripheral Venous Blood)"
      ),
      data,
      scaling = "log"
    )
  )
  evaluateObjective(task, list(c(3.9, 320), c(3, 600), c(3.9, 320)))
}
cases$clarithromycin <- function() {
  files <- c(
    IV250 = "Clarithromycin_Chu_1992_iv_250mg.pkml",
    PO250 = "Clarithromycin_Chu_1993_po_250mg.pkml"
  )
  sims <- lapply(files, function(f) {
    loadSimulation(
      file.path("inst", "extdata", f),
      loadFromCache = FALSE,
      addToCache = FALSE
    )
  })
  data <- loadExcelData(
    file.path("inst", "extdata", "Clarithromycin_Profiles.xlsx"),
    names(files),
    "{Sheet}"
  )
  outputPath <- paste0(
    "Organism|PeripheralVenousBlood|Clarithromycin|",
    "Plasma (Peripheral Venous Blood)"
  )
  task <- newTask(
    simulations = sims,
    parameters = list(
      piParameter(sims, "Clarithromycin-CYP3A4-fit|kcat", 10, 0, 100),
      piParameter(
        sims,
        paste0(
          "Neighborhoods|Kidney_pls_Kidney_ur|Clarithromycin|",
          "Renal Clearances-fitted|Specific clearance"
        ),
        10,
        0,
        100
      )
    ),
    mappings = lapply(names(files), function(n) {
      outputMapping(sims[[n]], outputPath, data[[n]])
    })
  )
  evaluateObjective(task, list(c(10, 10), c(20, 5), c(10, 10)))
}

# Settings changed between two public calls, which all start with the batch
# initialization
cases$settingsBetweenCalls <- function() {
  task <- aciclovirTask()
  first <- evaluateObjective(task, lipophilicitySets[1:2])
  mapping <- task$outputMappings[[1]]
  mapping$scaling <- "log"
  mapping$setDataWeights(
    stats::setNames(
      list(seq(2, 0.5, length.out = 11)),
      names(mapping$observedDataSets)
    )
  )
  task$configuration$objectiveFunctionOptions <- list(robustMethod = "huber")
  c(first, evaluateObjective(task, lipophilicitySets[1:2]))
}
# A change of the data transformations between two evaluations, with a batch
# initialization in between, as at the start of a public call. The base
# commit keeps the observed data it read until run() or the end of
# estimateCI(), so it ignores the change
cases$transformationsBetweenCalls <- structure(
  function() {
    task <- aciclovirTask()
    first <- evaluateObjective(task, lipophilicitySets[1])
    task$outputMappings[[1]]$setDataTransformations(yFactors = 0.8)
    c(first, evaluateObjective(task, lipophilicitySets[1]))
  },
  expectChange = TRUE
)

# A data set added between two calls, at times that were not output time
# points, because these are set at the first call: with least squares, times
# after the last simulated time, where the cost is infinite, with a warning
# that says why, and with M3, censored values at times inside the simulated
# times, which are interpolated (#320). The base commit keeps the observed
# data it read until run() or the end of estimateCI(), so it ignores the new
# data set
cases$observedTimesBetweenCalls <- structure(
  function() {
    laterData <- function(xValues, yValues, lloq = NULL) {
      laskin <- laskinData()
      dataSet <- syntheticData(
        "later",
        xValues = xValues,
        yValues = yValues,
        yUnit = laskin$yUnit,
        yDimension = laskin$yDimension
      )
      dataSet$molWeight <- laskin$molWeight
      if (!is.null(lloq)) {
        dataSet$LLOQ <- lloq
      }
      dataSet
    }
    withNewData <- function(task, dataSet) {
      first <- evaluateObjective(task, lipophilicitySets[1])
      task$outputMappings[[1]]$addObservedDataSets(dataSet)
      c(first, evaluateObjective(task, lipophilicitySets[1]))
    }
    list(
      lsq = withNewData(
        aciclovirTask(),
        laterData(c(1.62, 25, 50), c(1, 0.05, 0.01))
      ),
      m3 = withNewData(
        aciclovirTask(
          objectiveFunctionOptions = list(
            objectiveFunctionType = "m3",
            linScaleCV = 0.2
          ),
          dataSets = laskinData(lloq = 0.5)
        ),
        laterData(c(1.62, 3.21, 16.87), rep(0.1, 3), lloq = 0.5)
      )
    )
  },
  expectChange = TRUE
)

# A first simulation that fails. The base commit shows the warning of the
# simulation engine and logs a type error about `NULL`; the failed simulation
# is now logged by name, with the reason from the engine, without the warning
cases$failingSimulation <- structure(
  function() {
    task <- aciclovirTask()
    simulation <- task$simulations[[1]]
    simulation$solver$mxStep <- 1
    evaluateObjective(task, lipophilicitySets[1])
  },
  expectChange = TRUE
)

# ---- public methods ----------------------------------------------------------

runTask <- function(configuration) {
  sim <- newAciclovir()
  newTask(
    simulations = sim,
    parameters = list(
      piParameter(sim, lipophilicityPath, -0.097, -10, 10),
      piParameter(sim, clearancePath, 0.941241, 0.01, 100)
    ),
    mappings = outputMapping(sim, plasmaPath, laskinData(), scaling = "log"),
    configuration = configuration
  )
}

cases$runAndHessianCI <- function() {
  task <- runTask(piConfiguration(
    algorithmOptions = list(maxeval = 20),
    ciOptions = list(r = 2L)
  ))
  list(
    run = withoutRunTimes(task$run()$toList()),
    ci = withoutRunTimes(task$estimateCI()$toList())
  )
}
cases$runHJKBAndProfileCI <- function() {
  task <- runTask(piConfiguration(
    algorithm = "HJKB",
    algorithmOptions = list(maxfeval = 15),
    ciMethod = "PL",
    ciOptions = list(maxIter = 2L)
  ))
  list(
    run = withoutRunTimes(task$run()$toList()),
    ci = withoutRunTimes(task$estimateCI()$toList())
  )
}
cases$runAndBootstrapCI <- function() {
  task <- aciclovirTask(dataSets = individualData())
  task$configuration <- piConfiguration(
    algorithmOptions = list(maxeval = 3),
    ciMethod = "bootstrap",
    ciOptions = list(nBootstrap = 3L, seed = 2203L)
  )
  list(
    run = withoutRunTimes(task$run()$toList()),
    ci = withoutRunTimes(task$estimateCI()$toList())
  )
}
cases$gridSearchAndProfiles <- function() {
  task <- runTask(piConfiguration())
  list(
    grid = task$gridSearch(totalEvaluations = 9),
    profiles = task$calculateOFVProfiles(totalEvaluations = 3L)
  )
}
cases$plotResults <- function() {
  sim <- newAciclovir()
  unbound <- syntheticData(
    "unbound",
    xValues = c(0.5, 1, 2, 4, 8, 12),
    yValues = c(9, 8, 6, 3, 1.2, 0.5),
    yUnit = "µmol/l",
    yDimension = ospDimensions$`Concentration (molar)`
  )
  task <- newTask(
    simulations = sim,
    parameters = piParameter(sim, lipophilicityPath, -0.097, -10, 10),
    mappings = list(
      outputMapping(sim, plasmaPath, laskinData()),
      outputMapping(sim, unboundPath, unbound, scaling = "log")
    )
  )
  plots <- task$plotResults(par = 0.1)
  # The data of every layer of every sub-plot; a patchwork holds the first
  # sub-plots in `$patches` and builds as its last one
  lapply(plots, function(plot) {
    lapply(c(plot$patches$plots, list(plot)), function(p) {
      ggplot2::ggplot_build(p)$data
    })
  })
}

# ---- PK metric mode ----------------------------------------------------------

dosePath <- paste0(
  "Events|IV 250mg 10min|No formulation|Application_1|",
  "ProtocolSchemaItem|Dose"
)

# A task that fits the dose of Aciclovir to a target C_max
pkTask <- function(mxStep = NULL) {
  sim <- newAciclovir()
  if (!is.null(mxStep)) {
    sim$solver$mxStep <- mxStep
  }
  quantity <- getQuantity(plasmaPath, container = sim)
  ParameterIdentification$new(
    simulations = sim,
    parameters = piParameter(sim, dosePath, 2.5e-4, 1e-4, 1e-3),
    pkOutputMappings = PKOutputMapping$new(
      quantity = quantity,
      pkParameter = "C_max",
      targetValue = 30,
      targetUnit = quantity$unit
    ),
    configuration = piConfiguration(algorithmOptions = list(maxeval = 5))
  )
}
doseSets <- list(2.5e-4, 1e-4, 6e-4, 2.5e-4)

cases$pkObjective <- function() {
  private <- privateOf(pkTask())
  private$.batchInitialization()
  lapply(doseSets, function(dose) {
    list(
      pkValues = private$.getPKValues(dose),
      cost = private$.pkObjectiveFunction(dose)
    )
  })
}
cases$pkRun <- function() withoutRunTimes(pkTask()$run()$toList())
# A simulation that fails in PK mode: the first evaluation stops, the later
# ones return the largest cost. The base commit stops with a type error about
# `NULL` and shows the warning of the simulation engine on every evaluation
cases$pkFailingSimulation <- structure(
  function() {
    private <- privateOf(pkTask(mxStep = 1))
    private$.batchInitialization()
    first <- tryCatch(
      private$.pkObjectiveFunction(doseSets[[1]]),
      error = conditionMessage
    )
    list(
      first = first,
      later = lapply(doseSets[2:3], private$.pkObjectiveFunction)
    )
  },
  expectChange = TRUE
)
# A failing simulation without a PK mapping, which shares the dose with the
# simulation of the PK mapping: the warning of the simulation engine is shown,
# and the cost is that of the simulation of the PK mapping
cases$pkUnmappedFailingSimulation <- function() {
  sims <- list(newAciclovir(), newAciclovir())
  sims[[2]]$solver$mxStep <- 1
  quantity <- getQuantity(plasmaPath, container = sims[[1]])
  task <- ParameterIdentification$new(
    simulations = sims,
    parameters = piParameter(sims, dosePath, 2.5e-4, 1e-4, 1e-3),
    pkOutputMappings = PKOutputMapping$new(
      quantity = quantity,
      pkParameter = "C_max",
      targetValue = 30,
      targetUnit = quantity$unit
    ),
    configuration = piConfiguration(algorithmOptions = list(maxeval = 5))
  )
  private <- privateOf(task)
  private$.batchInitialization()
  list(
    costs = lapply(doseSets, private$.pkObjectiveFunction),
    run = withoutRunTimes(task$run()$toList())
  )
}

# ---- run and compare -------------------------------------------------------

runCase <- function(case) {
  conditions <- character()
  value <- withCallingHandlers(
    tryCatch(case(), error = function(e) {
      structure(list(error = conditionMessage(e)), class = "caseError")
    }),
    warning = function(w) {
      conditions <<- c(conditions, paste("warning:", conditionMessage(w)))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      conditions <<- c(conditions, paste("message:", conditionMessage(m)))
      invokeRestart("muffleMessage")
    }
  )
  list(value = value, conditions = conditions)
}

results <- list()
for (name in names(cases)) {
  t0 <- proc.time()[["elapsed"]]
  results[[name]] <- runCase(cases[[name]])
  cat(sprintf("%-30s %6.1f s\n", name, proc.time()[["elapsed"]] - t0))
}
results$.versions <- list(
  ospsuite = format(packageVersion("ospsuite")),
  pi = format(packageVersion("ospsuite.parameteridentification")),
  piLibrary = dirname(find.package("ospsuite.parameteridentification")),
  R = R.version.string
)

# All numeric values of a result, with their positions as names
numericLeaves <- function(x) {
  if (inherits(x, "ggplot") || is.environment(x) || is.function(x)) {
    return(numeric())
  }
  if (is.list(x)) {
    parts <- lapply(seq_along(x), function(i) {
      leaves <- numericLeaves(x[[i]])
      key <- names(x)[i]
      if (is.null(key) || !nzchar(key)) {
        key <- as.character(i)
      }
      if (length(leaves)) {
        names(leaves) <- paste0(key, "/", names(leaves))
      }
      leaves
    })
    return(unlist(parts))
  }
  if (is.numeric(x) && !is.factor(x)) {
    values <- as.numeric(x)
    names(values) <- seq_along(values)
    return(values)
  }
  numeric()
}

compareCase <- function(reference, current) {
  a <- numericLeaves(reference$value)
  b <- numericLeaves(current$value)
  sameShape <- identical(names(a), names(b))
  maxAbs <- NA_real_
  maxRel <- NA_real_
  if (sameShape && length(a)) {
    both <- is.finite(a) & is.finite(b)
    sameSpecial <- identical(is.finite(a), is.finite(b)) &&
      identical(a[!both], b[!both])
    diff <- abs(a[both] - b[both])
    maxAbs <- if (length(diff)) max(diff) else 0
    rel <- diff / abs(a[both])
    rel <- rel[abs(a[both]) > 0]
    maxRel <- if (length(rel)) max(rel) else 0
    if (!sameSpecial) maxAbs <- maxRel <- Inf
  }
  data.frame(
    identical = identical(reference$value, current$value),
    allEqual = isTRUE(all.equal(reference$value, current$value)),
    numbers = length(b),
    maxAbsDiff = maxAbs,
    maxRelDiff = maxRel,
    sameConditions = identical(reference$conditions, current$conditions)
  )
}

if (mode == "save") {
  saveRDS(results, args[[2]])
  cat("written", args[[2]], "\n")
} else {
  reference <- readRDS(args[[2]])
  if (length(args) >= 3) {
    saveRDS(results, args[[3]])
  }
  cat(
    "reference: ospsuite.parameteridentification",
    reference$.versions$pi,
    "from",
    reference$.versions$piLibrary,
    "\n"
  )
  cat(
    "current:   ospsuite.parameteridentification",
    results$.versions$pi,
    "from",
    results$.versions$piLibrary,
    "\n\n"
  )
  # A reference stored before a case was added does not have it
  compared <- intersect(names(cases), names(reference))
  table <- do.call(
    rbind,
    lapply(compared, function(name) {
      row <- compareCase(reference[[name]], results[[name]])
      row$expectChange <- isTRUE(attr(cases[[name]], "expectChange"))
      cbind(case = name, row)
    })
  )
  options(width = 150)
  print(table, row.names = FALSE, digits = 3)
  notInReference <- setdiff(names(cases), compared)
  if (length(notInReference)) {
    cat(
      "\nCases that are not in the reference:",
      paste(notInReference, collapse = ", "),
      "\n"
    )
  }
  for (name in compared) {
    if (!identical(reference[[name]]$conditions, results[[name]]$conditions)) {
      cat("\nConditions of", name, "\n  reference:\n")
      cat(paste0("    ", unique(reference[[name]]$conditions)), sep = "\n")
      cat("  current:\n")
      cat(paste0("    ", unique(results[[name]]$conditions)), sep = "\n")
    }
  }
  # A change of the errors, warnings or messages is a change, too
  unexpected <- table$case[
    !(table$identical & table$sameConditions) & !table$expectChange
  ]
  cat(
    "\nCases with other results or conditions, not expected to change:",
    if (length(unexpected)) paste(unexpected, collapse = ", ") else "none",
    "\n"
  )
  if (length(unexpected)) {
    quit(status = 1)
  }
}
