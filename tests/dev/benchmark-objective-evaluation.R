# Benchmark of one objective function evaluation (#303), with public data
# only: the Aciclovir model that ships with ospsuite, loaded nSim times, nPar
# constant organ parameters shared by all copies (one `PIParameters` object
# with nSim paths each), and one output mapping with one observation per copy.
#
#   Rscript tests/dev/benchmark-objective-evaluation.R <nSim> <nPar> <nEval> \
#     <numberOfCores> [outFile]
#
# Set the environment variable PI_LIB to load the package from another
# library, for example a build of a development branch. The script reports
# the median time of nEval evaluations at the start values, and of their
# parts:
#
# - applyParameterValues is `.applyParameterValues()`;
# - getSimulationContainer is the lookup of the simulation of every parameter
#   path on every evaluation, for a build of the package that does it there
#   (NA for a build that resolves the paths once per run);
# - simulation is `addRunValues()` and `runSimulationBatches()`;
# - simulatedData is the conversion of the simulation results into the
#   simulated values of every output mapping (`DataCombined` objects or
#   numeric vectors, depending on the build);
# - costAndDataFrames is the rest of the evaluation (observed data, unit
#   conversion, LLOQ handling, cost calculation and result data frames).
#
# The parts are timed separately, so they do not add up exactly to the full
# evaluation. The script calls private methods through
# `task$.__enclos_env__$private`.

piLib <- Sys.getenv("PI_LIB")
if (nzchar(piLib)) {
  .libPaths(c(piLib, .libPaths()))
}
suppressPackageStartupMessages({
  library(ospsuite)
  library(ospsuite.parameteridentification)
})

args <- commandArgs(trailingOnly = TRUE)
argOr <- function(i, default) if (length(args) >= i) args[[i]] else default
nSim <- as.integer(argOr(1, "66"))
nPar <- as.integer(argOr(2, "20"))
nEval <- as.integer(argOr(3, "5"))
cores <- as.integer(argOr(4, "2"))
outFile <- argOr(5, "")
now <- function() proc.time()[["elapsed"]]

simFile <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
outputPath <- paste0(
  "Organism|PeripheralVenousBlood|Aciclovir|",
  "Plasma (Peripheral Venous Blood)"
)

t0 <- now()
sims <- lapply(seq_len(nSim), function(i) {
  loadSimulation(simFile, loadFromCache = FALSE, addToCache = FALSE)
})

# nPar constant, positive, organ-level parameters of the model
allPar <- getAllParametersMatching("Organism|*|*", sims[[1]])
keep <- vapply(
  allPar,
  function(p) {
    isTRUE(p$formula$isConstant) &&
      !p$isStateVariable &&
      is.finite(p$value) &&
      p$value > 0
  },
  logical(1)
)
parPaths <- head(vapply(allPar[keep], function(p) p$path, character(1)), nPar)

piParameters <- lapply(parPaths, function(path) {
  params <- lapply(sims, function(s) getParameter(path, s))
  pp <- PIParameters$new(parameters = params)
  pp$minValue <- pp$startValue / 10
  pp$maxValue <- pp$startValue * 10
  pp
})

# One observation per simulation: the simulated value at 120 min, times 1.1
res <- runSimulations(sims[[1]])[[1]]
ov <- getOutputValues(res, quantitiesOrPaths = outputPath)
yAt120 <- stats::approx(ov$data$Time, ov$data[[outputPath]], xout = 120)$y
quantity <- getQuantity(outputPath, sims[[1]])

outputMappings <- lapply(seq_len(nSim), function(i) {
  ds <- DataSet$new(name = paste0("obs_", i))
  ds$setValues(xValues = 120, yValues = yAt120 * 1.1)
  ds$xDimension <- ospDimensions$Time
  ds$xUnit <- "min"
  ds$yDimension <- quantity$dimension
  ds$yUnit <- quantity$unit
  m <- PIOutputMapping$new(quantity = getQuantity(outputPath, sims[[i]]))
  m$addObservedDataSets(ds)
  m
})

cfg <- PIConfiguration$new()
cfg$autoEstimateCI <- FALSE
cfg$simulationRunOptions <- SimulationRunOptions$new(
  numberOfCores = cores,
  showProgress = FALSE
)
task <- ParameterIdentification$new(
  simulations = sims,
  parameters = piParameters,
  outputMappings = outputMappings,
  configuration = cfg
)
tBuild <- now() - t0

priv <- task$.__enclos_env__$private
priv$.batchInitialization()
start <- vapply(piParameters, function(p) p$startValue, numeric(1))
# The first evaluation also reads the observed data
invisible(priv$.objectiveFunction(start))

timeIt <- function(expr) {
  expr <- substitute(expr)
  env <- parent.frame()
  times <- vapply(
    seq_len(nEval),
    function(i) {
      ts <- now()
      eval(expr, env)
      now() - ts
    },
    numeric(1)
  )
  stats::median(times)
}

runBatches <- function() {
  for (simId in names(priv$.simulationBatches)) {
    priv$.simulationBatches[[simId]]$addRunValues(
      parameterValues = unlist(
        priv$.variableParameters[[simId]],
        use.names = FALSE
      ),
      initialValues = unlist(
        priv$.variableMolecules[[simId]],
        use.names = FALSE
      )
    )
  }
  runSimulationBatches(
    priv$.simulationBatches,
    simulationRunOptions = cfg$simulationRunOptions,
    silentMode = TRUE
  )
}

# Paths are resolved once per run when the task caches their targets
resolvesPathsPerEvaluation <- !exists(
  ".parameterTargets",
  envir = priv,
  inherits = FALSE
)

evaluation <- timeIt(priv$.objectiveFunction(start))
applyParameterValues <- timeIt(priv$.applyParameterValues(start))
getSimulationContainer <- if (resolvesPathsPerEvaluation) {
  timeIt(
    for (p in piParameters) {
      for (x in p$parameters) {
        ospsuite.parameteridentification:::.getSimulationContainer(x)
      }
    }
  )
} else {
  NA_real_
}
simulation <- timeIt(runBatches())
# Parameter values, simulation and simulated values of every output mapping
simulated <- if (exists(".simulateOutputs", envir = priv, inherits = FALSE)) {
  timeIt(priv$.simulateOutputs(start))
} else {
  timeIt(priv$.evaluate(start, includeObserved = FALSE))
}

out <- data.frame(
  nSim = nSim,
  nPar = nPar,
  parameterPaths = nSim * nPar,
  cores = cores,
  buildSeconds = round(tBuild, 1),
  evaluation = evaluation,
  applyParameterValues = applyParameterValues,
  getSimulationContainer = getSimulationContainer,
  simulation = simulation,
  simulatedData = simulated - applyParameterValues - simulation,
  costAndDataFrames = evaluation - simulated,
  ospsuite = format(packageVersion("ospsuite")),
  pi = format(packageVersion("ospsuite.parameteridentification")),
  piLibrary = dirname(find.package("ospsuite.parameteridentification"))
)
print(t(out))
cat(sprintf(
  "ospsuite %s, ospsuite.parameteridentification %s from %s, %s\n",
  out$ospsuite,
  out$pi,
  out$piLibrary,
  R.version.string
))
if (nzchar(outFile)) {
  append <- file.exists(outFile)
  utils::write.table(
    out,
    outFile,
    sep = ",",
    row.names = FALSE,
    col.names = !append,
    append = append
  )
}
