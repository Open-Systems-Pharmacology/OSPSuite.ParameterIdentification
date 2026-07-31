#' @title Internal Message Templates
#'
#' @keywords internal
#' @noRd
messages <- ospsuite.utils::messages

messages$errorDimensionsNotEqual <- function() {
  "All quantities must have the same dimension, but they do not!"
}

messages$errorNoObservedDataSets <- function() {
  "Cannot assign weights: no observed data sets defined."
}

messages$errorWeightsNames <- function() {
  "All weights must be a named list with names matching observed data set names."
}

messages$errorWeightsVectorLengthMismatch <- function(label, expected, actual) {
  sprintf(
    "Weights for '%s' must have length %d matching y-values, but got %d.",
    label,
    expected,
    actual
  )
}

messages$errorDataSetWeightsMismatch <- function() {
  "Dataset weights do not align with observed datasets in output mapping."
}

messages$errorObsVsPredListLengthMismatch <- function(expected, actual) {
  sprintf(
    "Number of combined data entries must be %d to match output mappings, but got %d.",
    expected,
    actual
  )
}

messages$warningDataWeightsPresent <- function() {
  "Data weights have already been set. Check if they are still valid after adding new datasets."
}

messages$errorNotAFunction <- function() {
  "The assigned value must be a function with arguments 'xVals' and 'yVals'!"
}

messages$logSimulationError <- function(values, errorCondition) {
  message("Simulation failed for parameter values: ", toString(values))
  message("Error: ", errorCondition$message)
}

messages$simulationError <- function(values) {
  paste0("Returning infinite cost structure due to simulation failure.")
}

messages$initialSimulationError <- function() {
  "Stopping optimization: Initial simulation failed."
}

messages$profilesNotSupplied <- function() {
  "Supply the result of the calculateOFVProfiles() method as the argument to the plotOFVProfiles() method."
}

messages$plotGridParameterCount <- function(count) {
  paste0(
    "The plotGrid() function requires a data frame with 3 columns, but ",
    count,
    " columns were supplied"
  )
}

messages$gridSearchParameterValueSet <- function(bestValues) {
  cat(
    "Grid search completed.",
    "\n",
    "Starting point for the next optimization updated to parameter values: ",
    "\n",
    paste(signif(bestValues, 4), collapse = " ")
  )
}

messages$logScaleFlagError <- function() {
  "Logarithmic scaling is not available for non-positive parameter values."
}

messages$optimizationAlgorithm <- function(name, par, error = FALSE) {
  if (error) {
    paste0("Unknown optimization algorithm: ", name)
  } else {
    paste0(
      "Starting optimization using '",
      name,
      "' with initial value(s):\n  ",
      paste(.formatValues(par), collapse = ", ")
    )
  }
}

messages$ciMethod <- function(name, par, error = FALSE) {
  if (error) {
    paste0("Unknown CI estimation method: ", name)
  } else {
    paste0(
      "Starting confidence interval estimation using '",
      name,
      "' for parameter value(s):\n  ",
      paste(.formatValues(par), collapse = ", ")
    )
  }
}

messages$evaluationFeedback <- function(fneval, par, objValue) {
  paste0(
    "fneval: ",
    fneval,
    " | parameters: ",
    paste(.formatValues(par), collapse = ", "),
    " | objective: ",
    .formatValues(objValue),
    "\n",
    sep = ""
  )
}

messages$hessianEstimation <- function() {
  "Post-hoc estimation of Hessian matrix."
}

messages$statusAutoEstimateCI <- function() {
  "Skipping confidence interval estimation (autoEstimateCI = FALSE)"
}

messages$errorMissingOptimizationResult <- function() {
  "No optimization result found. Ensure the optimization was run before estimating confidence intervals."
}

messages$errorSimulationIdMissing <- function(
  simulationIds,
  piParamIds,
  outputMappingIds
) {
  message <- utils::capture.output(cat(
    "Mismatch or missing ID detected.\n",
    "Ensure each Simulation ID matches with corresponding PIParameter and OutputMapping IDs.\n",
    "Simulation IDs: ",
    paste(simulationIds, collapse = ", "),
    "\n",
    "PIParameter IDs: ",
    paste(piParamIds, collapse = ", "),
    "\n",
    "OutputMapping IDs: ",
    paste(outputMappingIds, collapse = ", ")
  ))

  return(paste(message, collapse = "\n"))
}

messages$errorObservedDataNotFound <- function(
  caller,
  quantityPath,
  simulationPath
) {
  sprintf(
    "%s: No observed data found for quantity path: \"%s\"\nin simulation: \"%s\"",
    caller,
    quantityPath,
    simulationPath
  )
}

messages$errorNoParentContainer <- function(type) {
  paste0(type, " is not a parent container of entity.")
}

messages$errorUnitConversion <- function(quantityName, observedDataName) {
  paste0(
    "Unit conversion failed for quantity '",
    quantityName,
    "' and observed data '",
    observedDataName,
    "'."
  )
}

messages$fixedParamError <- function(error) {
  if (error == "fixed") {
    "All parameters are fixed! Optimization requires at least one free parameter."
  } else if (error == "length") {
    "`fixedParams$idx` and `fixedParams$values` must have the same length."
  }
}

messages$objectiveFnOutputError <- function(field) {
  paste0("Objective function must return a list containing '", field, "'.")
}

messages$ciEstimationError <- function(step, errorMessage) {
  paste0("Error during CI estimation step '", step, "': ", errorMessage)
}

messages$statusProfileLikelihood <- function(index, value) {
  paste0("Profiling CI for parameter ", index, ": ", value)
}

messages$plMaxiterWarning <- function(index) {
  paste0(
    "maxIter reached for parameter ",
    index,
    " without meeting cost threshold. Setting CI to Inf."
  )
}

messages$statusObservedDataClassification <- function(
  nIndividual,
  nAggregated
) {
  sprintf(
    "Classified observed data: %d individual, %d aggregated dataset(s).",
    nIndividual,
    nAggregated
  )
}

messages$warningLowIndividualData <- function(n = 3) {
  sprintf(
    "Less than %d individual datasets detected - bootstrap CI may be unreliable.",
    n
  )
}

messages$errorUnsupportedErrorType <- function() {
  stop(
    "Unsupported yErrorType: must be 'GeometricStdDev' or 'ArithmeticStdDev'."
  )
}

messages$statusBootstrap <- function(index, nTotal) {
  paste0("Running bootstrap replicate ", index, " of ", nTotal, ".")
}

messages$errorGPRModelConvergence <- function(dataSetName) {
  sprintf(
    "GPR model failed to converge for dataset '%s'.",
    dataSetName
  )
}

messages$statusGPRModelFitted <- function(dataSetName) {
  sprintf(
    "GPR model fitted successfully for dataset '%s'.",
    dataSetName
  )
}

messages$warnParameterMetadata <- function(message) {
  paste0(
    "Could not extract parameter metadata from piParameters: ",
    message
  )
}

messages$errorParameterMetadataMissing <- function() {
  paste0(
    "Parameter metadata is not available. `toDataFrame()` cannot be called ",
    "on a `PIResult` created without `piParameters`."
  )
}

messages$messageOptionsReset <- function(
  field,
  oldValue,
  newValue,
  optionsField
) {
  ospsuite.utils::cliFormat(
    "{.field {field}} changed from {.val {oldValue}} to {.val {newValue}}. {.field {optionsField}} reset to defaults."
  )
}

messages$warningUnknownOptions <- function(keys, fieldName) {
  ospsuite.utils::cliFormat(
    "{cli::qty(length(keys))}Unknown option{?s} for {.field {fieldName}}: {.val {keys}}. Ignored."
  )
}

messages$errorNAValue <- function(argName) {
  ospsuite.utils::cliFormat(
    "{.arg {argName}} must not be {.val NA}."
  )
}

messages$errorNonPositiveValue <- function(argName) {
  ospsuite.utils::cliFormat(
    "{.arg {argName}} must be greater than zero."
  )
}

messages$errorZeroStartValueBounds <- function() {
  ospsuite.utils::cliFormat(
    "Cannot derive optimization bounds from a start value of {.val {0}}. Provide explicit {.arg minValue} and {.arg maxValue} when creating the {.cls PIParameters}."
  )
}

messages$errorInvalidBound <- function(value, startValue) {
  ospsuite.utils::cliFormat(
    "{.arg minValue} and {.arg maxValue} must bracket the start value ({.val {startValue}}) with {.arg minValue} < {.arg maxValue}. Provided bound: {.val {value}}."
  )
}

messages$errorPKMappingUnitConversion <- function(
  pkParameter,
  unit,
  dimension,
  detail
) {
  ospsuite.utils::cliFormat(
    "Incompatible {.arg targetUnit} {.val {unit}} for {.val {pkParameter}} (dimension: {.val {dimension}}). Detail: {detail}"
  )
}

messages$errorPKParameterNotAvailable <- function(
  pkParameter,
  quantityPath,
  detail = NULL
) {
  if (is.null(detail)) {
    ospsuite.utils::cliFormat(
      "{.val {pkParameter}} cannot be computed from simulated data for {.val {quantityPath}}."
    )
  } else {
    ospsuite.utils::cliFormat(
      "{.val {pkParameter}} cannot be computed from simulated data for {.val {quantityPath}}.\nDetail: {detail}"
    )
  }
}

messages$errorPIMixedMappings <- function() {
  ospsuite.utils::cliFormat(
    "Provide either {.arg outputMappings} or {.arg pkOutputMappings}, not both."
  )
}

messages$errorPKMappingsReceivedConfiguration <- function() {
  ospsuite.utils::cliFormat(
    "{.arg pkOutputMappings} received a {.cls PIConfiguration} object. Did you mean to pass it as {.arg configuration}?"
  )
}

messages$errorPINoMappings <- function() {
  ospsuite.utils::cliFormat(
    "Provide either {.arg outputMappings} or {.arg pkOutputMappings}."
  )
}

messages$errorPKMappingsEmpty <- function() {
  ospsuite.utils::cliFormat(
    "{.arg pkOutputMappings} must contain at least one mapping."
  )
}

messages$errorMethodNotApplicableInPKMode <- function(methodName) {
  ospsuite.utils::cliFormat(
    "{.fn {methodName}} is not applicable for PK metric optimization."
  )
}

messages$errorPKMappingSimulationMismatch <- function() {
  ospsuite.utils::cliFormat(
    "All {.cls PKOutputMapping} objects must belong to the simulation passed to {.cls ParameterIdentification}."
  )
}

messages$errorPKMultiIndividualSimulation <- function(
  pkParameter,
  quantityPath,
  n
) {
  ospsuite.utils::cliFormat(
    "Multi-individual simulation results are not supported in PK mode. Expected 1 value for {.val {pkParameter}} at {.val {quantityPath}}, got {n}."
  )
}

messages$errorPKZeroTarget <- function(pkParameter, quantityPath) {
  ospsuite.utils::cliFormat(
    "Target value for {.val {pkParameter}} at {.val {quantityPath}} is zero or negative. Cannot compute relative cost."
  )
}

messages$warnAchievedPKValuesFailure <- function(detail) {
  ospsuite.utils::cliFormat(
    "Post-optimization PK value computation failed. {.field achievedValue} will be {.val NA}. Detail: {detail}"
  )
}

messages$warningNoValidErrorValues <- function() {
  ospsuite.utils::cliFormat(
    "Some error values are invalid and will use unit weights (equivalent to {.val none})."
  )
}

messages$errorNoResidualsToPlot <- function() {
  ospsuite.utils::cliFormat(
    "No residuals to plot: this {.cls modelCost} has no finite residuals."
  )
}

messages$errorObservedDataRemovedByBlq <- function(quantityPath, blqRemove) {
  ospsuite.utils::cliFormat(
    "All observed data for {.val {quantityPath}} was removed by {.arg blqRemove} = {.val {blqRemove}}. No observations remain to fit this mapping."
  )
}

messages$errorObjectiveFunctionTypeRemoved <- function() {
  ospsuite.utils::cliFormat(
    "{.arg objectiveFunctionType} has been removed from {.field objectiveFunctionOptions}.",
    "Use {.arg objectiveType} to select {.val lsq} or {.val mle} scoring.",
    "For censored (M3) handling set {.code objectiveType = \"mle\"} together with {.code blqMethod = \"m3\"}."
  )
}

messages$errorM3RequiresMle <- function(objectiveType) {
  ospsuite.utils::cliFormat(
    "{.code blqMethod = \"m3\"} requires {.code objectiveType = \"mle\"}, but {.arg objectiveType} is {.val {objectiveType}}.",
    "Censoring is a likelihood operation, so it cannot be scored by least squares."
  )
}

messages$errorMleRejectsRobust <- function(robustMethod) {
  ospsuite.utils::cliFormat(
    "{.code objectiveType = \"mle\"} cannot be combined with {.code robustMethod = {.val {robustMethod}}}.",
    "Robust weights can be exactly zero, which makes the likelihood infinite at every parameter value.",
    "Set {.code robustMethod = \"none\"}, or use {.code objectiveType = \"lsq\"}."
  )
}

messages$errorMleRejectsScaleVar <- function() {
  ospsuite.utils::cliFormat(
    "{.code objectiveType = \"mle\"} cannot be combined with {.code scaleVar = TRUE} while {.code residualWeightingMethod = \"error\"}.",
    "There is no concentrated scale for the scale factor to cancel against under a measured standard deviation, so it would inflate the measured standard deviation by the observation count.",
    "Set {.code scaleVar = FALSE}, or use {.code residualWeightingMethod = \"none\"}."
  )
}

messages$errorLsqStrandsM3 <- function() {
  ospsuite.utils::cliFormat(
    "{.code objectiveType = \"lsq\"} is not allowed while {.code blqMethod = \"m3\"}.",
    "Set {.code blqMethod} to {.val none}, {.val lloq}, or {.val lloqHalf} first."
  )
}

messages$errorUnknownErrorModelSource <- function(residualWeightingMethod) {
  ospsuite.utils::cliFormat(
    "No error model is defined for {.code residualWeightingMethod = {.val {residualWeightingMethod}}}."
  )
}

messages$errorUnusableErrorValues <- function(
  quantityPath,
  nNoUsableError,
  nNonPositiveValue
) {
  parts <- c(
    "{.code objectiveType = \"mle\"} with {.code residualWeightingMethod = \"error\"} needs a usable standard deviation on every scored observation, and {.val {quantityPath}} does not provide one everywhere.",
    if (nNoUsableError > 0) {
      "{nNoUsableError} observation{?s} without a usable error value. Supply an error value for every observation, or set {.code residualWeightingMethod = \"none\"} to estimate a single residual standard deviation instead."
    },
    if (nNonPositiveValue > 0) {
      "{nNonPositiveValue} observation{?s} with a value of zero or less and a usable error value. The data-error model turns that error value into a weight through the coefficient of variation, which is undefined at a non-positive value, so such an observation cannot be scored by this model at all. Remove it from the data set, or set {.code residualWeightingMethod = \"none\"}."
    }
  )
  do.call(
    ospsuite.utils::cliFormat,
    c(as.list(parts), list(.envir = environment()))
  )
}

messages$warningAnalyticCiUnderMle <- function(ciMethod) {
  ospsuite.utils::cliFormat(
    "{.arg ciMethod} = {.val {ciMethod}} is not yet corrected for the likelihood scale of {.code objectiveType = \"mle\"}.",
    "The Hessian and profile-likelihood estimators both still apply least-squares formulas, so the reported interval width is not trustworthy under {.val mle}.",
    "Use {.code ciMethod = \"bootstrap\"}, which re-optimizes the same objective instead of reading its curvature."
  )
}

messages$errorNonPositiveWeightsUnderMle <- function(quantityPath) {
  ospsuite.utils::cliFormat(
    "{.val {quantityPath}} carries a dataset weight of zero or less, which {.code objectiveType = \"mle\"} cannot represent.",
    "A zero weight means the residual standard deviation is infinite rather than that the observation is excluded.",
    "Remove the observation from the data set instead, or use {.arg blqRemove} if it is below the quantification limit."
  )
}
