# ParameterIdentification - Core

resetTestFactories()

test_that("ParameterIdentification is created successfully", {
  piConfiguration <- PIConfiguration$new()
  expect_silent(
    piTask <- ParameterIdentification$new(
      simulations = testSimulation(),
      parameters = testParameters(),
      outputMappings = testOutputMapping(),
      configuration = piConfiguration
    )
  )
  expect_s3_class(piTask, class = c("ParameterIdentification", "R6"))
})

test_that("ParameterIdentification read-only fields cannot be modified", {
  piTask <- testPiTask()
  expect_error(piTask$simulations <- testSimulation())
  expect_error(piTask$parameters <- testParameters())
  expect_error(piTask$outputMappings <- testOutputMapping())
})

test_that("ParameterIdentification configuration can be modified without errors", {
  piTask <- testPiTask()
  expect_no_error(piTask$configuration$algorithm <- "HJKB")
  expect_equal(piTask$configuration$algorithm, "HJKB")
  expect_no_error(piTask$configuration$printEvaluationFeedback <- TRUE)
  expect_true(piTask$configuration$printEvaluationFeedback)
  expect_no_error(piTask$configuration$autoEstimateCI <- FALSE)
  expect_false(piTask$configuration$autoEstimateCI)
  expect_no_error(piTask$configuration$algorithmOptions <- list(maxfeval = 3))
  expect_equal(piTask$configuration$algorithmOptions$maxfeval, 3)
  expect_no_error(
    piTask$configuration$objectiveFunctionOptions$robustMethod <- "huber"
  )
  expect_equal(
    piTask$configuration$objectiveFunctionOptions$robustMethod,
    "huber"
  )
  expect_no_error(
    piTask$configuration$blqOptions$linScaleCV <- 0.3
  )
  expect_equal(piTask$configuration$blqOptions$linScaleCV, 0.3)
})

test_that("ParameterIdentification instance prints expected output", {
  piTask <- testPiTask()

  expect_no_error(print(piTask))

  out <- utils::capture.output(print(piTask))
  expect_true(any(grepl("^<ParameterIdentification>$", out)))
  expect_true(any(grepl("Number of parameters:\\s*1", out)))
  expect_true(any(grepl("^Simulations:$", out)))
  expect_true(any(grepl("Aciclovir\\.pkml\\b", out)))
})

test_that("ParameterIdentification errors on missing simulation IDs", {
  simulationMismatch <- loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  )
  expect_error(
    ParameterIdentification$new(
      simulations = simulationMismatch,
      parameters = testParameters(),
      outputMappings = testOutputMapping(),
    ),
    "Mismatch or missing ID detected"
  )
})

test_that("ParameterIdentification errors when PIOutputMapping lacks observed data", {
  expect_error(
    ParameterIdentification$new(
      simulations = testSimulation(),
      parameters = testParameters(),
      outputMappings = testOutputMappingWithoutObsData(),
    ),
    'initialize: No observed data found for quantity path: "Vergin 1995 IV|
    Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
    in simulation: "Vergin 1995 IV"'
  )
})

test_that("ParameterIdentification verifies IDs with multiple simulations and parameters", {
  # no error with multiple simulations and parameter paths
  expect_no_error(
    ParameterIdentification$new(
      simulations = list(sim_250mg, sim_500mg),
      parameters = list(
        piParameterLipo,
        piParameterCl_250mg,
        piParameterCl_500mg
      ),
      outputMappings = list(outputMapping_250mg, outputMapping_500mg),
      configuration = NULL
    )
  )

  # error missing simulation ID
  expect_error(
    ParameterIdentification$new(
      simulations = list(sim_250mg),
      parameters = list(
        piParameterLipo,
        piParameterCl_250mg,
        piParameterCl_500mg
      ),
      outputMappings = list(outputMapping_250mg, outputMapping_500mg)
    ),
    "Mismatch or missing ID detected"
  )
  # error missing parameter ID
  expect_error(
    ParameterIdentification$new(
      simulations = list(sim_250mg, sim_500mg),
      parameters = list(piParameterCl_250mg),
      outputMappings = list(outputMapping_250mg, outputMapping_500mg)
    ),
    "Mismatch or missing ID detected"
  )
  # error missing output mapping ID
  expect_error(
    ParameterIdentification$new(
      simulations = list(sim_250mg, sim_500mg),
      parameters = list(
        piParameterLipo,
        piParameterCl_250mg,
        piParameterCl_500mg
      ),
      outputMappings = list(outputMapping_500mg)
    ),
    "Mismatch or missing ID detected"
  )
})

test_that("ParameterIdentification returns infinite value if simulation fails", {
  testTask <- PISimFailureTester$new(
    simulations = testSimulation(),
    parameters = testParameters(),
    outputMappings = testOutputMapping()
  )

  suppressMessages(
    expect_message(
      piResult <- testTask$run(),
      "Returning infinite cost structure due to simulation failure"
    )
  )

  piResult <- piResult$toList()

  expect_identical(piResult$objectiveValue, Inf)
  expect_false(piResult$convergence)
  expect_true(is.na(piResult$sd))
})

test_that(".objectiveFunction preserves the substitution and M3 costs on LLOQ data", {
  # Substitution path: default blqMethod = "lloqHalf" (blqMethod != "m3" gate)
  taskSub <- testPiTask()
  privSub <- taskSub$.__enclos_env__$private
  privSub$.batchInitialization()
  dsSub <- privSub$.outputMappings[[1]]$observedDataSets[[1]]
  dsSub$LLOQ <- 2.5
  svSub <- sapply(privSub$.piParameters, `[[`, "startValue")
  costSub <- privSub$.objectiveFunction(svSub)$modelCost
  expect_equal(costSub, 870.9881520191, tolerance = 1e-4)

  # M3 path: blqMethod = "m3" with blqOptions sourcing linScaleCV
  taskM3 <- testPiTask()
  privM3 <- taskM3$.__enclos_env__$private
  privM3$.batchInitialization()
  dsM3 <- privM3$.outputMappings[[1]]$observedDataSets[[1]]
  dsM3$LLOQ <- 2.5
  taskM3$configuration$blqMethod <- "m3"
  taskM3$configuration$blqOptions <- list(linScaleCV = 0.2)
  svM3 <- sapply(privM3$.piParameters, `[[`, "startValue")
  costM3 <- privM3$.objectiveFunction(svM3)$modelCost
  expect_equal(costM3, 843.0572708008, tolerance = 1e-4)

  expect_true(costSub != costM3)
})

test_that("blqRemove = 'none' leaves the cost unchanged (no regression)", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  ds <- priv$.outputMappings[[1]]$observedDataSets[[1]]
  ds$LLOQ <- 2.5
  sv <- sapply(priv$.piParameters, `[[`, "startValue")
  cost <- priv$.objectiveFunction(sv)$modelCost
  expect_equal(cost, 870.9881520191, tolerance = 1e-4)
})

test_that("blqRemove = 'always' reduces nObservations relative to 'none'", {
  taskNone <- testPiTask()
  privNone <- taskNone$.__enclos_env__$private
  privNone$.batchInitialization()
  dsNone <- privNone$.outputMappings[[1]]$observedDataSets[[1]]
  dsNone$LLOQ <- 2.5
  svNone <- sapply(privNone$.piParameters, `[[`, "startValue")
  costNone <- privNone$.objectiveFunction(svNone)

  taskAlways <- testPiTask()
  privAlways <- taskAlways$.__enclos_env__$private
  privAlways$.batchInitialization()
  dsAlways <- privAlways$.outputMappings[[1]]$observedDataSets[[1]]
  dsAlways$LLOQ <- 2.5
  taskAlways$configuration$blqRemove <- "always"
  svAlways <- sapply(privAlways$.piParameters, `[[`, "startValue")
  costAlways <- privAlways$.objectiveFunction(svAlways)

  expect_lt(
    costAlways$costVariables$nObservations,
    costNone$costVariables$nObservations
  )
})

test_that("blqRemove filters the observed cache once and reuses it", {
  task <- testPiTask()
  priv <- task$.__enclos_env__$private
  priv$.batchInitialization()
  ds <- priv$.outputMappings[[1]]$observedDataSets[[1]]
  ds$LLOQ <- 2.5
  task$configuration$blqRemove <- "always"
  sv <- sapply(priv$.piParameters, `[[`, "startValue")

  priv$.objectiveFunction(sv)
  rows1 <- nrow(priv$.obsVsPredDfCache[[1]])
  priv$.objectiveFunction(sv)
  rows2 <- nrow(priv$.obsVsPredDfCache[[1]])
  expect_equal(rows1, rows2)
})

# modelFolder <- file.path(testthat::test_path("../dev/Models/Simulations"))
# sim <- loadSimulation(paste0(modelFolder, "/IR_model_doseResponse.pkml"))
# modelParameter <- ospsuite::getParameter(path = "Organism|IR_I_P_Inter_tHalf", container = sim)
#
# ########### Load observed data########
# # Path to the folder where experimental data files are located
# dataFolder <- file.path(testthat::test_path("../Data"))
# # Name of the excel file with experimental data
# dataFile <- "DataSet.xlsx"
# dataSheets <- c("DoseResponse")
#
# importerConfiguration <- ospsuite::loadDataImporterConfiguration(
#   configurationFilePath = file.path(dataFolder, "dataImporter_configuration.xml")
# )
# importerConfiguration$sheets <- dataSheets
#
# dataSets <- ospsuite::loadDataSetsFromExcel(
#   xlsFilePath = file.path(dataFolder, dataFile),
#   importerConfigurationOrPath = importerConfiguration
# )
#
# # Observed values are given as % from maximal observed value. Model ouput is the
# # absolute value. In order to compare observed and simulated values, simulated
# # results must be normalized first. This is achieved by providing a transformation
# # function.
# resultsTransformationFunction <- function(xVals, yVals) {
#   yVals <- yVals / max(yVals) * 100
#   return(list(xVals = xVals, yVals = yVals))
# }
#
# # Define optimization parameter
# piParameter <- PIParameters$new(parameters = modelParameter)
# # Define data mapping
# piOutputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|IRS1_P",
#   container = sim
# ))
# piOutputMapping$transformResultsFunction <- resultsTransformationFunction
# dataSets$`________IRS_P_rel`$yDimension <- ospDimensions$Amount
# piOutputMapping$addObservedDataSets(data = dataSets$`________IRS_P_rel`)
#
# # test_that("It can initialize ParameterIdentification when the simulateSteadyState
# #           is TRUE and the model does not contain any state variable parameters", {
# #   piConfiguration <- PIConfiguration$new()
# #   piConfiguration$simulateSteadyState <- TRUE
# #
# #   # Create new parameter identification.
# #   expect_no_error(pi <- ParameterIdentification$new(
# #     simulations = sim, parameters = piParameter,
# #     outputMappings = piOutputMapping,
# #     configuration = piConfiguration
# #   ))
# # })
