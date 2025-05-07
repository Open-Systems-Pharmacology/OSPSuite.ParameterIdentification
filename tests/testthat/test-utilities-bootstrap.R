test_that(".resampleMappingWeights errors on mismatched weight groups", {
  outputMappings <- list(1)
  mappingWeights <- list()
  expect_error(
    .resampleMappingWeights(outputMappings, mappingWeights, seed = 1),
    messages$errorWeightGroupLengthMismatch(
      length(outputMappings), length(mappingWeights)
    )
  )
})
