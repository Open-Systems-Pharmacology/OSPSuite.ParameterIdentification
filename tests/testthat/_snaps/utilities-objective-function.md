# calculateCostMetrics returns correct cost metric values for default parameters

    structure(list(modelCost = 677.390283322667, minLogProbability = 348.803465526585, 
        costVariables = structure(list(nObservations = 11L, M3Contribution = 0, 
            rawSSR = 677.390283322667, weightedSSR = 677.390283322667), class = "data.frame", row.names = c(NA, 
        -1L)), residualDetails = structure(list(index = c(NA_real_, 
        NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, 
        NA_real_, NA_real_, NA_real_, NA_real_), x = c(16.36363792, 
        29.09090996, 45.45454788, 69.09089661, 96.36366272, 118.1818237, 
        180, 301.8181763, 420, 658.1820068, 1141.817993), yObserved = c(10.52483879, 
        13.08289107, 13.66473992, 13.66473992, 9.647618206, 8.590657224, 
        5.806944566, 3.250717651, 1.928446744, 0.729726916, 0.117343636
        ), ySimulated = c(35.76633072, 18.81596375, 13.82712555, 
        11.8872242, 10.44830894, 9.451786041, 7.135941505, 4.128609657, 
        2.443620682, 0.865079761, 0.113525197), scaleFactor = c(1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1), errorWeights = c(1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1), robustWeights = c(1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1), userWeights = c(1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1), totalWeights = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1), rawResiduals = c(25.24149193, 5.73307268, 0.162385629999999, 
        -1.77751572, 0.800690734, 0.861128817000001, 1.328996939, 
        0.877892006, 0.515173938, 0.135352845, -0.00381843900000001
        ), weightedResiduals = c(25.24149193, 5.73307268, 0.162385629999999, 
        -1.77751572, 0.800690734, 0.861128817000001, 1.328996939, 
        0.877892006, 0.515173938, 0.135352845, -0.00381843900000001
        )), class = "data.frame", row.names = c(NA, -11L))), class = "modelCost")

# calculateCostMetrics with residualWeightingMethod `none` returns expected results

    677.3903

# calculateCostMetrics with residualWeightingMethod `error` returns expected results

    642.4931

# .computeErrorWeights stops when its inputs differ in length

    Code
      .computeErrorWeights(yValues = yValues, yErrorValues = c(1, 2), yErrorType = rep(
        "ArithmeticStdDev", 3))
    Condition
      Error in `ospsuite.utils::validateIsSameLength()`:
      ! Arguments "yValues, yErrorValues" must have the same length, but they don't!

---

    Code
      .computeErrorWeights(yValues = yValues, yErrorValues = c(1, 2, 3), yErrorType = rep(
        "ArithmeticStdDev", 2))
    Condition
      Error in `ospsuite.utils::validateIsSameLength()`:
      ! Arguments "yValues, yErrorType" must have the same length, but they don't!

