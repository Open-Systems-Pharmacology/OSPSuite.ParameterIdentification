# .applyLogTransformation correctly log-transforms `yValues` and `lloq`

    c(-46.0517018598809, 3.57700696819377, 2.93470564516249, 2.62663228242601, 
    2.47546422676115, 2.34644014139247, 2.2462037226952, 1.96514419807483, 
    1.41794070550117, 0.893480825499678, -0.144933567052714, -2.17573046669903, 
    2.35373806258653, 2.57130535142208, 2.61481878663396, 2.61481878663396, 
    2.26671106685522, 2.15067524343028, 1.75905454027926, 1.17887578765776, 
    0.656714883039828, -0.315084902499181, -2.14264858908876)

---

    c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, -0.693147180559945, -0.693147180559945, -0.693147180559945
    )

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

# calculateCostMetrics with residualWeightingMethod `std` returns expected results

    18576.1565

# calculateCostMetrics with residualWeightingMethod `mean` returns expected results

    36738.0871

# calculateCostMetrics with residualWeightingMethod `error` returns expected results

    642.4931

