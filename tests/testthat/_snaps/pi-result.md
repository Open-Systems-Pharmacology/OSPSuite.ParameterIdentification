# toList() produces expected output

    Code
      piResult$toList()
    Output
      $finalParameters
      [1] 1.313491
      
      $objectiveValue
      [1] 156.2675
      
      $initialParameters
      [1] -0.097
      
      $convergence
      [1] TRUE
      
      $algorithm
      [1] "BOBYQA"
      
      $elapsed
      [1] 0
      
      $iterations
      [1] 10
      
      $fnEvaluations
      [1] 10
      
      $ciMethod
      [1] "hessian"
      
      $ciElapsed
      [1] 0
      
      $ciError
      NULL
      
      $ciDetails
      $ciDetails$hessian
               [,1]
      [1,] 726.9775
      
      $ciDetails$covMat
                 [,1]
      [1,] 0.04299102
      
      $ciDetails$eigen
      [1] 0.04299102
      
      $ciDetails$corMat
           [,1]
      [1,]    1
      
      
      $sd
      [1] 0.2073428
      
      $cv
      [1] 0.1578562
      
      $lowerCI
      [1] 0.9071067
      
      $upperCI
      [1] 1.719875
      
      $paramNames
      [1] "Lipophilicity"
      
      $costDetails
      $modelCost
      [1] 156.2675
      
      $minLogProbability
      [1] 88.24208
      
      $costVariables
        nObservations M3Contribution   rawSSR weightedSSR
      1            11              0 156.2675    156.2675
      
      $residualDetails
         index          x  yObserved ySimulated scaleFactor errorWeights
      1      1   16.36364 10.5248388 15.9581556           1            1
      2      1   29.09091 13.0828911 10.3704977           1            1
      3      1   45.45455 13.6647399  8.2325163           1            1
      4      1   69.09090 13.6647399  6.6754422           1            1
      5      1   96.36366  9.6476182  5.4353223           1            1
      6      1  118.18182  8.5906572  4.6898856           1            1
      7      1  180.00000  5.8069446  3.3294461           1            1
      8      1  301.81818  3.2507177  2.1837802           1            1
      9      1  420.00000  1.9284467  1.7249416           1            1
      10     1  658.18201  0.7297269  1.2656908           1            1
      11     1 1141.81799  0.1173436  0.7997589           1            1
         robustWeights userWeights totalWeights rawResiduals weightedResiduals
      1              1           1            1    5.4333168         5.4333168
      2              1           1            1   -2.7123934        -2.7123934
      3              1           1            1   -5.4322236        -5.4322236
      4              1           1            1   -6.9892977        -6.9892977
      5              1           1            1   -4.2122959        -4.2122959
      6              1           1            1   -3.9007716        -3.9007716
      7              1           1            1   -2.4774985        -2.4774985
      8              1           1            1   -1.0669375        -1.0669375
      9              1           1            1   -0.2035051        -0.2035051
      10             1           1            1    0.5359639         0.5359639
      11             1           1            1    0.6824152         0.6824152
      
      attr(,"class")
      [1] "modelCost"
      

# print() produces expected output

    Code
      piResult$print()
    Message
      <PIResult>
      Optimization Summary:
        * Algorithm: BOBYQA
        * CI Method: hessian
        * Convergence: TRUE
        * Objective value: 156.3
        * Iterations: 10
        * Function evaluations: 10
        * Elapsed (optimization): 0.00e+00 s
        * Elapsed (CI): 0.00e+00 s
      Parameter Estimates:
        * Lipophilicity: Estimate = 1.313, SD = 0.2073, CV = 0.1579, CI = [0.9071,
        1.720]

# toDataFrame() produces expected output

    Code
      piResult$toDataFrame()
    Output
        group          name                                   path      unit estimate
      1     1 Lipophilicity Vergin 1995 IV|Aciclovir|Lipophilicity Log Units 1.313491
               sd        cv   lowerCI  upperCI initialValue
      1 0.2073428 0.1578562 0.9071067 1.719875       -0.097

