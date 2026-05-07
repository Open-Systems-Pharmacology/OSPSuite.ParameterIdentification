# toList() produces expected output

    Code
      piResult$toList()
    Output
      $finalParameters
      [1] 1.31349
      
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
      [1,] 727.0411
      
      $ciDetails$covMat
                 [,1]
      [1,] 0.04298725
      
      $ciDetails$eigen
      [1] 0.04298725
      
      $ciDetails$corMat
           [,1]
      [1,]    1
      
      
      $sd
      [1] 0.2073337
      
      $cv
      [1] 0.1578494
      
      $lowerCI
      [1] 0.9071235
      
      $upperCI
      [1] 1.719857
      
      $ciType
      [1] NA
      
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
      1      1   16.36364 10.5248388 15.9581718           1            1
      2      1   29.09091 13.0828911 10.3705034           1            1
      3      1   45.45455 13.6647399  8.2325211           1            1
      4      1   69.09090 13.6647399  6.6754460           1            1
      5      1   96.36366  9.6476182  5.4353247           1            1
      6      1  118.18182  8.5906572  4.6898885           1            1
      7      1  180.00000  5.8069446  3.3294468           1            1
      8      1  301.81818  3.2507177  2.1837821           1            1
      9      1  420.00000  1.9284467  1.7249435           1            1
      10     1  658.18201  0.7297269  1.2656922           1            1
      11     1 1141.81799  0.1173436  0.7997594           1            1
         robustWeights userWeights totalWeights rawResiduals weightedResiduals
      1              1           1            1    5.4333331         5.4333331
      2              1           1            1   -2.7123876        -2.7123876
      3              1           1            1   -5.4322189        -5.4322189
      4              1           1            1   -6.9892939        -6.9892939
      5              1           1            1   -4.2122935        -4.2122935
      6              1           1            1   -3.9007687        -3.9007687
      7              1           1            1   -2.4774978        -2.4774978
      8              1           1            1   -1.0669356        -1.0669356
      9              1           1            1   -0.2035032        -0.2035032
      10             1           1            1    0.5359653         0.5359653
      11             1           1            1    0.6824158         0.6824158
      
      attr(,"class")
      [1] "modelCost"
      

# print() produces expected output

    Code
      piResult$print()
    Output
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
        * Lipophilicity: Estimate = 1.313, SD = 0.2073, CV = 0.1578, CI = [0.9071,
        1.720]

# toDataFrame() produces expected output

    Code
      piResult$toDataFrame()
    Output
        group          name                                   path      unit estimate
      1     1 Lipophilicity Vergin 1995 IV|Aciclovir|Lipophilicity Log Units  1.31349
               sd        cv   lowerCI  upperCI ciType initialValue
      1 0.2073337 0.1578494 0.9071235 1.719857   <NA>       -0.097

