# toList() produces expected output

    Code
      piResult$toList()
    Output
      $finalParameters
      [1] 1.313491
      
      $objectiveValue
      [1] 7.925817
      
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
      [1,] 36.87197
      
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
      
      $ciType
      [1] NA
      
      $paramNames
      [1] "Lipophilicity"
      
      $costDetails
      $modelCost
      [1] 7.925817
      
      $minLogProbability
      [1] 14.07123
      
      $costVariables
        nObservations M3Contribution   rawSSR weightedSSR
      1            11              0 7.925817    7.925817
      
      $residualDetails
         index          x  yObserved ySimulated scaleFactor errorWeights
      1      1  0.2727273 2.37029894  3.5939362           1            1
      2      1  0.4848485 2.94639790  2.3355398           1            1
      3      1  0.7575758 3.07743608  1.8540450           1            1
      4      1  1.1515149 3.07743608  1.5033763           1            1
      5      1  1.6060610 2.17274010  1.2240889           1            1
      6      1  1.9696971 1.93470191  1.0562091           1            1
      7      1  3.0000000 1.30778199  0.7498246           1            1
      8      1  5.0303029 0.73209412  0.4918091           1            1
      9      1  7.0000000 0.43430549  0.3884741           1            1
      10     1 10.9697001 0.16434180  0.2850462           1            1
      11     1 19.0302999 0.02642696  0.1801137           1            1
         robustWeights userWeights totalWeights rawResiduals weightedResiduals
      1              1           1            1   1.22363729        1.22363729
      2              1           1            1  -0.61085811       -0.61085811
      3              1           1            1  -1.22339108       -1.22339108
      4              1           1            1  -1.57405973       -1.57405973
      5              1           1            1  -0.94865116       -0.94865116
      6              1           1            1  -0.87849277       -0.87849277
      7              1           1            1  -0.55795743       -0.55795743
      8              1           1            1  -0.24028498       -0.24028498
      9              1           1            1  -0.04583139       -0.04583139
      10             1           1            1   0.12070443        0.12070443
      11             1           1            1   0.15368673        0.15368673
      
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
        * Objective value: 7.926
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
               sd        cv   lowerCI  upperCI ciType initialValue
      1 0.2073428 0.1578562 0.9071067 1.719875   <NA>       -0.097

