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

