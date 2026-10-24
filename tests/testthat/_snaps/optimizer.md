# It can print default optimizer

    Code
      print(optimizer)
    Output
      <Optimizer>
        Public:
          algorithm: active binding
          ciMethod: active binding
          estimateCI: function (par, fn, lower, upper, optimizer = NULL, resetFn = NULL) 
          initialize: function (configuration) 
          modelCostField: active binding
          run: function (par, fn, lower, upper, fixedParams = NULL) 
        Private:
          .computeProfileCI: function (par, fn, optimizer, p, direction, controlCI, resetFn = NULL) 
          .configuration: PIConfiguration, R6
          .estimateBootstrapCI: function (par, fn, lower, upper, controlCI, optimizer = NULL, 
          .estimateCIProfileLikelihood: function (par, fn, controlCI, optimizer, resetFn = NULL) 
          .estimateHessianCI: function (par, fn, controlCI) 
          .formatOptimizationOutput: function (optimResult, controlOptim = list()) 
          .initializeCIResult: function () 
          .preprocessFn: function (fn, fixedParams) 
          .runBOBYQA: function (par, fn, lower, upper, controlOptim, fixedParams = NULL) 
          .runDEoptim: function (par, fn, lower, upper, controlOptim, fixedParams = NULL) 
          .runHJKB: function (par, fn, lower, upper, controlOptim, fixedParams = NULL) 
          .updateFixedParams: function (par, fixedParams) 
          .verbose: TRUE

# Optimizer returns correct parameters for BOBYQA

    list(par = c(5.51056363987369, 0.384109285445577, 0.683844281729887
    ), value = 0.421489866588719, convergence = TRUE, convergenceMessage = "NLOPT_SUCCESS: Generic success return value.", 
        iterations = 100L, fnEvaluations = 100L, algorithm = "BOBYQA", 
        elapsed = 0L)

# HJKB stopped at maxfeval reports why it stopped

    Code
      cat(optResult$convergenceMessage)
    Output
      Stopped because `maxfeval` was reached before step size fell below `tol`.

# HJKB reports the stop reason at its limits

    Code
      cat(atMaxfeval$convergenceMessage)
    Output
      Stopped because `maxfeval` was reached before step size fell below `tol`.
    Code
      cat(pastTarget$convergenceMessage)
    Output
      Stopped because the absolute objective value exceeded `target`.
    Code
      cat(overshotLastStep$convergenceMessage)
    Output
      Stopped because `maxfeval` was reached before step size fell below `tol`.

# A non-finite objective value is not converged and says why

    Code
      cat(optResult$convergenceMessage)
    Output
      Used 2 of `itermax` = 2 generations. Relative change of the best value over the last 2 generations is not below 1e-5. The objective value is not finite.

# DEoptim reports generations used against itermax

    Code
      cat(optResult$convergenceMessage)
    Output
      Used 3 of `itermax` = 3 generations. Relative change of the best value over the last 3 generations is below 1e-5.

# Optimizer output has correct structure and values for HJKB

    list(par = c(5.51062088012695, 0.384121704101562, 0.683872680664062
    ), value = 0.421489868224619, convergence = TRUE, convergenceMessage = "Step size fell below `tol`.", 
        iterations = 19, fnEvaluations = NA_real_, algorithm = "HJKB", 
        elapsed = 0)

# Optimizer output has correct structure and values for DEoptim

    list(par = c(5.51056395063057, 0.384109273755908, 0.683844221712587
    ), value = 0.421489866588652, convergence = TRUE, convergenceMessage = "Used 200 of `itermax` = 200 generations. Relative change of the best value over the last 5 generations is below 1e-5.", 
        iterations = 200L, fnEvaluations = NA_real_, algorithm = "DEoptim", 
        elapsed = 0)

# Optimizer works with fixed parameter

    c(5, 0.339307152012859, 0.639173644085057)

# Optimizer works with two fixed parameters

    c(5, 0.349021131926258, 0.68)

# Optimizer estimates confidence intervals using Hessian

    list(sd = c(0.0277230973751015, 0.0749704491756164, 0.342007474291089
    ), cv = c(0.0277617306428418, 0.0360174275799184, 0.126683896946994
    ), lowerCI = c(0.944272126042404, -2.22844420206634, 2.02936944040689
    ), upperCI = c(1.0529446708326, -1.93456544148835, 3.37001410451498
    ), error = NULL, method = "hessian", elapsed = NA_real_, details = list(
        hessian = structure(c(6087.622102347, 4.22282990725027e-11, 
        368.421052631563, 4.22282990725027e-11, 368.421052631532, 
        -1.46124342581759e-12, 368.421052631563, -1.46124342581759e-12, 
        39.9999999999992), dim = c(3L, 3L)), covMat = structure(c(0.00076857012806936, 
        -1.16169954787298e-16, -0.00707893539011238, -1.16169954787298e-16, 
        0.00562056824959368, 1.27531188577441e-15, -0.00707893539011238, 
        1.27531188577441e-15, 0.11696911247097), dim = c(3L, 3L)), 
        eigen = c(0.117398772415603, 0.00562056824959368, 0.000338910183436461
        ), corMat = structure(c(1, -5.58935838449509e-14, -0.746604495874943, 
        -5.58935838449509e-14, 1, 4.97382724703412e-14, -0.746604495874943, 
        4.97382724703412e-14, 1), dim = c(3L, 3L))))

# Optimizer estimates confidence intervals using profile likelihood method

    list(se = c(0.0356652410156552, 0.0955810593651919, 0.468322484431836
    ), rse = c(0.0357149419847258, 0.0459192111232189, 0.173472575354382
    ), lowerCI = c(0.928705810546875, -2.2688402557373, 1.78179656982422
    ), upperCI = c(1.06851098632813, -1.89416938781738, 3.61758697509766
    ), error = NULL, method = "PL", elapsed = NA_real_, details = list(
        paramHistory = structure(list(Parameter = c(1, 1, 1, 1, 1
        ), Iteration = c(1, 2, 3, 4, 5), Value = c(0.988622314453125, 
        0.97863623046875, 0.968650146484375, 0.9586640625, 0.948677978515625
        ), Cost = c(8.86783708548332, 9.06938641054093, 9.4052745765092, 
        9.87550158336852, 10.4800674310877)), row.names = c(NA, 5L
        ), class = "data.frame")))

# Optimizer estimates confidence intervals using bootstrap method

    list(sd = c(0.0232250971973313, 0.0697139396777191, 0.344648219590348
    ), cv = c(0.0232574623182331, 0.0334920865656185, 0.12766206242729
    ), lowerCI = c(0.921116638183594, -2.19458770751953, 2.01670455932617
    ), upperCI = c(1.05830841064453, -1.83130645751953, 3.86165237426758
    ), error = NULL, method = "bootstrap", elapsed = NA_real_, details = list(
        bootstrapResults = structure(c(1.01316528320313, 0.987412261962891, 
        1.00729446411133, 1.03567962646484, 0.970944213867188, -2.11624908447266, 
        -2.04559707641602, -1.97860336303711, -1.99147033691406, 
        -2.07143020629883, 3.45014953613281, 2.93540191650391, 3.24865341186523, 
        2.3338737487793, 3.30581665039062), dim = c(5L, 3L)), corMat = structure(c(1, 
        -0.211707142395003, -0.781584854855845, -0.211707142395003, 
        1, -0.0140386952666812, -0.781584854855845, -0.0140386952666812, 
        1), dim = c(3L, 3L))), ciType = c("two-sided", "two-sided", 
    "two-sided"))

# estimateCI() with the bootstrap method stops without replicate

    Code
      optimizer$estimateCI(par = parTest, fn = fnObjective, lower = lowerTest, upper = upperTest)
    Condition
      Error in `optimizer$estimateCI()`:
      ! The bootstrap method needs `replicate`, a `function(seed)` that returns the objective function of one bootstrap replicate.

# Hessian CI reports a failed Hessian calculation

    Code
      ciResult$error
    Output
      [1] "Error during CI estimation step 'Hessian calculation': function evaluation failed"

# Hessian CI reports a singular Hessian

    Code
      ciResult$error
    Output
      [1] "Error during CI estimation step 'Covariance matrix calculation': Lapack routine dgesv: system is exactly singular: U[1,1] = 0"

# Hessian CI stops when the objective function value has no cost variables

    Code
      optimizer$estimateCI(par = parTest, fn = fnScalar, lower = lowerTest, upper = upperTest)
    Message
      Starting confidence interval estimation using 'hessian' for parameter value(s):
        0.8000, -1.500, 2.500
    Condition
      Error:
      ! The Hessian method needs weightedSSR and nObservations in costVariables of the objective function value, missing: "weightedSSR" and "nObservations". Return a value with these fields, for example a <modelCost> object as <ParameterIdentification> does, or use `ciMethod = "PL"`.

