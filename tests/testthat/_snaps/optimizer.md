# It can print default optimizer

    Code
      print(optimizer)
    Output
      <Optimizer>
        Public:
          algorithm: active binding
          ciMethod: active binding
          estimateCI: function (par, fn, lower, upper, optimizer = NULL) 
          initialize: function (configuration) 
          modelCostField: active binding
          run: function (par, fn, lower, upper, fixedParams = NULL) 
        Private:
          .computeProfileCI: function (par, fn, optimizer, p, direction, controlCI) 
          .configuration: PIConfiguration, R6
          .estimateBootstrapCI: function (par, fn, lower, upper, controlCI, optimizer = NULL) 
          .estimateCIProfileLikelihood: function (par, fn, controlCI, optimizer) 
          .estimateHessianCI: function (par, fn, controlCI) 
          .formatOptimizationOutput: function (optimResult) 
          .initializeCIResult: function () 
          .preprocessFn: function (fn, fixedParams) 
          .runBOBYQA: function (par, fn, lower, upper, controlOptim, fixedParams = NULL) 
          .runDEoptim: function (par, fn, lower, upper, controlOptim, fixedParams = NULL) 
          .runHJKB: function (par, fn, lower, upper, controlOptim, fixedParams = NULL) 
          .updateFixedParams: function (par, fixedParams) 
          .verbose: TRUE

# Optimizer returns correct parameters for BOBYQA

    list(par = c(5.51056363987369, 0.384109285445577, 0.683844281729887
    ), value = 0.421489866588719, convergence = TRUE, iterations = 123L, 
        fnEvaluations = 123L, algorithm = "BOBYQA", elapsed = 0)

# Optimizer output has correct structure and values for HJKB

    list(par = c(5.51054840087891, 0.3841064453125, 0.683838348388672
    ), value = 0.421489866678708, convergence = TRUE, iterations = 19, 
        fnEvaluations = NA_real_, algorithm = "HJKB", elapsed = 0)

# Optimizer output has correct structure and values for DEoptim

    list(par = c(5.51056393151954, 0.384109269437653, 0.683844224608981
    ), value = 0.421489866588654, convergence = TRUE, iterations = 200L, 
        fnEvaluations = NA_real_, algorithm = "DEoptim", elapsed = 0)

# Optimizer works with fixed parameter

    c(5, 0.339307152012859, 0.639173644085057)

# Optimizer works with two fixed parameters

    c(5, 0.349021131926258, 0.68)

# Optimizer estimates confidence intervals using Hessian

    list(se = c(0.0272454770065792, 0.0736788397613054, 0.33611528505631
    ), cv = c(2.72834446908413, 3.53969104421257, 12.4501355482489
    ), lowerCI = c(0.945208244762991, -2.2259126941322, 2.04091791909716
    ), upperCI = c(1.05200855211201, -1.93709694942249, 3.35846562582472
    ), error = NULL, method = NULL, elapsed = NA_real_, details = list(
        hessian = structure(c(6087.622102347, 4.22282990725027e-11, 
        368.421052631563, 4.22282990725027e-11, 368.421052631532, 
        -1.46124342581759e-12, 368.421052631563, -1.46124342581759e-12, 
        39.9999999999992), dim = c(3L, 3L)), corMat = structure(c(1, 
        -5.58935838449508e-14, -0.746604495874943, -5.58935838449508e-14, 
        1, 4.97382724703412e-14, -0.746604495874943, 4.97382724703412e-14, 
        1), dim = c(3L, 3L))))

# Optimizer estimates confidence intervals using profile likelihood method

    list(se = c(0.0356652410156552, 0.0955810593651919, 0.468322484431836
    ), cv = c(3.57149419847258, 4.59192111232189, 17.3472575354382
    ), lowerCI = c(0.928705810546875, -2.2688402557373, 1.78179656982422
    ), upperCI = c(1.06851098632813, -1.89416938781738, 3.61758697509766
    ), error = NULL, method = NULL, elapsed = NA_real_, details = list(
        paramHistory = structure(list(Parameter = c(1, 1, 1, 1, 1
        ), Iteration = c(1, 2, 3, 4, 5), Value = c(0.988622314453125, 
        0.97863623046875, 0.968650146484375, 0.9586640625, 0.948677978515625
        ), Cost = c(8.86783708548332, 9.06938641054093, 9.4052745765092, 
        9.87550158336852, 10.4800674310877)), row.names = c(NA, 5L
        ), class = "data.frame")))

# Optimizer estimates confidence intervals using bootstrap method

    list(se = c(0.00170906450823763, 0.00902964500794223, 0.0283998283570631
    ), cv = c(0.171144615938717, 0.433803703622077, 1.05196558535921
    ), lowerCI = c(0.964100646972656, -2.14092636108398, 2.73056030273438
    ), upperCI = c(1.01421432495117, -1.88913345336914, 3.6590461730957
    ), error = NULL, method = NULL, elapsed = NA_real_, details = list(
        bootstrapResults = structure(c(0.9732177734375, 0.9732177734375, 
        1.01421432495117, 0.993557739257813, 0.971829223632813, -1.88913345336914, 
        -1.88913345336914, -1.97151184082031, -1.92397689819336, 
        -1.99475479125977, 3.02717208862305, 3.02717208862305, 2.86656951904297, 
        3.18732070922852, 3.6590461730957), dim = c(5L, 3L)), corMat = structure(c(1, 
        -0.0644012867104282, -0.628340292433886, -0.0644012867104282, 
        1, -0.134966842176002, -0.628340292433886, -0.134966842176002, 
        1), dim = c(3L, 3L))))

