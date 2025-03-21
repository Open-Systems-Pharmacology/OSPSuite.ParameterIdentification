# It can print default optimizer

    Code
      print(optimizer)
    Output
      <Optimizer>
        Public:
          algorithm: active binding
          initialize: function (algorithm, controlOptim = NULL, modelCostField = NULL) 
          modelCostField: active binding
          run: function (par, fn, lower, upper, fixedParams = NULL) 
        Private:
          .algorithm: BOBYQA
          .controlOptim: NULL
          .formatOptimizationOutput: function (optimResult) 
          .modelCostField: modelCost
          .preprocessFn: function (fn, fixedParams) 
          .runBOBYQA: function (par, fn, lower, upper, controlOptim, fixedParams = NULL) 
          .runDEoptim: function (par, fn, lower, upper, controlOptim, fixedParams = NULL) 
          .runHJKB: function (par, fn, lower, upper, controlOptim, fixedParams = NULL) 
          .updateFixedParams: function (par, fixedParams) 

# Optimizer estimates confidence intervals using Hessian

    list(se = c(0.0272454770065792, 0.0736788397613054, 0.33611528505631
    ), cv = c(2.72834446908413, 3.53969104421257, 12.4501355482489
    ), lowerCI = c(0.945208244762991, -2.2259126941322, 2.04091791909716
    ), upperCI = c(1.05200855211201, -1.93709694942249, 3.35846562582472
    ), error = NULL, method = "hessian", elapsed = NA, details = list(
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
    ), error = NULL, method = "PL", elapsed = NA, details = list(
        paramHistory = NA))

# Optimizer estimates confidence intervals using bootstrap method

    list(se = c(0.0170906450823763, 0.0902964500794223, 0.283998283570631
    ), cv = c(1.71144615938717, 4.33803703622077, 10.5196558535921
    ), lowerCI = c(0.964100646972656, -2.14092636108398, 2.73056030273438
    ), upperCI = c(1.01421432495117, -1.88913345336914, 3.6590461730957
    ), error = NULL, method = "bootstrap", elapsed = NA, details = list(
        bootstrapResults = NA, corMat = structure(c(1, -0.0644012867104282, 
        -0.628340292433886, -0.0644012867104282, 1, -0.134966842176002, 
        -0.628340292433886, -0.134966842176002, 1), dim = c(3L, 3L
        ))))

