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

