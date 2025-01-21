# ParameterIdentification$run() outputs expected evaluation feedback using BOBYQA algorithm

    WAoAAAACAAQEAgACAwAAAAAQAAAAAQAEAAkAAAMvZm5ldmFsIDE6IHBhcmFtZXRlcnMgLTAu
    MDk3LCBvYmplY3RpdmUgZnVuY3Rpb24gNzc4LjEKZm5ldmFsIDI6IHBhcmFtZXRlcnMgLTAu
    MDk3LCBvYmplY3RpdmUgZnVuY3Rpb24gNzc4LjEKZm5ldmFsIDM6IHBhcmFtZXRlcnMgLTAu
    MDk3LCBvYmplY3RpdmUgZnVuY3Rpb24gNzc4LjEKZm5ldmFsIDQ6IHBhcmFtZXRlcnMgNC45
    LCBvYmplY3RpdmUgZnVuY3Rpb24gODY5LjQKZm5ldmFsIDU6IHBhcmFtZXRlcnMgLTUuMSwg
    b2JqZWN0aXZlIGZ1bmN0aW9uIDU2NTIKZm5ldmFsIDY6IHBhcmFtZXRlcnMgLTAuMDk3LCBv
    YmplY3RpdmUgZnVuY3Rpb24gNzc4LjEKZm5ldmFsIDc6IHBhcmFtZXRlcnMgLTAuMDk3LCBv
    YmplY3RpdmUgZnVuY3Rpb24gNzc4LjEKZm5ldmFsIDg6IHBhcmFtZXRlcnMgLTAuMDg3Mywg
    b2JqZWN0aXZlIGZ1bmN0aW9uIDc2Ni4zCmZuZXZhbCA5OiBwYXJhbWV0ZXJzIC0wLjEwNywg
    b2JqZWN0aXZlIGZ1bmN0aW9uIDc5MC4xCmZuZXZhbCAxMDogcGFyYW1ldGVycyAtMC4wOTIy
    LCBvYmplY3RpdmUgZnVuY3Rpb24gNzcyLjIKZm5ldmFsIDExOiBwYXJhbWV0ZXJzIC0wLjEw
    Miwgb2JqZWN0aXZlIGZ1bmN0aW9uIDc4NC4xCmZuZXZhbCAxMjogcGFyYW1ldGVycyAtMC4w
    OTQ2LCBvYmplY3RpdmUgZnVuY3Rpb24gNzc1LjIKZm5ldmFsIDEzOiBwYXJhbWV0ZXJzIC0w
    LjA5OTQsIG9iamVjdGl2ZSBmdW5jdGlvbiA3ODEuMQpmbmV2YWwgMTQ6IHBhcmFtZXRlcnMg
    LTAuMDk1OCwgb2JqZWN0aXZlIGZ1bmN0aW9uIDc3Ni42CmZuZXZhbCAxNTogcGFyYW1ldGVy
    cyAtMC4wOTgyLCBvYmplY3RpdmUgZnVuY3Rpb24gNzc5LjY=

# ParameterIdentification$gridSearch() works with multiple parameters and default settings

    Code
      gridSearchResults[1:10, ]
    Output
      # A tibble: 10 x 3
         `Aciclovir|Lipophilicity` Neighborhoods|Kidney_pls_Kidney_ur|Aciclovi~1   ofv
                             <dbl>                                         <dbl> <dbl>
       1                   -0.97                                          0.0941 4324.
       2                   -0.810                                         0.0941 3735.
       3                   -0.650                                         0.0941 3169.
       4                   -0.490                                         0.0941 2640.
       5                   -0.330                                         0.0941 2159.
       6                   -0.170                                         0.0941 1735.
       7                   -0.0097                                        0.0941 1375.
       8                   -0.97                                          1.65   1713.
       9                   -0.810                                         1.65   1519.
      10                   -0.650                                         1.65   1311.
      # i abbreviated name:
      #   1: `Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS|TSspec`

# ParameterIdentification$gridSearch() sets new start values with correct message

    Code
      startValueMessage
    Output
      [1] "Grid search completed. \n Starting point for the next optimization updated to parameter values:  \n -0.0097 9.412"

