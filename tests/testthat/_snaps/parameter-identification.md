# ParameterIdentification instance prints without errors

    Code
      print(piTask)
    Message
      <ParameterIdentification>
        * Number of parameters: 1
      Simulations:
        *
      <SimPath>

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

# ParameterIdentification$gridSearch() returns `Inf` upon simulation failure

    Code
      gridSearchResults$ofv
    Output
      [1]      Inf      Inf 413.1742 417.4718

# ParameterIdentification$calculateOFVProfiles() works as expected

    Code
      ofvProfiles[[1]][1:10, ]
    Output
      # A tibble: 10 x 2
         `Aciclovir|Lipophilicity`   ofv
                             <dbl> <dbl>
       1                   -0.107   790.
       2                   -0.106   789.
       3                   -0.105   788.
       4                   -0.104   786.
       5                   -0.103   785.
       6                   -0.102   784.
       7                   -0.101   783.
       8                   -0.0996  781.
       9                   -0.0985  780.
      10                   -0.0975  779.

---

    Code
      ofvProfiles[[2]][1:10, ]
    Output
      # A tibble: 10 x 2
         Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS|TSsp~1   ofv
                                                                           <dbl> <dbl>
       1                                                                   0.847  812.
       2                                                                   0.857  808.
       3                                                                   0.867  805.
       4                                                                   0.877  801.
       5                                                                   0.887  797.
       6                                                                   0.897  794.
       7                                                                   0.907  790.
       8                                                                   0.916  787.
       9                                                                   0.926  783.
      10                                                                   0.936  780.
      # i abbreviated name:
      #   1: `Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS|TSspec`

# ParameterIdentification$calculateOFVProfiles() returns `Inf` upon simulation failure

    Code
      ofvProfiles[[2]]$ofv
    Output
      [1]      Inf 3566.243 3153.906

