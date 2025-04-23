# ParameterIdentification instance prints without errors

    Code
      print(piTask)
    Message
      <ParameterIdentification>
        * Number of parameters: 1
      Simulations:
      <SimPath>

# ParameterIdentification$run() runs successfully using default BOBYQA algorithm

    list(par = 1.31885961110325, value = 156.257356828486, convergence = TRUE, 
        iterations = 20L, fnEvaluations = 20L, algorithm = "BOBYQA", 
        elapsed = 0)

# ParameterIdentification$run() outputs expected evaluation feedback using BOBYQA algorithm

    "fneval: 1 | parameters: -0.097 | objective: 778.129fneval: 2 | parameters: -0.097 | objective: 778.129fneval: 3 | parameters: -0.097 | objective: 778.129fneval: 4 | parameters: 4.903 | objective: 869.379fneval: 5 | parameters: -5.097 | objective: 5652.395"

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

