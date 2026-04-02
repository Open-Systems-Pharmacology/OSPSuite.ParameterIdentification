# gridSearch() works with multiple parameters and default settings

    Code
      gridSearchResults[1:10, ]
    Output
      # A tibble: 10 x 3
         `Aciclovir|Lipophilicity` Neighborhoods|Kidney_pls_Kidney_ur|Aciclovi~1   ofv
                             <dbl>                                         <dbl> <dbl>
       1                   -0.97                                          0.0941 219. 
       2                   -0.810                                         0.0941 189. 
       3                   -0.650                                         0.0941 161. 
       4                   -0.490                                         0.0941 134. 
       5                   -0.330                                         0.0941 109. 
       6                   -0.170                                         0.0941  88.0
       7                   -0.0097                                        0.0941  69.7
       8                   -0.97                                          1.65    86.9
       9                   -0.810                                         1.65    77.1
      10                   -0.650                                         1.65    66.5
      # i abbreviated name:
      #   1: `Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS|TSspec`

# gridSearch() sets new start values with correct message

    Code
      startValueMessage
    Output
      [1] "Grid search completed. \n Starting point for the next optimization updated to parameter values:  \n -0.0097 9.412"

# gridSearch() returns `Inf` upon simulation failure

    Code
      gridSearchResults$ofv
    Output
      [1]      Inf      Inf 20.95601 21.17398

# calculateOFVProfiles() works with multiple parameters

    Code
      ofvProfiles[[1]][1:10, ]
    Output
      # A tibble: 10 x 2
         `Aciclovir|Lipophilicity`   ofv
                             <dbl> <dbl>
       1                   -0.107   40.1
       2                   -0.106   40.0
       3                   -0.105   39.9
       4                   -0.104   39.9
       5                   -0.103   39.8
       6                   -0.102   39.8
       7                   -0.101   39.7
       8                   -0.0996  39.6
       9                   -0.0985  39.6
      10                   -0.0975  39.5

---

    Code
      ofvProfiles[[2]][1:10, ]
    Output
      # A tibble: 10 x 2
         Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS|TSsp~1   ofv
                                                                           <dbl> <dbl>
       1                                                                   0.847  41.2
       2                                                                   0.857  41.0
       3                                                                   0.867  40.8
       4                                                                   0.877  40.6
       5                                                                   0.887  40.4
       6                                                                   0.897  40.3
       7                                                                   0.907  40.1
       8                                                                   0.916  39.9
       9                                                                   0.926  39.7
      10                                                                   0.936  39.6
      # i abbreviated name:
      #   1: `Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS|TSspec`

# calculateOFVProfiles() returns `Inf` on simulation failure

    Code
      ofvProfiles[[2]]$ofv
    Output
      [1]      Inf 180.8782 159.9647

