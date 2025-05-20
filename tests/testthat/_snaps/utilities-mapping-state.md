# .extractOutputMappingState works for single mapping with one dataset

    c("List of 2", " $ dataSetWeights:List of 1", "  ..$ :List of 1", 
    "  .. ..$ AciclovirLaskinData.Laskin 1982.Group A: num [1:11] 1 1 1 1 1 1 1 1 1 1 ...", 
    " $ dataSetValues :List of 1", "  ..$ :List of 1", "  .. ..$ AciclovirLaskinData.Laskin 1982.Group A:List of 4", 
    "  .. .. ..$ xValues     : num [1:11] 0.273 0.485 0.758 1.152 1.606 ...", 
    "  .. .. ..$ yValues     : num [1:11] 2.37 2.95 3.08 3.08 2.17 ...", 
    "  .. .. ..$ yErrorValues: num [1:11] NaN 1.23 1.28 1.82 NaN ...", 
    "  .. .. ..$ yErrorType  : chr \"ArithmeticStdDev\"")

# .extractOutputMappingState works for single mapping with multiple datasets

    c("List of 2", " $ dataSetWeights:List of 1", "  ..$ :List of 2", 
    "  .. ..$ dataSet1: num [1:11] 1 1 1 1 1 1 1 1 1 1 ...", "  .. ..$ dataSet2: num [1:10] 1 1 1 1 1 1 1 1 1 1", 
    " $ dataSetValues :List of 1", "  ..$ :List of 2", "  .. ..$ dataSet1:List of 4", 
    "  .. .. ..$ xValues     : num [1:11] 0.273 0.485 0.758 1.152 1.606 ...", 
    "  .. .. ..$ yValues     : num [1:11] 2.37 2.95 3.08 3.08 2.17 ...", 
    "  .. .. ..$ yErrorValues: num [1:11] NaN 1.23 1.28 1.82 NaN ...", 
    "  .. .. ..$ yErrorType  : chr \"ArithmeticStdDev\"", "  .. ..$ dataSet2:List of 4", 
    "  .. .. ..$ xValues     : num [1:10] 0.273 0.485 0.758 1.152 1.606 ...", 
    "  .. .. ..$ yValues     : num [1:10] 3.56 4.42 4.62 4.62 3.26 ...", 
    "  .. .. ..$ yErrorValues: num [1:10] NaN 1.23 1.28 1.82 NaN ...", 
    "  .. .. ..$ yErrorType  : chr \"ArithmeticStdDev\"")

# .extractOutputMappingState works for multiple mappings with single dataset

    c("List of 2", " $ dataSetWeights:List of 2", "  ..$ :List of 1", 
    "  .. ..$ AciclovirLaskinData.Laskin 1982.Group A: num [1:11] 1 1 1 1 1 1 1 1 1 1 ...", 
    "  ..$ :List of 1", "  .. ..$ AciclovirLaskinData.Laskin 1982.Group A: num [1:11] 1 1 1 1 1 1 1 1 1 1 ...", 
    " $ dataSetValues :List of 2", "  ..$ :List of 1", "  .. ..$ AciclovirLaskinData.Laskin 1982.Group A:List of 4", 
    "  .. .. ..$ xValues     : num [1:11] 0.273 0.485 0.758 1.152 1.606 ...", 
    "  .. .. ..$ yValues     : num [1:11] 2.37 2.95 3.08 3.08 2.17 ...", 
    "  .. .. ..$ yErrorValues: num [1:11] NaN 1.23 1.28 1.82 NaN ...", 
    "  .. .. ..$ yErrorType  : chr \"ArithmeticStdDev\"", "  ..$ :List of 1", 
    "  .. ..$ AciclovirLaskinData.Laskin 1982.Group A:List of 4", 
    "  .. .. ..$ xValues     : num [1:11] 0.273 0.485 0.758 1.152 1.606 ...", 
    "  .. .. ..$ yValues     : num [1:11] 2.37 2.95 3.08 3.08 2.17 ...", 
    "  .. .. ..$ yErrorValues: num [1:11] NaN 1.23 1.28 1.82 NaN ...", 
    "  .. .. ..$ yErrorType  : chr \"ArithmeticStdDev\"")

