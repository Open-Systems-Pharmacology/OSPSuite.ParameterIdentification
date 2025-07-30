# PIParameters can print PIOutputMapping

    Code
      print(piParam)
    Message
      <PIParameters>
        * Number of parameters: 1
        * Value: 1.764e-05
        * Start value: 1.764e-05
        * Min value: 1.764e-06
        * Max value: 0.0001764
        * Unit: dm/min

# PIParameters can export to data.frame

    Code
      piParam$toDataFrame()
    Output
        group         name                                  path   unit    currValue
      1    NA Permeability Vergin 1995 IV|Aciclovir|Permeability dm/min 1.764167e-05
          startValue     minValue     maxValue
      1 1.764167e-05 1.764167e-06 0.0001764167

# PIParameters with multiple parameters can export to data.frame

    Code
      piParam$toDataFrame()
    Output
        group         name                                  path   unit    currValue
      1    NA Permeability Vergin 1995 IV|Aciclovir|Permeability dm/min 1.764167e-05
          startValue     minValue     maxValue
      1 1.764167e-05 1.764167e-06 0.0001764167

