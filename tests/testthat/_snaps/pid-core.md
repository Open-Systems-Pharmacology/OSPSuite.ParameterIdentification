# ParameterIdentification returns infinite value if simulation fails

    Code
      suppressMessages(piResult <- testTask$run())
    Condition
      Warning in `testTask$run()`:
      The simulations failed in 13 of 13 evaluations. Check the parameter bounds and the solver settings.

