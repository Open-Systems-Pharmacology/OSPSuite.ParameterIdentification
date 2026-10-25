# .ofvGrid() spaces a log-scaled parameter geometrically

    Code
      .ofvGrid(quadratic, lower = c(0, 1), upper = c(100, 1), logScaleFlag = c(TRUE,
        FALSE), totalEvaluations = 9, parameterNames = c("a", "b"))
    Condition
      Error in `.ofvGrid()`:
      ! Logarithmic scaling is not available for non-positive parameter values.

