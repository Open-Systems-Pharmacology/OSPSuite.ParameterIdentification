# .validateModelCost() names what is wrong

    Code
      .validateModelCost(list(modelCost = 2))
    Condition
      Error in `.validateModelCost()`:
      ! The cost details are not a valid <modelCost> object as built by the objective function: it does not have the class modelCost.

---

    Code
      .validateModelCost(unclass(cost))
    Condition
      Error in `.validateModelCost()`:
      ! The cost details are not a valid <modelCost> object as built by the objective function: it does not have the class modelCost.

---

    Code
      .validateModelCost(noResiduals)
    Condition
      Error in `.validateModelCost()`:
      ! The cost details are not a valid <modelCost> object as built by the objective function: its fields are not modelCost, minLogProbability, costVariables, residualDetails.

---

    Code
      .validateModelCost(fewerColumns)
    Condition
      Error in `.validateModelCost()`:
      ! The cost details are not a valid <modelCost> object as built by the objective function: costVariables does not have the columns nObservations, M3Contribution, rawSSR, weightedSSR.

---

    Code
      .validateModelCost(vectorCost)
    Condition
      Error in `.validateModelCost()`:
      ! The cost details are not a valid <modelCost> object as built by the objective function: modelCost and minLogProbability are not single numbers.

# PIObjective validates its inputs

    Code
      objective$observedData <- NULL
    Condition
      Error:
      ! Property $observedData is readonly

