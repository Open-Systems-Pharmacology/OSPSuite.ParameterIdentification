# Summarize Cost Lists

This function takes two lists, each being the output of the
`.calculateCostMetrics` function, and summarizes them. It aggregates
model costs and min log probabilities, and combines cost and residual
details by row-binding.

## Usage

``` r
.summarizeCostLists(list1, list2)
```

## Arguments

- list1:

  The first list, containing the output of the `.calculateCostMetrics`
  function, which includes `modelCost`, `minLogProbability`,
  `costVariables`, and `residualDetails`.

- list2:

  The second list, containing the output of the `.calculateCostMetrics`
  function, which includes `modelCost`, `minLogProbability`,
  `costVariables`, and `residualDetails`.

## Value

Returns a list that includes the sum of `modelCosts`, the sum of
`minLogProbabilities`, a row-bound combination of `costVariables`, and a
row-bound combination of `residualDetails`.
