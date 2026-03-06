# Plot Model Cost Residuals

Plots raw residuals and, if different, weighted and robust weighted
residuals from a `modelCost` object.

## Usage

``` r
# S3 method for class 'modelCost'
plot(x, legpos = "topright", ...)
```

## Arguments

- x:

  A `modelCost` object containing residuals to plot.

- legpos:

  Position of the legend; default is "topright". Use NA to omit the
  legend.

- ...:

  Additional arguments passed to the plot function.

## Value

Generates a plot.

## Examples

``` r
# Assuming modelCostObj is a valid `modelCost` object
if (FALSE) { # \dontrun{
plot.modelCost(modelCostObj)
} # }
```
