# Constructs Model Cost Summary for Error Handling

Creates a model cost summary compatible with the structure returned by
`.calculateCostMetrics`, filled with either infinite values (for
simulation failures) or zeros (for objective function failures).

## Usage

``` r
.createErrorCostStructure(infinite = FALSE)
```

## Arguments

- infinite:

  Logical flag indicating if the structure should contain infinite
  values (TRUE) or zeros (FALSE).

## Value

A model cost summary structured identically to the output of
`.calculateCostMetrics`, with fields for model cost, minimum log
probability, statistical measures, and detailed residuals, tailored for
failure scenarios or initial setup.
