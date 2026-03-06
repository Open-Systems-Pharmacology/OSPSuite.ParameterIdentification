# Apply Log Transformation to Data Frame

Transforms the `yValues` and `lloq` columns in the given data frame
using a log transformation. Currently, this function only supports
`obsVsPredDf` data frames, which must contain `yDimension`, `yUnit`,
`yValues`, and `lloq` columns.

## Usage

``` r
.applyLogTransformation(df, base = exp(1))
```

## Arguments

- df:

  A `tbl_df` representing the observed vs predicted data frame
  (`obsVsPredDf`).

- base:

  A positive numeric value specifying the logarithm base. Defaults to
  natural logarithm (`exp(1)`).

## Value

A transformed data frame with log-transformed `yValues` and `lloq`.

## Examples

``` r
# Assuming df is a valid obsVsPredDf data frame
if (FALSE) { # \dontrun{
transformedDf <- applyLogTransformation(df)
} # }
```
