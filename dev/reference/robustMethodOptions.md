# Robust Weighting Methods for Cost Function

Robust weighting methods to address outliers in the cost function
calculation during parameter optimization.

## Usage

``` r
robustMethodOptions
```

## Format

An object of class `list` of length 3.

## Details

The available methods are:

- **`none`** – No robust weighting applied.

- **`huber`** – Huber weighting for moderate outliers.

- **`bisquare`** – Bisquare (Tukey's biweight) weighting for severe
  outliers.
