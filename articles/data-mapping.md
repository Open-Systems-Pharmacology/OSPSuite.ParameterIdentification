# Data mapping and weights

## Introduction

## Introduction

This vignette shows how to link simulation outputs to observed data and
how to apply weights and transformations in the parameter identification
workflow.  
Mappings are created with the `PIOutputMapping` class, which connects a
simulation quantity to one or more observed datasets. Each mapping can
be customized by assigning dataset- or point-specific weights and by
applying transformations such as offsets or scaling factors. These
adjustments help align observed and simulated values and control how
much influence each dataset has on the optimization.

## Creating a mapping

Mappings are created with the `PIOutputMapping` class, which connects a
simulation quantity (for example a plasma concentration) to one or more
observed datasets.  
The quantity itself is retrieved from a loaded simulation via
`getQuantity`.

In the following we use the Aciclovir example simulation:

``` r
library(ospsuite.parameteridentification)
#> Loading required package: ospsuite

# Load example simulation
simulation <- loadSimulation(
  system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
)

# Retrieve a quantity by path from the simulation
quantity <- getQuantity(
  path = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  container = simulation
)

# Create a new mapping for this quantity
outputMapping <- PIOutputMapping$new(quantity = quantity)
```

The `path` corresponds to the quantity’s location in the simulation
structure. At this point, `outputMapping` represents the link between
the chosen simulation output and the observed data that will be added
next.

For a full description of all fields and methods, see the
[`PIOutputMapping`](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/reference/PIOutputMapping.html)
documentation.

## Adding observed data

Observed datasets can be imported from Excel and attached to the
mapping.  
Import settings (such as the naming pattern) are defined via an importer
configuration. For more background on working with observed data and
importing from Excel, see the [Observed
data](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/observed-data.html)
article in the {ospsuite} documentation.

``` r
filePath <- system.file("extdata", "Aciclovir_Profiles.xlsx",
                        package = "ospsuite.parameteridentification")

importConfig <- createImporterConfigurationForFile(filePath)
importConfig$namingPattern <- "{Source}.{Sheet}.{Dose}"

obsData <- loadDataSetsFromExcel(
  xlsFilePath = filePath,
  importerConfigurationOrPath = importConfig,
  importAllSheets = TRUE
)

# Attach one or more datasets to the mapping (labels come from the naming pattern)
outputMapping$addObservedDataSets(
  data = obsData$`Aciclovir_Profiles.Vergin 1995.Iv.250 mg`
)
```

## Scaling

Scaling specifies whether residuals are calculated on the raw (linear)
scale or on a logarithmic scale. The default is linear (`"lin"`), but
logarithmic scaling (`"log"`) can be selected to emphasize relative
differences when data span several orders of magnitude.

``` r
outputMapping$scaling <- "log"
```

## Setting weights

Weights control the relative influence of observed data on the
optimization.  
They can be supplied directly when datasets are added to a mapping, or
they can be set or updated later with `setDataWeights()`. In both cases,
weights are passed as a named list where the names must match the
dataset labels.

### Adding weights directly

Weights can be attached at the time of adding datasets. This allows
entire datasets to be up- or down-weighted in one step.

``` r
## Not run: example only
exampleMapping$addObservedDataSets(
  data = list("Study A" = dsA, "Study B" = dsB),
  weights = list(
    "Study A" = 1.0,   
    "Study B" = 0.5    
  )
)
## End(Not run)
```

The code above is illustrative and not executed here.

### Example with one dataset

In the Aciclovir example only one dataset is mapped. A vector of weights
is used, with one weight per data point:

``` r
# number of observed points
nPoints <- length(obsData$`Aciclovir_Profiles.Vergin 1995.Iv.250 mg`$yValues)

# give less weight to the first 3 points, full weight to the rest
outputMapping$setDataWeights(
  list(
    "Aciclovir_Profiles.Vergin 1995.Iv.250 mg" =
      c(rep(0.3, 3), rep(1.0, nPoints - 3))
  )
)
```

Printing the `PIOutputMapping` object provides a concise summary showing
the output path and the labels of attached datasets, the labels where
weights were applied, and the selected scaling:

``` r
print(outputMapping)
#> <PIOutputMapping>
#>   • Output path: Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral
#>   Venous Blood)
#>   • Observed data labels: Aciclovir_Profiles.Vergin 1995.Iv.250 mg
#>   • Data weight labels: Aciclovir_Profiles.Vergin 1995.Iv.250 mg
#>   • Scaling: log
```

The assigned weights can also be accessed directly via the dataWeights
field, which is useful for validation:

``` r
outputMapping$dataWeights
#> $`Aciclovir_Profiles.Vergin 1995.Iv.250 mg`
#>  [1] 0.3 0.3 0.3 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
```

### Other options

Weights are always applied at the level of individual data points. Using
a single scalar for a dataset (for example `0.5`) is just a shorthand,
which internally assigns that same weight to every point in the dataset.

The examples below illustrate the different ways weights can be
specified.

#### Dataset-level weights

A scalar weight is broadcast to all points of a dataset:

``` r
## Not run: example only
outputMapping$setDataWeights(
  list(
    "StudyA" = 1.0,  # full influence
    "StudyB" = 0.5   # down-weight entire dataset
  )
)
## End(Not run)
```

#### Point-level weights

A numeric vector with the same length as the dataset’s y-values:

``` r
## Not run: example only
outputMapping$setDataWeights(list(
  "StudyB" = c(0.25, 0.25, 0.25, rep(1.0, 7))  # lower weight on first 3 points
))
```

#### Combined effects

Dataset- and point-level effects can be combined by multiplying them
before passing to `setDataWeights()`:

``` r
## Not run: example only
baseWeight <- 0.6
pointWeights <- ifelse(seq_along(dataSetB$yValues) <= 3, 0.3, 1.0)

outputMapping$setDataWeights(list(
  "StudyB" = baseWeight * pointWeights
))
## End(Not run)
```

## Transformations

Observed data can be adjusted through transformations to improve
comparability with the corresponding simulation output. This is done per
dataset or globally for all datasets in the mapping. Transformations
allow offsets and scaling factors to be applied to x- and y-values, for
example to account for systematic shifts between experiments or to align
starting points across datasets.

``` r
outputMapping$setDataTransformations(
  labels   = "Aciclovir_Profiles.Vergin 1995.Iv.250 mg",
  xOffsets = 0.5, yOffsets = 0,
  xFactors = 1,   yFactors = 1
)

outputMapping$dataTransformations
#> $xOffsets
#>                                          
#>                                      0.0 
#> Aciclovir_Profiles.Vergin 1995.Iv.250 mg 
#>                                      0.5 
#> 
#> $yOffsets
#>                                          
#>                                        0 
#> Aciclovir_Profiles.Vergin 1995.Iv.250 mg 
#>                                        0 
#> 
#> $xFactors
#>                                          
#>                                        1 
#> Aciclovir_Profiles.Vergin 1995.Iv.250 mg 
#>                                        1 
#> 
#> $yFactors
#>                                          
#>                                        1 
#> Aciclovir_Profiles.Vergin 1995.Iv.250 mg 
#>                                        1
```

## Practical tips

- Use scalar dataset weights for coarse emphasis or down-weighting of
  assays.
- Use point-level vectors to emphasize a time window or de-emphasize
  outliers.
- Prefer `log` scaling when values span orders of magnitude.
- If weighting errors occur, first check label names and vector lengths.
