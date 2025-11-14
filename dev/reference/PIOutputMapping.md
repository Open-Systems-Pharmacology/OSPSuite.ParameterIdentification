# PIOutputMapping

Establishes connections between simulated quantities and corresponding
observed data sets. Utilized within `ParameterIdentification` instances
to align and compare simulation outputs with empirical data.

## Active bindings

- `observedDataSets`:

  A named list containing `DataSet` objects for comparison with
  simulation outcomes.

- `dataTransformations`:

  A named list of factors and offsets.

- `dataWeights`:

  A named list of y-value weights.

- `quantity`:

  Simulation quantities to be aligned with observed data values.

- `simId`:

  Identifier of the simulation associated with the mapped quantity.

- `scaling`:

  Specifies scaling for output mapping: linear (default) or logarithmic.

- `transformResultsFunction`:

  A function to preprocess simulated results (time and observation
  values) before residual calculation. It takes numeric vectors `xVals`
  and `yVals`, and returns a named list with keys `xVals` and `yVals`.

## Methods

### Public methods

- [`PIOutputMapping$new()`](#method-PIOutputMapping-new)

- [`PIOutputMapping$addObservedDataSets()`](#method-PIOutputMapping-addObservedDataSets)

- [`PIOutputMapping$removeObservedDataSet()`](#method-PIOutputMapping-removeObservedDataSet)

- [`PIOutputMapping$setDataTransformations()`](#method-PIOutputMapping-setDataTransformations)

- [`PIOutputMapping$setDataWeights()`](#method-PIOutputMapping-setDataWeights)

- [`PIOutputMapping$print()`](#method-PIOutputMapping-print)

- [`PIOutputMapping$clone()`](#method-PIOutputMapping-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new instance of the class.

#### Usage

    PIOutputMapping$new(quantity)

#### Arguments

- `quantity`:

  An object of the type `Quantity`.

#### Returns

A new `PIOutputMapping` object. Adds or updates observed data using
`DataSet` objects.

------------------------------------------------------------------------

### Method `addObservedDataSets()`

#### Usage

    PIOutputMapping$addObservedDataSets(data, weights = NULL)

#### Arguments

- `data`:

  A `DataSet` object or a list thereof, matching the simulation quantity
  dimensions.

- `weights`:

  A named list of numeric values or numeric vectors. The names must
  match the names of the observed datasets.

#### Details

Replaces any existing dataset with the same label.

------------------------------------------------------------------------

### Method `removeObservedDataSet()`

Removes specified observed data series.

#### Usage

    PIOutputMapping$removeObservedDataSet(label)

#### Arguments

- `label`:

  The label of the observed data series to remove.

------------------------------------------------------------------------

### Method `setDataTransformations()`

Configures transformations for datasets.

#### Usage

    PIOutputMapping$setDataTransformations(
      labels = NULL,
      xOffsets = 0,
      yOffsets = 0,
      xFactors = 1,
      yFactors = 1
    )

#### Arguments

- `labels`:

  List of dataset labels for targeted transformations. Absence of labels
  applies transformations globally.

- `xOffsets`:

  Numeric list/value for X-offset adjustments.

- `yOffsets`:

  Numeric list/value for Y-offset adjustments.

- `xFactors`:

  Numeric list/value for X-scaling factors.

- `yFactors`:

  Numeric list/value for Y-scaling factors.

------------------------------------------------------------------------

### Method `setDataWeights()`

Assigns weights to observed data sets for residual weighting during
parameter identification.

#### Usage

    PIOutputMapping$setDataWeights(weights)

#### Arguments

- `weights`:

  A named list of numeric values or numeric vectors. The names must
  match the names of the observed datasets.

  Each element in the list can be:

  - a scalar, which will be broadcast to all y-values of the
    corresponding dataset,

  - or a numeric vector matching the number of y-values for that
    dataset.

  To apply both dataset-level and point-level weights, multiply them
  beforehand and provide the combined result as a single numeric vector
  per dataset.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints a summary of the PIOutputMapping.

#### Usage

    PIOutputMapping$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PIOutputMapping$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
