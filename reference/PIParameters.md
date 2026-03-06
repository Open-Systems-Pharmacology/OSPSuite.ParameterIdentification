# PIParameters

A parameter to be optimized in a parameter identification routine

## Active bindings

- `parameters`:

  A list of parameter objects. Adding or removing parameters is not
  supported.

- `currValue`:

  The current parameter values for simulations, in units specified by
  `$unit`.

- `startValue`:

  Initial value used for optimization.

- `minValue`:

  Lowest permissible parameter value.

- `maxValue`:

  Highest permissible parameter value.

- `unit`:

  Parameter value units. Changing the unit does NOT automatically adjust
  min/max/start values.

## Methods

### Public methods

- [`PIParameters$new()`](#method-PIParameters-new)

- [`PIParameters$setValue()`](#method-PIParameters-setValue)

- [`PIParameters$toDataFrame()`](#method-PIParameters-toDataFrame)

- [`PIParameters$print()`](#method-PIParameters-print)

- [`PIParameters$clone()`](#method-PIParameters-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new instance of the class. The initial start value is
derived from the first parameter upon creation, modifiable via
`PIParameters$startValue`. All parameters are optimized using this
unified value.

#### Usage

    PIParameters$new(parameters)

#### Arguments

- `parameters`:

  List of `Parameter` class objects to be optimized.

#### Returns

A new `PIParameters` object.

------------------------------------------------------------------------

### Method `setValue()`

Updates parameter(s) value. Value is specified in units of `$unit`.

#### Usage

    PIParameters$setValue(value)

#### Arguments

- `value`:

  Numeric value to set.

------------------------------------------------------------------------

### Method `toDataFrame()`

Export parameter metadata and configuration to a data frame. Returns one
row per internal model parameter wrapped by the `PIParameters` object.

#### Usage

    PIParameters$toDataFrame(group = NULL)

#### Arguments

- `group`:

  Optional character label identifying the optimization group.

#### Returns

A `data.frame` with one row per internal parameter.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints a summary of the PIParameters.

#### Usage

    PIParameters$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PIParameters$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
