# PKResult

Structured output of a PK-target parameter identification task,
including optimization results, achieved PK values, and mapping
metadata.

## Methods

### Public methods

- [`PKResult$new()`](#method-PKResult-initialize)

- [`PKResult$toDataFrame()`](#method-PKResult-toDataFrame)

- [`PKResult$toList()`](#method-PKResult-toList)

- [`PKResult$print()`](#method-PKResult-print)

- [`PKResult$clone()`](#method-PKResult-clone)

------------------------------------------------------------------------

### `PKResult$new()`

Initializes a `PKResult` instance. For internal use only.

#### Usage

    PKResult$new(optimResult, piParameters = NULL, pkMappings, achievedPKValues)

#### Arguments

- `optimResult`:

  A named list containing optimization results.

- `piParameters`:

  (Optional) A list of `PIParameter` objects used in the optimization.

- `pkMappings`:

  A list of `PKOutputMapping` objects.

- `achievedPKValues`:

  A list of numeric scalars with the PK parameter values achieved at the
  optimized parameter estimate.

------------------------------------------------------------------------

### `PKResult$toDataFrame()`

Export PKResult to a `data.frame` in long format. Produces N \* M rows,
where N is the number of optimized parameters and M is the number of PK
mappings.

#### Usage

    PKResult$toDataFrame()

#### Returns

A data frame with the following columns:

- `quantityPath`: Simulation quantity path

- `pkParameter`: PK parameter name (e.g., `"C_max"`)

- `targetValue`: Target PK value in display units

- `targetUnit`: Unit of the target value

- `achievedValue`: PK value achieved at the optimized parameter

- `estimatedValue`: Optimized parameter value

- `parameterUnit`: Unit of the estimated parameter (`NA` when
  `piParameters` was not provided)

- `parameterIndex`: Integer index of the optimized parameter (1..N)

- `parameterPath`: Path of the optimized parameter in the simulation
  (`NA` when `piParameters` was not provided)

------------------------------------------------------------------------

### `PKResult$toList()`

Returns the full internal result list.

#### Usage

    PKResult$toList()

#### Returns

A named list containing all result values.

------------------------------------------------------------------------

### `PKResult$print()`

Prints a summary of `PKResult`

#### Usage

    PKResult$print()

------------------------------------------------------------------------

### `PKResult$clone()`

The objects of this class are cloneable with this method.

#### Usage

    PKResult$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
