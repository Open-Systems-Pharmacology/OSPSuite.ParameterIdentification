# PKOutputMapping

Maps a simulation output quantity to a target PK parameter for use in
[`ParameterIdentification`](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/reference/ParameterIdentification.md)
optimization.

## Active bindings

- `quantity`:

  The `Quantity` object from the simulation. Read-only.

- `simId`:

  ID of the root simulation container. Read-only.

- `pkParameter`:

  Name of the PK parameter to target. Must be one of
  [`ospsuite::StandardPKParameter`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/StandardPKParameter.html).

- `targetValueInBaseUnit`:

  Target value in base units. Computed from `targetValue` and
  `targetUnit`. Read-only.

- `targetValue`:

  Numeric target value in units of `targetUnit`. Recomputes
  `targetValueInBaseUnit` on assignment.

- `targetUnit`:

  Unit string for `targetValue`. Must be compatible with the PK
  parameter's dimension. Recomputes `targetValueInBaseUnit` on
  assignment.

## Methods

### Public methods

- [`PKOutputMapping$new()`](#method-PKOutputMapping-initialize)

- [`PKOutputMapping$print()`](#method-PKOutputMapping-print)

- [`PKOutputMapping$clone()`](#method-PKOutputMapping-clone)

------------------------------------------------------------------------

### `PKOutputMapping$new()`

Initialize a new `PKOutputMapping`.

#### Usage

    PKOutputMapping$new(quantity, pkParameter, targetValue, targetUnit)

#### Arguments

- `quantity`:

  An ospsuite `Quantity` object from the simulation whose PK profile
  will be analyzed.

- `pkParameter`:

  Character string specifying the PK parameter to target. Must be one of
  [`ospsuite::StandardPKParameter`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/StandardPKParameter.html)
  (e.g., `"C_max"`, `"AUC_tEnd"`).

- `targetValue`:

  Numeric scalar target value in units of `targetUnit`.

- `targetUnit`:

  Character string specifying the unit of `targetValue`. Must be
  dimensionally compatible with `pkParameter`.

#### Returns

A new `PKOutputMapping` object.

#### Examples

    sim <- ospsuite::loadSimulation("model.pkml")
    quantity <- ospsuite::getQuantity(
      "Organism|PeripheralVenousBlood|Drug|Plasma (Peripheral Venous Blood)",
      container = sim
    )
    mapping <- PKOutputMapping$new(
      quantity = quantity,
      pkParameter = "C_max",
      targetValue = 500,
      targetUnit = "nmol/l"
    )

------------------------------------------------------------------------

### `PKOutputMapping$print()`

Print a summary of the `PKOutputMapping`.

#### Usage

    PKOutputMapping$print()

#### Returns

Called for its side effect of printing. Returns `invisible(self)`.

------------------------------------------------------------------------

### `PKOutputMapping$clone()`

The objects of this class are cloneable with this method.

#### Usage

    PKOutputMapping$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r

## ------------------------------------------------
## Method `PKOutputMapping$new()`
## ------------------------------------------------

if (FALSE) { # \dontrun{
sim <- ospsuite::loadSimulation("model.pkml")
quantity <- ospsuite::getQuantity(
  "Organism|PeripheralVenousBlood|Drug|Plasma (Peripheral Venous Blood)",
  container = sim
)
mapping <- PKOutputMapping$new(
  quantity = quantity,
  pkParameter = "C_max",
  targetValue = 500,
  targetUnit = "nmol/l"
)
} # }
```
