# Output

R6 class representing workflow output

## Public fields

- `path`:

  path name for the simulation (e.g.
  `Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)`)

- `displayName`:

  display name for `path`

- `displayUnit`:

  display unit for `path`

- `groupID`:

  Grouping Identifier. Outputs with same identifier and unit are plotted
  together

- `color`:

  Displayed line/point color of the Output in plots

- `fill`:

  Displayed range color of the Output in plots

- `dataSelection`:

  character or expression used to select a subset of observed data

- `dataUnit`:

  Unit of the observed data

- `dataDisplayName`:

  display name of the observed data

- `pkParameters`:

  R6 class `PkParameterInfo` objects

- `residualScale`:

  Scale for calculation of residuals as included in enum
  `ResidualScales`

## Methods

### Public methods

- [`Output$new()`](#method-Output-new)

- [`Output$clone()`](#method-Output-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `Output` object.

#### Usage

    Output$new(
      path,
      displayName = NULL,
      displayUnit = NULL,
      groupID = NULL,
      color = NULL,
      fill = NULL,
      dataSelection = DataSelectionKeys$NONE,
      dataUnit = NULL,
      dataDisplayName = NULL,
      pkParameters = NULL,
      residualScale = ResidualScales$Logarithmic
    )

#### Arguments

- `path`:

  path name for the simulation (e.g.
  `Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)`)

- `displayName`:

  display name for `path`

- `displayUnit`:

  display unit for `path`

- `groupID`:

  Grouping Identifier. Outputs with same identifier and unit are plotted
  together

- `color`:

  Displayed line/point color of the `Output` in plots

- `fill`:

  Displayed range color of the `Output` in plots

- `dataSelection`:

  characters or expression to select subset the observed data By
  default, no data is selected. When using a character array, selections
  are concatenated with the `&` sign

- `dataUnit`:

  Unit of the observed data

- `dataDisplayName`:

  display name of the observed data

- `pkParameters`:

  R6 class `PkParameterInfo` objects or their names

- `residualScale`:

  Scale for calculation of residuals as included in enum
  `ResidualScales`

#### Returns

A new `Output` object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Output$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
