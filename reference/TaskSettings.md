# TaskSettings

R6 class defining properties of plot task settings

## Active bindings

- `bins`:

  number or edges of bins when aggregation is used

- `stairstep`:

  logical defining if plot uses stairstep when aggregation is used

- `digits`:

  number significant digits used in reporting of numbers

- `nsmall`:

  number decimal digits a accordingly with digits used in reporting of
  numbers

- `scientific`:

  logical defining scientific notation is used in reporting of numbers

- `dodge`:

  logical defining if observed data bars dodge simulated data bars in
  demography histogram

- `referenceGlobalRange`:

  logical defining if reference population is plotted as range

- `scales`:

  named list of logicals defining if log/linear plots are included

- `plotConfigurations`:

  named list of `PlotConfiguration` objects defining aesthetic
  properties of reported plots

## Methods

### Public methods

- [`TaskSettings$new()`](#method-TaskSettings-new)

- [`TaskSettings$clone()`](#method-TaskSettings-clone)

------------------------------------------------------------------------

### Method `new()`

Create a `TaskSettings` object

#### Usage

    TaskSettings$new(taskName)

#### Arguments

- `taskName`:

  name of the task using the settings

#### Returns

A new `TaskSettings` object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TaskSettings$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
