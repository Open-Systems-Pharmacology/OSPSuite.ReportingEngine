# SimulationSet

R6 class representing Reporting Engine Mean Model Set

## Public fields

- `simulationSetName`:

  display name of simulation set

- `simulationFile`:

  names of pkml file to be used for the simulation

- `outputs`:

  list of `Output` R6 class objects

- `dataSource`:

  A `DataSource` object

- `dataSelection`:

  character or expression used to select a subset of observed data

- `timeUnit`:

  display unit for time variable

- `applicationRanges`:

  named list of logicals defining which Application ranges are included
  in

- `minimumSimulationEndTime`:

  is the minimum length of time for which a simulation must be run

- `timeOffset`:

  shift of display time in time profile plots

- `massBalanceSettings`:

  List of mass balance settings reported time profiles and residual
  plots when applicable

## Methods

### Public methods

- [`SimulationSet$new()`](#method-SimulationSet-new)

- [`SimulationSet$clone()`](#method-SimulationSet-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `SimulationSet` object.

#### Usage

    SimulationSet$new(
      simulationSetName,
      simulationFile,
      outputs = NULL,
      dataSource = NULL,
      dataSelection = DataSelectionKeys$ALL,
      timeUnit = "h",
      applicationRanges = ApplicationRanges,
      minimumSimulationEndTime = NULL,
      timeOffset = 0,
      massBalanceFile = NULL
    )

#### Arguments

- `simulationSetName`:

  display name of simulation set

- `simulationFile`:

  names of pkml file to be used for the simulation

- `outputs`:

  list of `Output` R6 class objects

- `dataSource`:

  A `DataSource` object

- `dataSelection`:

  characters or expression to select subset the observed data By
  default, all the data is selected. When using a character array,
  selections are concatenated with the `&` sign

- `timeUnit`:

  display unit for time variable. Default is "h"

- `applicationRanges`:

  names of application ranges to include in the report. Names are
  available in enum `ApplicationRanges`.

- `minimumSimulationEndTime`:

  is the minimum length of time for which a simulation must be run

- `timeOffset`:

  shift of display time in time profile plots

- `massBalanceFile`:

  List of mass balance settings provided either as a json file

#### Returns

A new `SimulationSet` object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SimulationSet$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
