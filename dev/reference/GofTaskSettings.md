# GofTaskSettings

R6 class defining properties of time profiles and residuals plot task
settings

## Super class

[`ospsuite.reportingengine::TaskSettings`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/TaskSettings.md)
-\> `GofTaskSettings`

## Public fields

- `referenceData`:

  Data results obtained by TimeProfilesAndResiduals task corresponding
  to referencePopulation

## Active bindings

- `includeReferenceData`:

  logical defining if reference population should be included in time
  profiles and residual plots when applicable

- `outputSelections`:

  is a subset of paths of all outputs belonging to a simulation set, to
  be used to generate a plot with only selected outputs

## Methods

### Public methods

- [`GofTaskSettings$new()`](#method-GofTaskSettings-new)

- [`GofTaskSettings$setStatistics()`](#method-GofTaskSettings-setStatistics)

- [`GofTaskSettings$getStatistics()`](#method-GofTaskSettings-getStatistics)

- [`GofTaskSettings$clone()`](#method-GofTaskSettings-clone)

------------------------------------------------------------------------

### Method `new()`

Create a `GofTaskSettings` object

#### Usage

    GofTaskSettings$new(
      taskName = AllAvailableTasks$plotTimeProfilesAndResiduals,
      outputSelections = NULL,
      statisticsType = NULL
    )

#### Arguments

- `taskName`:

  name of the task using the settings

- `outputSelections`:

  subset of simulationSet outputs to be used in GoF plot

- `statisticsType`:

  Statistics summarizing time profile simulated data

#### Returns

A new `GofTaskSettings` object

------------------------------------------------------------------------

### Method `setStatistics()`

Set statistics used in population time profiles and residuals plots

#### Usage

    GofTaskSettings$setStatistics(
      statisticsType = NULL,
      y = NULL,
      ymin = NULL,
      ymax = NULL,
      yCaption = NULL,
      rangeCaption = NULL
    )

#### Arguments

- `statisticsType`:

  Name of statistics type as defined in enum `StatisticsTypes`

- `y`:

  Function or function name for middle values statistics

- `ymin`:

  Function or function name for min values statistics

- `ymax`:

  Function or function name for max values statistics

- `yCaption`:

  Legend caption for middle values statistics

- `rangeCaption`:

  Legend caption for range values statistics

#### Examples

    \dontrun{
    # Set the statistics as geometric mean
    workflow$plotTimeProfilesAndResiduals$settings$setStatistics(
    statisticsType = StatisticsTypes$`Geometric mean`
    )

    # Set the legend caption displayed for range
    workflow$plotTimeProfilesAndResiduals$settings$setStatistics(
    statisticsType = StatisticsTypes$`Geometric mean`,
    rangeCaption = "90% population range"
    )

    }

------------------------------------------------------------------------

### Method `getStatistics()`

Get statistics used in population time profiles and residuals plots

#### Usage

    GofTaskSettings$getStatistics()

#### Examples

    \dontrun{
    # Get the statistics of time profiles task
    workflow$plotTimeProfilesAndResiduals$settings$getStatistics()
    }

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GofTaskSettings$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## ------------------------------------------------
## Method `GofTaskSettings$setStatistics`
## ------------------------------------------------

if (FALSE) { # \dontrun{
# Set the statistics as geometric mean
workflow$plotTimeProfilesAndResiduals$settings$setStatistics(
statisticsType = StatisticsTypes$`Geometric mean`
)

# Set the legend caption displayed for range
workflow$plotTimeProfilesAndResiduals$settings$setStatistics(
statisticsType = StatisticsTypes$`Geometric mean`,
rangeCaption = "90% population range"
)

} # }


## ------------------------------------------------
## Method `GofTaskSettings$getStatistics`
## ------------------------------------------------

if (FALSE) { # \dontrun{
# Get the statistics of time profiles task
workflow$plotTimeProfilesAndResiduals$settings$getStatistics()
} # }
```
