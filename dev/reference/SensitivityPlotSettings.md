# SensitivityPlotSettings

R6 class for sensitivity analysis plot settings

## Active bindings

- `totalSensitivityThreshold`:

  cut-off used for plots of the most sensitive parameters

- `maximalParametersPerSensitivityPlot`:

  is the maximal number of parameters to display in a sensitivity plot

- `plotConfiguration`:

  `PlotConfiguration` R6 class object from `tlf` library

- `xAxisFontSize`:

  for sensitivity plot. This Value will overwrite values defined by plot
  configuration object

- `yAxisFontSize`:

  for sensitivity plot. This Value will overwrite values defined by plot
  configuration object

- `xLabel`:

  for sensitivity plot. This Value will overwrite values defined by plot
  configuration object

- `yLabel`:

  for sensitivity plot. This Value will overwrite values defined by plot
  configuration object

- `colorPalette`:

  for sensitivity plot. This Value will overwrite values defined by plot
  configuration object

- `maxLinesPerParameter`:

  maximum number of lines allowed per displayed parameters

- `maxWidthPerParameter`:

  maximum number of characters allowed per lines of displayed parameters

## Methods

### Public methods

- [`SensitivityPlotSettings$new()`](#method-SensitivityPlotSettings-new)

- [`SensitivityPlotSettings$clone()`](#method-SensitivityPlotSettings-clone)

------------------------------------------------------------------------

### Method `new()`

Create a `SensitivityPlotSettings` object

#### Usage

    SensitivityPlotSettings$new(
      totalSensitivityThreshold = NULL,
      variableParameterPaths = NULL,
      maximalParametersPerSensitivityPlot = NULL,
      plotConfiguration = NULL,
      xAxisFontSize = 6,
      yAxisFontSize = 6,
      maxLinesPerParameter = NULL,
      maxWidthPerParameter = NULL,
      xLabel = "Sensitivity",
      yLabel = NULL,
      colorPalette = "Spectral"
    )

#### Arguments

- `totalSensitivityThreshold`:

  cut-off used for plots of the most sensitive parameters

- `variableParameterPaths`:

  paths that were varied in the sensitivity analysis. If supplied
  totalSensitivityThreshold = 1, else 0.9.

- `maximalParametersPerSensitivityPlot`:

  maximalParametersPerSensitivityPlot is the maximal number of
  parameters to display in a sensitivity plot

- `plotConfiguration`:

  `PlotConfiguration` object from `tlf` library

- `xAxisFontSize`:

  Font size of x-axis labels for sensitivity plot

- `yAxisFontSize`:

  Font size of y-axis labels for sensitivity plot

- `maxLinesPerParameter`:

  maxLinesPerParameter maximum number of lines allowed per displayed
  parameters

- `maxWidthPerParameter`:

  maximum number of characters allowed per lines of displayed parameters

- `xLabel`:

  Label of x-axis for sensitivity plot

- `yLabel`:

  Label of y-axis for sensitivity plot

- `colorPalette`:

  Name of a color palette to be used by
  [`ggplot2::scale_fill_brewer()`](https://ggplot2.tidyverse.org/reference/scale_brewer.html)
  for sensitivity plot

#### Returns

A new `SensitivityPlotSettings` object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SensitivityPlotSettings$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
