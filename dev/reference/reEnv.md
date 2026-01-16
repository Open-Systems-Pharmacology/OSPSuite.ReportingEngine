# reEnv

Environment that holds various global variables and settings for the RE
library. It is not exported and should not be directly manipulated by
other packages.

## Usage

``` r
reEnv
```

## Format

An object of class `environment` of length 39.

## Fields

- `packageName`:

  Name of the package. This will be used to retrieve information on the
  package at run time.

- `log`:

  `Logging` object defining how to record log messages

- `defaultLogPrint`:

  List of logical values indicating whether to print log messages on
  console

- `formatNumericsDigits`:

  Number of digits to display for numeric values in reports

- `formatNumericsScientific`:

  Logical value indicating whether to use scientific notation for
  numeric values in reports

- `defaultBins`:

  Default number of bins to use in histograms

- `binUsingQuantiles`:

  Logical value indicating whether splitting method for bins `TRUE` uses
  same number of points in bins through quantiles `FALSE` uses same
  interval ranges

- `defaultStairstep`:

  Logical value indicating whether to use `stairstep` lines in range
  plots

- `defaultSimulationSetDescriptor`:

  Default descriptor for simulation sets to include in reports

- `maximalParametersPerSensitivityPlot`:

  Maximum number of parameters to include in a sensitivity plot

- `maxWidthPerParameter`:

  Maximum number of characters allowed before adding line breaks in
  sensitivity plot parameter captions.

- `maxLinesPerParameter`:

  Maximum number of lines allowed when line breaks need to be included
  in sensitivity plot parameter captions.

- `maxWidthPerLegendCaption`:

  Maximum number of characters allowed before adding line breaks in
  legend captions.

- `maxLinesPerLegendCaption`:

  Maximum number of lines allowed when line breaks need to be included
  in legend captions.

- `autoAxisLimitMargin`:

  For DDI Ratio plots, value of the margin to add to the limits of the
  axis when left undefined

- `blankLinesBetweenArtifacts`:

  Default number of line breaks to include in reports after inclusion of
  artifact

- `fontScaleFactor`:

  Arbitrary scale factor for font size when exporting plot as png to
  render prettier captions

- `workflowWatermarkMessage`:

  Default watermark message to print when system is not validated

- `residualsHistogramLabel`:

  Default ylabel in goodness of fit residuals histogram plots

- `demographyHistogramLabel`:

  Default ylabel in demography histogram plots

- `residualsQQLabel`:

  Default ylabel in goodness of fit QQ-plots

- `referenceColor`:

  Color of reference simulationSet in plots for Population Workflows

- `referenceFill`:

  Fill color of reference simulationSet in plots for Population
  Workflows

- `defaultErrorbarCapSize`:

  Default size of the error bar caps in the plots

- `defaultSimulationNumberOfCores`:

  default numberOfCores for simulation

- `defaultSensitivityAnalysisNumberOfCores`:

  default numberOfCores for sensitivity analysis

- `defaultVariationRange`:

  default parameter variation range for sensitivity analysis

- `defaultQuantileVec`:

  default quantiles for population sensitivity analysis

- `defaultMaxSimulationsPerCore`:

  Default value for a scale factor used in a parallel simulation. The
  product of this scale factor and the number of allowable cores
  (`allowedCores`) sets the maximum number of simulations that may be
  run on one core.

- `defaultMCRandomSeed`:

  Default Random Seed for Monte Carlo sampling when calculating PK
  Ratios in population workflows

- `defaultMCRepetitions`:

  Default number of repetitions for Monte Carlo sampling when
  calculating PK Ratios in population workflows

- `defaultPKParametersHeader`:

  Default header for PK parameters summary tables in reports

- `pkRatioDictionary`:

  Dictionary of column names for PK Ratio tables

- `ddiRatioListColumnMappings`:

  Dictionary of column names for DDI Ratio tables

- `ddiRatioSubsetsDictionary`:

  Dictionary of sub-sections names for DDI Ratios

- `theme`:

  A `Theme` object from the `{tlf}` package defining default properties
  of the figures

- `defaultPlotFormat`:

  List of default format when exporting plots

- `defaultTimeProfileStatistics`:

  A list of functions and labels defining the summary statistics and
  captions for population time profiles
