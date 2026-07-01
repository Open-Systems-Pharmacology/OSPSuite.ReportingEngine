# plotMeanGoodnessOfFit

Plot goodness of fit diagnostics including time profiles, observations
vs predictions, residuals plots (residuals vs time, vs predictions,
qq-plots and histogram)

## Usage

``` r
plotMeanGoodnessOfFit(structureSet, settings = NULL)
```

## Arguments

- structureSet:

  `SimulationStructure` R6 class object

- settings:

  List of settings such as `PlotConfiguration` R6 class objects for each
  goodness of fit plot

## Value

list with `plots`, `tables` and `residuals` objects to be saved
