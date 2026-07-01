# getPopSensDfForPkAndOutput

Retrieve dataframe of ranked and filtered population sensitivity results
for a given PK parameter and model output pathID

## Usage

``` r
getPopSensDfForPkAndOutput(
  simulation,
  sensitivityResultsFolder,
  indexDf,
  output,
  pkParameter,
  totalSensitivityThreshold
)
```

## Arguments

- simulation:

  loaded from the used in the simulationFile path in the simulation set

- sensitivityResultsFolder:

  location of sensitivity analysis results

- indexDf:

  dataframe with contents of population sensitivity results index file

- output:

  pathID of output for which to obtain the population sensitivity
  results

- pkParameter:

  name of PK parameter for which to obtain the population sensitivity
  results

- totalSensitivityThreshold:

  cut-off used for plots of the most sensitive parameters

## Value

sortedFilteredIndividualsDfForPKParameter dataframe of population-wide
sensitivity results for pkParameter and output
