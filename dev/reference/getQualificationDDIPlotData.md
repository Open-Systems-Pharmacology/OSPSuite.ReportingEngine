# getQualificationDDIPlotData

Build dataframes and metadata for each DDI plot

## Usage

``` r
getQualificationDDIPlotData(configurationPlan)
```

## Arguments

- configurationPlan:

  The configuration plan of a Qualification workflow read from json
  file.

## Value

plotDDIdata, a list of lists of the form list(dataframe,metadata)
specific to each DID plot
