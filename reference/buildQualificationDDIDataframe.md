# buildQualificationDDIDataframe

Build dataframe for DDI

## Usage

``` r
buildQualificationDDIDataframe(dataframe, metadata, pkParameter, plotType)
```

## Arguments

- dataframe:

  data.frame

- metadata:

  meta data on `data`

- pkParameter:

  for which DDI ratios are to be evaluated

- plotType:

  for which DDI ratios are to be evaluated. `plotType` is either
  `predictedVsObserved` or `residualsVsObserved`.

## Value

dataframe for plotting goodness of fit of residuals vs time type
