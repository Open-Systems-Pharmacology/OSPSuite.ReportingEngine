# getQuantileIndividualIds

Find IDs of individuals whose PK analysis results closest to quantiles
given by vector of quantiles quantileVec

## Usage

``` r
getQuantileIndividualIds(pkAnalysisResultsDataframe, quantileVec)
```

## Arguments

- pkAnalysisResultsDataframe:

  Dataframe storing the PK analysis results for multiple individuals for
  a single PK parameter and single output path

- quantileVec:

  vector of quantiles in the pk results distribution. Ids for
  individuals with pk parameter values at these quantiles will be
  returned.

## Value

ids, IDs of individuals whose PK analysis results closest to quantiles
given by vector of quantiles quantileVec
