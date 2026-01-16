# getPkOutputIndexDf

Function to filter the population results index file for given
pkParameter and output

## Usage

``` r
getPkOutputIndexDf(indexDf, pkParameter, output)
```

## Arguments

- indexDf:

  dataframe containing summary of sensitivity results

- pkParameter:

  name of PK parameter for which to obtain the population sensitivity
  results

- output:

  pathID of output for which to obtain the population sensitivity
  results

## Value

pkOutputIndexDf dataframe containing index of files containing
population sensitivity analysis results conducted for given output and
pkParameter
