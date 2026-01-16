# getPKRatioSummaryStatistics

Calculate and save summary statistics of ratios of PK parameters Note
that this function computes on matrix objects to be faster than on
data.frame when Monte Carlo simulation is performed

## Usage

``` r
getPKRatioSummaryStatistics(pkData, referencePKData)
```

## Arguments

- pkData:

  A matrix of PK Parameter values for Population to compare

- referencePKData:

  A matrix of PK Parameter values for reference Population

## Value

A matrix of the PK Parameter ratios summary statistics
