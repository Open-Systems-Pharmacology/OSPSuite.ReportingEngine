# getPKRatioSummaryFromMCSampling

Get PK Parameter Ratio Measure From Monte Carlo Sampling

## Usage

``` r
getPKRatioSummaryFromMCSampling(
  pkData,
  referencePKData,
  simulationSetName,
  settings = NULL
)
```

## Arguments

- pkData:

  A data.frame of PK Parameter values for Population to compare

- referencePKData:

  A data.frame of PK Parameter values for reference Population

- simulationSetName:

  Name of simulation set

- settings:

  A list of task settings

## Value

A data.frame of the PK Parameter ratios summary statistics
