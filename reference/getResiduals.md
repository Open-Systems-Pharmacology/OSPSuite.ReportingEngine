# getResiduals

This function may be reshape to be more generic later on Currently, the
input variable data is a data.frame with "Time", "Concentration" and
"Legend" The function get the simulated data with the time the closest
to the observed data times

## Usage

``` r
getResiduals(
  observedData,
  simulatedData,
  residualScale = ResidualScales$Logarithmic
)
```

## Arguments

- observedData:

  data.frame of time profile observed data

- simulatedData:

  data.frame of time profile simulated data

- residualScale:

  Scale for calculation of residuals as included in enum
  `ResidualScales`

## Value

residualsData data.frame with Time, Observed, Simulated, Residuals
