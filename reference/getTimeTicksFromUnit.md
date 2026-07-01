# getTimeTicksFromUnit

Defines auto time ticks from time unit and time values

## Usage

``` r
getTimeTicksFromUnit(unit, timeValues = NULL, maxTicks = 10)
```

## Arguments

- unit:

  A time unit as defined in `ospsuite::ospUnits$Time`

- timeValues:

  Numeric values used by the data

- maxTicks:

  Maximum number of ticks allowed

## Value

List of `ticks` and their `ticklabels`
