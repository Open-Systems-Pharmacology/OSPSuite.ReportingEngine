# generateDDIPlotPKParameterName

Generate name for a PK parameter calculated between a start and end time

## Usage

``` r
generateDDIPlotPKParameterName(pkParameter, startTime, endTime)
```

## Arguments

- pkParameter:

  the name of the PK parameter from the qualification
  `ConfigurationPlan`

- startTime:

  the starting time of the interval over which the PK parameter is
  calculated (from the qualification `ConfigurationPlan`)

- endTime:

  the ending time of the interval over which the PK parameter is
  calculated (from the qualification `ConfigurationPlan`)

## Value

String `pkParameterName`
