# getOutputsFromConfigurationPlan

Get a list of outputs from simulation and from `ConfigurationPlan`

## Usage

``` r
getOutputsFromConfigurationPlan(configurationPlan)
```

## Arguments

- configurationPlan:

  The configuration plan of a Qualification workflow read from json
  file.

## Value

A dataframe of project, simulation, output paths and (if applicable) pk
parameters and start and end times of interval over which the pk
parameter is evaluated
