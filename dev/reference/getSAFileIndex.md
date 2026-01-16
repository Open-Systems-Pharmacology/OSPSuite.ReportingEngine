# getSAFileIndex

Function to build and write to CSV a dataframe that stores all
sensitivity analysis result files that will be output by a population
sensitivity analysis.

## Usage

``` r
getSAFileIndex(structureSet, settings, resultsFileName)
```

## Arguments

- structureSet:

  `SimulationStructure` R6 class object

- settings:

  list of settings for the population sensitivity analysis

- resultsFileName:

  root name of population sensitivity analysis results CSV files
