# analyzeCoreSensitivity

Run a sensitivity analysis for a single individual, varying only the set
of parameters variableParameterPaths

## Usage

``` r
analyzeCoreSensitivity(
  simulation,
  variableParameterPaths = NULL,
  variationRange = 0.1,
  numberOfCores = NULL,
  debugLogFileName = defaultFileNames$logDebugFile(),
  nodeName = NULL,
  showProgress = FALSE
)
```

## Arguments

- simulation:

  simulation class object

- variableParameterPaths:

  paths of parameters to be analyzed

- variationRange:

  variation range for sensitivity analysis

- numberOfCores:

  Number of cores to use on local node. This parameter should be should
  be set to 1 when parallelizing over many nodes.

- debugLogFileName:

  path to debug log file

- nodeName:

  identifier for node used in parallel computation of sensitivity

- showProgress:

  option to print progress of simulation to console

## Value

sensitivity analysis results
