# SimulationStructure

R6 class representing that stores file structure (file names and paths)
relating to a particular instance of a simulationSet object

## Public fields

- `workflowFolder`:

  root folder storing workflow results

- `simulationSet`:

  the set for which the SimulationStructure object stores paths input
  and results

- `simulationResultsFolder`:

  path to folder storing results of simulations

- `pkAnalysisResultsFolder`:

  path to folder storing results of pk analyses

- `sensitivityAnalysisResultsFolder`:

  path to folder storing results of sensitivity analyses

- `simulationResultFileNames`:

  vector of names of CSV files storing simulation results

- `pkAnalysisResultsFileNames`:

  vector of names of CSV files storing results of pk analyses

- `pkRatioResultsFileNames`:

  vector of names of CSV files storing results of pk ratios

- `sensitivityAnalysisResultsFileNames`:

  vector of names of CSV files storing results of sensitivity analyses

- `popSensitivityAnalysisResultsIndexFile`:

  path to file containing index of population sensitivity analysis
  results

- `parameterDisplayPaths`:

  data.frame mapping parameters to user-defined display paths

- `simulationSetDescriptor`:

  Descriptor of simulation sets indicated in reports

## Methods

### Public methods

- [`SimulationStructure$new()`](#method-SimulationStructure-new)

- [`SimulationStructure$clone()`](#method-SimulationStructure-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `SimulationStructure` object. Build and store names of
potential subfolders to hold simulation results, pkAnalysis results and
sensitivityAnalysis results.

#### Usage

    SimulationStructure$new(simulationSet, workflowFolder = getwd())

#### Arguments

- `simulationSet`:

  A `SimulationSet` or `PopulationSimulationSet` object

- `workflowFolder`:

  output folder of the workflow

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SimulationStructure$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
