# PopulationSimulationSet

R6 class representing Reporting Engine Population Model Set

## Super class

[`ospsuite.reportingengine::SimulationSet`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/SimulationSet.md)
-\> `PopulationSimulationSet`

## Public fields

- `referencePopulation`:

  logical for reference population used in Pediatric and Ratio
  Comparison workflows

- `populationFile`:

  name of csv file to be used for the population

- `populationName`:

  display name of population

- `studyDesignFile`:

  name of study design csv file

- `plotReferenceObsData`:

  logical for plotting reference observed data in Pediatric and Ratio
  Comparison workflows

## Methods

### Public methods

- [`PopulationSimulationSet$new()`](#method-PopulationSimulationSet-new)

- [`PopulationSimulationSet$copyInputFiles()`](#method-PopulationSimulationSet-copyInputFiles)

- [`PopulationSimulationSet$clone()`](#method-PopulationSimulationSet-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `PopulationSimulationSet` object.

#### Usage

    PopulationSimulationSet$new(
      referencePopulation = FALSE,
      simulationSetName,
      simulationFile,
      populationFile,
      populationName = NULL,
      studyDesignFile = NULL,
      plotReferenceObsData = FALSE,
      ...
    )

#### Arguments

- `referencePopulation`:

  logical for reference population used in Pediatric and Ratio
  Comparison workflows

- `simulationSetName`:

  display name of simulation set

- `simulationFile`:

  names of pkml file to be used for the simulation

- `populationFile`:

  name of csv file to be used for the population

- `populationName`:

  display name of population

- `studyDesignFile`:

  name of study design csv file

- `plotReferenceObsData`:

  logical for plotting reference observed data in Pediatric and Ratio
  Comparison workflows

- `...`:

  inputs inherited from `SimulationSet`

#### Returns

A new `PopulationSimulationSet` object

------------------------------------------------------------------------

### Method `copyInputFiles()`

Copy input files into a simulation set specific folder

#### Usage

    PopulationSimulationSet$copyInputFiles(inputFilesFolder)

#### Arguments

- `inputFilesFolder`:

  where input are located

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PopulationSimulationSet$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
