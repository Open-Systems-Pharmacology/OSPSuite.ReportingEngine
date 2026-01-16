# ConfigurationPlan

R6 class for ConfigurationPlan guiding Qualification Workflow

## See also

Other qualification workflow:
[`QualificationVersionInfo`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/QualificationVersionInfo.md),
[`adjustTitlePage()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/adjustTitlePage.md),
[`loadConfigurationPlan()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadConfigurationPlan.md),
[`loadQualificationWorkflow()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/loadQualificationWorkflow.md),
[`startQualificationRunner()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/reference/startQualificationRunner.md)

## Public fields

- `plots`:

  list defining `PlotConfiguration` settings for specific tasks

- `inputs`:

  data.frame or list mapping input files to their paths

- `intro`:

  data.frame or list mapping introduction files to their paths

- `markdownIntro`:

  name of markdown file that includes intro content

- `referenceFolder`:

  Reference path for accessing inputs

- `workflowFolder`:

  path of the output folder created or used by the Workflow.

## Active bindings

- `sections`:

  data.frame mapping section ids to their paths

- `simulationMappings`:

  data.frame mapping simulations to their paths

- `observedDataSets`:

  data.frame mapping observed datasets to their paths

## Methods

### Public methods

- [`ConfigurationPlan$getSectionPath()`](#method-ConfigurationPlan-getSectionPath)

- [`ConfigurationPlan$getSectionTitle()`](#method-ConfigurationPlan-getSectionTitle)

- [`ConfigurationPlan$getSectionMarkdown()`](#method-ConfigurationPlan-getSectionMarkdown)

- [`ConfigurationPlan$getSectionLevel()`](#method-ConfigurationPlan-getSectionLevel)

- [`ConfigurationPlan$getIntroMarkdown()`](#method-ConfigurationPlan-getIntroMarkdown)

- [`ConfigurationPlan$copySectionContent()`](#method-ConfigurationPlan-copySectionContent)

- [`ConfigurationPlan$copyInput()`](#method-ConfigurationPlan-copyInput)

- [`ConfigurationPlan$copyIntro()`](#method-ConfigurationPlan-copyIntro)

- [`ConfigurationPlan$copyContentFiles()`](#method-ConfigurationPlan-copyContentFiles)

- [`ConfigurationPlan$getObservedDataPath()`](#method-ConfigurationPlan-getObservedDataPath)

- [`ConfigurationPlan$getMolWeightForObservedData()`](#method-ConfigurationPlan-getMolWeightForObservedData)

- [`ConfigurationPlan$getSimulationPath()`](#method-ConfigurationPlan-getSimulationPath)

- [`ConfigurationPlan$getPopulationPath()`](#method-ConfigurationPlan-getPopulationPath)

- [`ConfigurationPlan$getSimulationResultsPath()`](#method-ConfigurationPlan-getSimulationResultsPath)

- [`ConfigurationPlan$getPKAnalysisResultsPath()`](#method-ConfigurationPlan-getPKAnalysisResultsPath)

- [`ConfigurationPlan$updateTheme()`](#method-ConfigurationPlan-updateTheme)

------------------------------------------------------------------------

### Method `getSectionPath()`

Get location of directory corresponding to a specific section Id

#### Usage

    ConfigurationPlan$getSectionPath(id)

#### Arguments

- `id`:

  section identifier

#### Returns

The section path corresponding to the id in the configuration plan field
`sections`

------------------------------------------------------------------------

### Method `getSectionTitle()`

Get markdown title to a specific section Id

#### Usage

    ConfigurationPlan$getSectionTitle(id)

#### Arguments

- `id`:

  section identifier

#### Returns

The title associated with "#" corresponding to the subsection level

------------------------------------------------------------------------

### Method `getSectionMarkdown()`

Get location of .md file corresponding to a specific section Id

#### Usage

    ConfigurationPlan$getSectionMarkdown(id)

#### Arguments

- `id`:

  section identifier

#### Returns

The markdown file corresponding to the id in the configuration plan
field `sections`

------------------------------------------------------------------------

### Method `getSectionLevel()`

Get section level

#### Usage

    ConfigurationPlan$getSectionLevel(id)

#### Arguments

- `id`:

  section identifier

#### Returns

Section level

------------------------------------------------------------------------

### Method `getIntroMarkdown()`

Get location of .md intro file

#### Usage

    ConfigurationPlan$getIntroMarkdown()

#### Returns

The markdown file corresponding to introduction

------------------------------------------------------------------------

### Method `copySectionContent()`

Copy content to section markdown

#### Usage

    ConfigurationPlan$copySectionContent(id)

#### Arguments

- `id`:

  section identifier

------------------------------------------------------------------------

### Method `copyInput()`

Copy input to section markdown

#### Usage

    ConfigurationPlan$copyInput(input)

#### Arguments

- `input`:

  list including SectionId and Path

------------------------------------------------------------------------

### Method `copyIntro()`

Copy intro to intro markdown

#### Usage

    ConfigurationPlan$copyIntro(intro)

#### Arguments

- `intro`:

  list including Path

------------------------------------------------------------------------

### Method `copyContentFiles()`

If available, copy all files and folders of `Content` directory into
`<workflowFolder>`

#### Usage

    ConfigurationPlan$copyContentFiles()

------------------------------------------------------------------------

### Method `getObservedDataPath()`

Get location of observed data corresponding to a specific
observedDataSet Id

#### Usage

    ConfigurationPlan$getObservedDataPath(id)

#### Arguments

- `id`:

  observedDataSet identifier

#### Returns

The observed data file path corresponding to the id in the configuration
plan field `observedDataSet`

------------------------------------------------------------------------

### Method `getMolWeightForObservedData()`

Get molecular weight of observed data corresponding to a specific
observedDataSet Id

#### Usage

    ConfigurationPlan$getMolWeightForObservedData(id)

#### Arguments

- `id`:

  observedDataSet identifier

#### Returns

The observed data file path corresponding to the id in the configuration
plan field `observedDataSet`

------------------------------------------------------------------------

### Method `getSimulationPath()`

Get location of simulation file corresponding to a specific simulation
and project names

#### Usage

    ConfigurationPlan$getSimulationPath(project, simulation)

#### Arguments

- `project`:

  name of simulation project

- `simulation`:

  name of the simulation

#### Returns

The simulation file path corresponding to the project and simulation in
the configuration plan field `simulationMappings`

------------------------------------------------------------------------

### Method `getPopulationPath()`

Get location of population file corresponding to a specific simulation
and project names

#### Usage

    ConfigurationPlan$getPopulationPath(project, simulation)

#### Arguments

- `project`:

  name of simulation project

- `simulation`:

  name of the simulation

#### Returns

The population file path corresponding to the project and simulation in
the configuration plan field `simulationMappings`

------------------------------------------------------------------------

### Method `getSimulationResultsPath()`

Get location of simulation result file corresponding to a specific
simulation and project names

#### Usage

    ConfigurationPlan$getSimulationResultsPath(project, simulation)

#### Arguments

- `project`:

  name of simulation project

- `simulation`:

  name of the simulation

#### Returns

The simulation results file path corresponding to the project and
simulation in the configuration plan field `simulationMappings`

------------------------------------------------------------------------

### Method `getPKAnalysisResultsPath()`

Get location of PK Analysis result file corresponding to a specific
simulation and project names

#### Usage

    ConfigurationPlan$getPKAnalysisResultsPath(project, simulation)

#### Arguments

- `project`:

  name of simulation project

- `simulation`:

  name of the simulation

#### Returns

The PKAnalysis results file path corresponding to the project and
simulation in the configuration plan field `simulationMappings`

------------------------------------------------------------------------

### Method `updateTheme()`

Update environment theme that will be used as default during workflow

#### Usage

    ConfigurationPlan$updateTheme()
