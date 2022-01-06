# OSPSuite.ReportingEngine

OSPSuite.ReportingEngine implementation in R

<!-- badges: start -->

[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/Open-Systems-Pharmacology/OSPSuite.ReportingEngine?branch=develop&svg=true)](https://ci.appveyor.com/project/open-systems-pharmacology-ci/OSPSuite-ReportingEngine/branch/develop)
[![codecov](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/branch/develop/graph/badge.svg)](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite.ReportingEngine)

<!-- badges: end -->

# Overview

The **OSPSuite.ReportingEngine** package provides a R frame to create tables and figures from the Open Systems Pharmacology models and combines them into Markdown and MS-Word reports.

- [Documentation](#documentation)
- [Installation](#installation)
- [Usage](#usage)
- [Issue reporting](#issue-reporting)
- [Code of conduct](#code-of-conduct)
- [Contribution](#contribution)
- [Licence](#licence)

# Documentation

Please refer to the [online documentation](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/index.html) for more details on the package

# Installation

The **OSPSuite.ReportingEngine** package is compatible with version 3.6.x **AND** version 4.x.x of R. One of its indirect dependency, **rClr** needs to be installed specifically for the targeted R version. Please follow the installation instructions below:

**OSPSuite.ReportingEngine** requires following packages to be installed:

- [![](https://img.shields.io/static/v1?label=ospsuite&message=v10.x&color=important)](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/releases)
- rClr
  - For [![](https://img.shields.io/static/v1?&logo=r&label=4.x.x&message=rClr&color=important)](https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v0.9.1/rClr_0.9.1.zip)
  - For [![](https://img.shields.io/static/v1?&logo=r&label=3.6.x&message=rClr&color=important)](https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v0.9.1-R3/rClr_0.9.1.zip)
- [![](https://img.shields.io/static/v1?label=tlf&message=v1.2.x&color=important)](https://github.com/Open-Systems-Pharmacology/TLF-Library/releases)
- [![](https://img.shields.io/static/v1?&logo=r&label=&message=R6&color=informational)](https://github.com/r-lib/R6)
- [![](https://img.shields.io/static/v1?&logo=r&label=&message=ggplot2&color=informational)](https://cran.r-project.org/web/packages/ggplot2)

# Usage

In general to create a report, every workflow uses the following steps:

- Creation of `Output` objects defining which simulation paths, associated PK parameters and associated observed data are evaluated.
- Creation of `SimulationSet` objects defining all the relevant information needed to report the evaluations of a model.
- Creation of a `Workflow` object designing, running and reporting of the evaluations of PBPK models developed on PK-Sim or/and Mobi.

```r
# Get the pkml simulation file: "MiniModel2.pkml"
simulationFile <- system.file(
  "extdata", "MiniModel2.pkml",
  package = "ospsuite.reportingengine"
)

# Create Output objects defining which simulation paths, associated PK parameters and associated observed data are evaluated
outputA <- Output$new(
  path = "Organism|A|Concentration in container",
  displayName = "Concentration of A"
)
outputB <- Output$new(
  path = "Organism|B|Concentration in container",
  displayName = "Concentration of B"
)

# Create a SimulationSet object defining all the relevant information needed to report the evaluations of a model
myExampleSet <- SimulationSet$new(
  simulationSetName = "My example",
  simulationFile = simulationFile,
  outputs = c(outputA, outputB)
)

# Create the workflow object designing, running and reporting of the evaluations of PBPK models developed on PK-Sim or/and Mobi
myExampleWorkflow <- MeanModelWorkflow$new(
    simulationSets = myExampleSet,
    workflowFolder = "myExample-Results"
  )

# Set the workflow tasks to be run
myExampleWorkflow$activateTasks(
  c("simulate", "plotTimeProfilesAndResiduals")
)

# Run the workflow and report its evaluations
myExampleWorkflow$runWorkflow(
```

# Issue reporting

Bugs and desired features can be reported on github at the following link:

[https://github.com/open-systems-pharmacology/ospsuite.reportingengine/issues](https://github.com/open-systems-pharmacology/ospsuite.reportingengine/issues)

# Code of conduct

Everyone interacting in the Open Systems Pharmacology community (codebases, issue trackers, chat rooms, mailing lists etc...) is expected to follow the Open Systems Pharmacology [code of conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md).

# Contribution

We encourage contribution to the Open Systems Pharmacology community. Before getting started please read the [contribution guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md). If you are contributing code, please be familiar with the [coding standards](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARDS_R.md).

# License

OSPSuite.ReportingEngine Library is released under the [GPLv2 License](LICENSE).

All trademarks within this document belong to their legitimate owners.
