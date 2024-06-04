# OSPSuite.ReportingEngine

OSPSuite.ReportingEngine implementation in R

<!-- badges: start -->

  [![](https://img.shields.io/github/downloads/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/latest/total?label=%E2%AD%B3%20Downloads%20latest%20release)](https://github.com/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/releases/latest)
  [![](https://img.shields.io/github/downloads/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/total?label=%E2%AD%B3%20Downloads%20total)](https://github.com/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/releases)

  [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/Open-Systems-Pharmacology/OSPSuite.ReportingEngine?branch=develop&svg=true)](https://ci.appveyor.com/project/open-systems-pharmacology-ci/OSPSuite-ReportingEngine/branch/develop)
  [![codecov](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/branch/develop/graph/badge.svg)](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite.ReportingEngine)
  [![Lint Test](https://img.shields.io/github/actions/workflow/status/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/lint.yaml?logo=githubactions&logoColor=white&label=lint)](https://github.com/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/actions/workflows/lint.yaml)
  [![Linux Test](https://img.shields.io/github/actions/workflow/status/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/LinuxImages.yml?logo=githubactions&logoColor=white&label=linux%20tests)](https://github.com/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/actions/workflows/LinuxImages.yml)

<!-- badges: end -->

# Installation

The **OSPSuite.ReportingEngine** package is compatible with version 4.x.x of R. Please follow the installation instructions below:

**OSPSuite.ReportingEngine** requires following packages to be installed:

- From CRAN:
  - **ggplot2** (>= 3.3.0)
  - **jsonlite**
  - **knitr**
  - **pander**
  - **patchwork**
  - **readxl**
  - **reshape2**
  - **rmarkdown**
  - **styler**
  - ... (s. [Installation instructions for the package **ospsuite**](https://github.com/Open-Systems-Pharmacology/OSPSuite-R#installation))
- Must be downloaded manually:
  - **ospsuite.utils** (>= 1.4.0)
    - [Package download](https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils/releases)
  - **tlf** (>= 1.5.0)
    - [Package download](https://github.com/Open-Systems-Pharmacology/TLF-Library/releases)
  - **rSharp** (s. [Installation instructions for the package **ospsuite**](https://github.com/Open-Systems-Pharmacology/OSPSuite-R#installation))
  - **ospsuite** (>= 11.1)
    - [Package download](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/releases)
    - [Installation instructions](https://github.com/Open-Systems-Pharmacology/OSPSuite-R#installation)

[OPTIONAL] Install **Pandoc** (required for generation of reports in MS-Word format):

* [Pandoc Installer (Windows)](https://github.com/jgm/pandoc/releases/download/2.9.2.1/pandoc-2.9.2.1-windows-x86_64.msi)
* [Pandoc Installer (Linux)](https://github.com/jgm/pandoc/releases/download/2.9.2.1/pandoc-2.9.2.1-linux-amd64.tar.gz)

[OPTIONAL] Install **rsvg-convert** (required by Pandoc for conversion of images in SVG format)

* [Installer (Windows)](https://github.com/miyako/console-rsvg-convert/releases)
  * The installation folder must be added to the system path.
* For Linux, **librsvg** package must be installed (package name depends on distribution, e.g. **librsvg2-bin** for Ubuntu).

# Development tasks

## dev_mode

  `devtools::dev_mode` function switches your version of R into "development mode". This is useful to avoid clobbering the existing versions of CRAN packages that you need for other tasks. Calling dev_mode() again will turn development mode off, and return you to your default library setup.

```R
# This will install the package in the folder C:/Rpackages
devtools::dev_mode(path="C:/Rpackages")
```

## Reload the package

```R
devtools::load_all()
```

or `Ctrl + Shift + L`

## Add or update script files

  `.R` files defined in `tests\dev\` will be removed from the package and can be used to simulate interaction with the package. See [scripts.R](tests/dev/scripts.R)

## Code of conduct

Everyone interacting in the Open Systems Pharmacology community (codebases, issue trackers, chat rooms, mailing lists etc...) is expected to follow the Open Systems Pharmacology [code of conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md).

## Contribution

We encourage contribution to the Open Systems Pharmacology community. Before getting started please read the [contribution guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md). If you are contributing code, please be familiar with the [coding standards](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARDS_R.md).

## License

OSPSuite.ReportingEngine Library is released under the [GPLv2 License](LICENSE).

All trademarks within this document belong to their legitimate owners.
