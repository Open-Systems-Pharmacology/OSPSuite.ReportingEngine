# OSPSuite.ReportingEngine

OSPSuite.ReportingEngine implementation in R

<!-- badges: start -->

  [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/Open-Systems-Pharmacology/OSPSuite.ReportingEngine?branch=develop&svg=true)](https://ci.appveyor.com/project/open-systems-pharmacology-ci/OSPSuite-ReportingEngine/branch/develop)
  [![codecov](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/branch/develop/graph/badge.svg)](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite.ReportingEngine)

<!-- badges: end -->

# Installation

The **OSPSuite.ReportingEngine** package is compatible with version 3.6.x **AND** version 4.x.x of R. One of its indirect dependency, **rClr** needs to be installed specifically for the targeted R version. Please follow the installation instructions below:

**OSPSuite.ReportingEngine** requires following packages to be installed:
- [ospsuite v10.x](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/releases)
- rClr
  - [For R 4.x.x](https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v0.9.1/rClr_0.9.1.zip)
  - [For R 3.6.x](https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v0.9.1-R3/rClr_0.9.1.zip)
- [tlf v1.2.x](https://github.com/Open-Systems-Pharmacology/TLF-Library/releases)
- [ggplot2](https://cran.r-project.org/web/packages/ggplot2)
- [R6](https://github.com/r-lib/R6)

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
