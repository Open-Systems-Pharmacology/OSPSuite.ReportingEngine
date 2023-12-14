# OSPSuite.ReportingEngine

OSPSuite.ReportingEngine implementation in R

<!-- badges: start -->

  ![](https://img.shields.io/github/downloads/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/latest/total?label=Downloads%20latest%20release&logo=data:image/svg%2bxml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0idXRmLTgiPz4KPCEtLSBMaWNlbnNlOiBQRC4gTWFkZSBieSBNYXJ5IEFrdmVvOiBodHRwczovL21hcnlha3Zlby5jb20vIC0tPgo8c3ZnIGZpbGw9IiMwMDAwMDAiIHdpZHRoPSI4MDBweCIgaGVpZ2h0PSI4MDBweCIgdmlld0JveD0iMCAwIDI0IDI0IiBpZD0iZG93bmxvYWQtNSIgZGF0YS1uYW1lPSJMaW5lIENvbG9yIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIGNsYXNzPSJpY29uIGxpbmUtY29sb3IiPjxwb2x5bGluZSBpZD0ic2Vjb25kYXJ5IiBwb2ludHM9IjE1IDE0IDEyIDE3IDkgMTQiIHN0eWxlPSJmaWxsOiBub25lOyBzdHJva2U6IHJnYigyNTUsIDI1NSwgMjU1KTsgc3Ryb2tlLWxpbmVjYXA6IHJvdW5kOyBzdHJva2UtbGluZWpvaW46IHJvdW5kOyBzdHJva2Utd2lkdGg6IDI7Ij48L3BvbHlsaW5lPjxsaW5lIGlkPSJzZWNvbmRhcnktMiIgZGF0YS1uYW1lPSJzZWNvbmRhcnkiIHgxPSIxMiIgeTE9IjE3IiB4Mj0iMTIiIHkyPSIzIiBzdHlsZT0iZmlsbDogbm9uZTsgc3Ryb2tlOiByZ2IoMjU1LCAyNTUsIDI1NSk7IHN0cm9rZS1saW5lY2FwOiByb3VuZDsgc3Ryb2tlLWxpbmVqb2luOiByb3VuZDsgc3Ryb2tlLXdpZHRoOiAyOyI+PC9saW5lPjxwYXRoIGlkPSJwcmltYXJ5IiBkPSJNNCwxN3YzYTEsMSwwLDAsMCwxLDFIMTlhMSwxLDAsMCwwLDEtMVYxNyIgc3R5bGU9ImZpbGw6IG5vbmU7IHN0cm9rZTogcmdiKDI1NSwgMjU1LCAyNTUpOyBzdHJva2UtbGluZWNhcDogcm91bmQ7IHN0cm9rZS1saW5lam9pbjogcm91bmQ7IHN0cm9rZS13aWR0aDogMjsiPjwvcGF0aD48L3N2Zz4=)
  ![](https://img.shields.io/github/downloads/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/total?label=Downloads%20total&logo=data:image/svg%2bxml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0idXRmLTgiPz4KPCEtLSBMaWNlbnNlOiBQRC4gTWFkZSBieSBNYXJ5IEFrdmVvOiBodHRwczovL21hcnlha3Zlby5jb20vIC0tPgo8c3ZnIGZpbGw9IiMwMDAwMDAiIHdpZHRoPSI4MDBweCIgaGVpZ2h0PSI4MDBweCIgdmlld0JveD0iMCAwIDI0IDI0IiBpZD0iZG93bmxvYWQtNSIgZGF0YS1uYW1lPSJMaW5lIENvbG9yIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIGNsYXNzPSJpY29uIGxpbmUtY29sb3IiPjxwb2x5bGluZSBpZD0ic2Vjb25kYXJ5IiBwb2ludHM9IjE1IDE0IDEyIDE3IDkgMTQiIHN0eWxlPSJmaWxsOiBub25lOyBzdHJva2U6IHJnYigyNTUsIDI1NSwgMjU1KTsgc3Ryb2tlLWxpbmVjYXA6IHJvdW5kOyBzdHJva2UtbGluZWpvaW46IHJvdW5kOyBzdHJva2Utd2lkdGg6IDI7Ij48L3BvbHlsaW5lPjxsaW5lIGlkPSJzZWNvbmRhcnktMiIgZGF0YS1uYW1lPSJzZWNvbmRhcnkiIHgxPSIxMiIgeTE9IjE3IiB4Mj0iMTIiIHkyPSIzIiBzdHlsZT0iZmlsbDogbm9uZTsgc3Ryb2tlOiByZ2IoMjU1LCAyNTUsIDI1NSk7IHN0cm9rZS1saW5lY2FwOiByb3VuZDsgc3Ryb2tlLWxpbmVqb2luOiByb3VuZDsgc3Ryb2tlLXdpZHRoOiAyOyI+PC9saW5lPjxwYXRoIGlkPSJwcmltYXJ5IiBkPSJNNCwxN3YzYTEsMSwwLDAsMCwxLDFIMTlhMSwxLDAsMCwwLDEtMVYxNyIgc3R5bGU9ImZpbGw6IG5vbmU7IHN0cm9rZTogcmdiKDI1NSwgMjU1LCAyNTUpOyBzdHJva2UtbGluZWNhcDogcm91bmQ7IHN0cm9rZS1saW5lam9pbjogcm91bmQ7IHN0cm9rZS13aWR0aDogMjsiPjwvcGF0aD48L3N2Zz4=)

  [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/Open-Systems-Pharmacology/OSPSuite.ReportingEngine?branch=develop&svg=true)](https://ci.appveyor.com/project/open-systems-pharmacology-ci/OSPSuite-ReportingEngine/branch/develop)
  [![codecov](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/branch/develop/graph/badge.svg)](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite.ReportingEngine)
  ![Lint Test](https://img.shields.io/github/actions/workflow/status/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/lint.yaml?logo=githubactions&logoColor=white&label=lint)
  ![Linux Test](https://img.shields.io/github/actions/workflow/status/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/LinuxImages.yml?logo=githubactions&logoColor=white&label=linux%20tests)

<!-- badges: end -->

# Installation

The **OSPSuite.ReportingEngine** package is compatible with version 3.6.x **AND** version 4.x.x of R. One of its indirect dependency, **rClr** needs to be installed specifically for the targeted R version. Please follow the installation instructions below:

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
  - **rClr** (s. [Installation instructions for the package **ospsuite**](https://github.com/Open-Systems-Pharmacology/OSPSuite-R#installation))
  - **ospsuite** (>= 11.1)
    - [Package download](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/releases)
    - [Installation instructions](https://github.com/Open-Systems-Pharmacology/OSPSuite-R#installation)

[OPTIONAL] Install **Pandoc** (required for generation of reports in MS-Word format):

* [Pandoc Installer (Windows)](https://github.com/jgm/pandoc/releases/download/2.9.2.1/pandoc-2.9.2.1-windows-x86_64.msi)
* [Pandoc Installer (Linux)](https://github.com/jgm/pandoc/releases/download/2.9.2.1/pandoc-2.9.2.1-linux-amd64.tar.gz)

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
