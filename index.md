# OSPSuite.ReportingEngine

The
[ospsuite.reportingengine](https://github.com/open-systems-pharmacology/ospsuite.reportingengine)
package provides a framework in R to design and create reports
evaluating PBPK models developed in the [Open Systems
Pharmacology](https://github.com/Open-Systems-Pharmacology) ecosystem.

## Installation

The
[ospsuite.reportingengine](https://github.com/open-systems-pharmacology/ospsuite.reportingengine)
package is compatible with version 4.x.x of R. Please follow the
installation instructions below:

### From Github ![](https://avatars.githubusercontent.com/github)

You can install the development version of
[ospsuite.reportingengine](https://github.com/open-systems-pharmacology/ospsuite.reportingengine)
from [GitHub](https://github.com/) with:

``` r

# install.packages("remotes")
remotes::install_github("Open-Systems-Pharmacology/OSPSuite.ReportingEngine")
```

### Using the package bundle 📦

You can install the
[ospsuite.reportingengine](https://github.com/open-systems-pharmacology/ospsuite.reportingengine)
package by downloading and installing its zip or tar.gz bundle.

- The package bundle for the **release** version is available [here
  📦](https://ci.appveyor.com/project/open-systems-pharmacology-ci/OSPSuite-ReportingEngine/branch/master/artifacts).
- The package bundle for the **development** version is available [here
  📦](https://ci.appveyor.com/project/open-systems-pharmacology-ci/OSPSuite-ReportingEngine/branch/develop/artifacts).
- The package bundles for specific versions of
  [ospsuite.reportingengine](https://github.com/open-systems-pharmacology/ospsuite.reportingengine)
  are available [here
  📦](https://ci.appveyor.com/project/open-systems-pharmacology-ci/OSPSuite-ReportingEngine/history).

Then, to install manually, replace the
`path/to/ospsuite.reportingengine.zip` by your actual local path to the
`.zip` or `tar.gz` file in the code below:

``` r

install.packages(path/to/ospsuite.reportingengine.zip, repos = NULL)
```

To install the package along with its tests, the `.tar.gz` bundle is
required. Then, use the `install-tests` option as illustrated below.

``` r

install.packages(path/to/ospsuite.reportingengine.tar.gz, repos = NULL, INSTALL_opts = "--install-tests")
```

### Required packages

[ospsuite.reportingengine](https://github.com/open-systems-pharmacology/ospsuite.reportingengine)
requires following packages to be installed:

- From the [Open Systems
  Pharmacology](https://github.com/Open-Systems-Pharmacology) ecosystem:

[TABLE]

Once the bundles downloaded, you can install the packages by using the
code below:

``` r

# bundlePath <- path/to/osp/bundles
install.packages(file.path(bundlePath,"ospsuite.utils.zip"), repos = NULL)
install.packages(file.path(bundlePath,"tlf.zip"), repos = NULL)
install.packages(file.path(bundlePath,"rSharp.zip"), repos = NULL)
install.packages(file.path(bundlePath,"ospsuite.zip"), repos = NULL)
```

- Required packages from [CRAN](https://cran.r-project.org/): \> Some of
  these packages may already be installed as dependencies of the Open
  Systems Pharmacology ecosystem
  - [`{dplyr}`](https://cran.r-project.org/web/packages/dplyr)
  - [`{ggplot2}` \>=
    3.3.0](https://cran.r-project.org/web/packages/ggplot2)
  - [`{jsonlite}`](https://cran.r-project.org/web/packages/jsonlite)
  - [`{R6}`](https://cran.r-project.org/web/packages/R6)
  - [`{tidyr}`](https://cran.r-project.org/web/packages/tidyr)
- Optional packages from [CRAN](https://cran.r-project.org/)
  - [`{crayon}`](https://cran.r-project.org/web/packages/crayon)
  - [`{knitr}`](https://cran.r-project.org/web/packages/knitr)
  - [`{parallel}`](https://cran.r-project.org/web/packages/parallel)
  - [`{readxl}`](https://cran.r-project.org/web/packages/readxl)
  - [`{Rmpi}`](https://cran.r-project.org/web/packages/Rmpi)
  - [`{styler}`](https://cran.r-project.org/web/packages/styler)

To install these packages, use the code below:

``` r

# Required packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("jsonlite")
install.packages("R6")
install.packages("tidyr")
# Optional packages
install.packages("crayon")
install.packages("knitr")
install.packages("parallel")
install.packages("readxl")
install.packages("Rmpi")
install.packages("styler")
```

## MS-Word reports

It is possible to convert markdown reports to MS-Word (`.docx` format)
from the
[ospsuite.reportingengine](https://github.com/open-systems-pharmacology/ospsuite.reportingengine)
package. This conversion requires the installation of an additional
software: [Pandoc](https://pandoc.org/).

A dedicated article details how to create MS-Word reports using the
[ospsuite.reportingengine](https://github.com/open-systems-pharmacology/ospsuite.reportingengine)
package:
[here](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/dev/articles/word-report.html)

### \[OPTIONAL\] Pandoc Installation

Install **Pandoc** (required for generation of reports in MS-Word
format) by downloading one of the following files:

- [Pandoc Installer
  (Windows)](https://github.com/jgm/pandoc/releases/download/3.1.2/pandoc-3.1.2-windows-x86_64.msi)

- [Pandoc Installer
  (Linux)](https://github.com/jgm/pandoc/releases/download/3.1.2/pandoc-3.1.2-linux-amd64.tar.gz)

### \[OPTIONAL\] Use SVG figures

In order to use SVG figures in MS-Word report, you need to install
**rsvg-convert** (required by Pandoc for conversion of images in SVG
format)

- [Installer
  (Windows)](https://github.com/miyako/console-rsvg-convert/releases)
  - ⚠ The installation folder must be added to the **system path**. ⚠
- For Linux, **librsvg** package must be installed (package name depends
  on distribution, e.g. **librsvg2-bin** for Ubuntu).

## Documentation 📖

A detailed account of existing functions and articles on how to use them
can be found on the [dedicated
website](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/).

## Code of conduct

Everyone interacting in the Open Systems Pharmacology community
(codebases, issue trackers, chat rooms, mailing lists etc…) is expected
to follow the Open Systems Pharmacology [code of
conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md).

## Contribution 💡

We encourage contribution to the Open Systems Pharmacology community.
Before getting started please read the [contribution
guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md).
If you are contributing code, please be familiar with the [coding
standards](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARDS_R.md).

## License

OSPSuite.ReportingEngine Library is released under the [GPLv2
License](https://www.open-systems-pharmacology.org/OSPSuite.ReportingEngine/LICENSE).

All trademarks within this document belong to their legitimate owners.
