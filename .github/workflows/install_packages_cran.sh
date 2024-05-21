#!/bin/sh

# Install packages
R --no-save -e "install.packages(c('remotes', 'dplyr', 'tidyr', 'purrr', 'patchwork', 'readxl','data.table', 'gridtext', 'ggtext', 'openxlsx'), repos='http://cran.rstudio.com/')"
# R --no-save -e "remotes::install_version(package='lintr', version='3.0.2', repos='http://cran.rstudio.com/', upgrade='always')"
R --no-save -e "remotes::install_github('Open-Systems-Pharmacology/OSPSuite.RUtils', ref ='develop')"
R --no-save -e "remotes::install_github('Open-Systems-Pharmacology/TLF-Library', ref ='develop')"
