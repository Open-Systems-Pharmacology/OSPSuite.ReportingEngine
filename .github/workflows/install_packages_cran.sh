#!/bin/sh

# Install packages
R --no-save -e "install.packages(c('remotes', 'dplyr', 'purrr', 'ospsuite.utils'), repos='http://cran.rstudio.com/')"
R --no-save -e "remotes::install_github('Open-Systems-Pharmacology/TLF-Library', ref ='develop')"
