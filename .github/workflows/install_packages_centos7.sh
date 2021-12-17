#!/bin/sh

# Get software 
wget -nv https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/ospsuite-r/artifacts/ospsuite_centOS7.tar.gz -P /tmp_setup/

# Install packages
R --no-save -e "install.packages('remotes', repos='http://cran.rstudio.com/'); remotes::install_github('Open-Systems-Pharmacology/TLF-Library', ref ='develop'); remotes::install_github('Open-Systems-Pharmacology/OSPSuite.RUtils', ref ='main')"
R CMD INSTALL /tmp_setup/ospsuite_centOS7.tar.gz --install-tests
