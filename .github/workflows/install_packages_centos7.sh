#!/bin/sh

# Get software 
wget -nv https://ci.appveyor.com/api/buildjobs/9qcwkyuo2orotq0r/artifacts/ospsuite_10.0.39_centOS7.tar.gz -P /tmp_setup/

# Install packages
R --no-save -e "install.packages('remotes', repos='http://cran.rstudio.com/');remotes::install_github('Open-Systems-Pharmacology/TLF-Library', ref ='develop')"
R CMD INSTALL /tmp_setup/ospsuite_10.0.39_centOS7.tar.gz --install-tests
