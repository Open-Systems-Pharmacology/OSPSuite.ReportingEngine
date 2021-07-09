ARG r_version
ARG os_version

FROM ghcr.io/open-systems-pharmacology/ospsuite-r${r_version}-${os_version}:latest

# Args are resetted by FROM - thus need to redeclare! s. https://docs.docker.com/engine/reference/builder/#understand-how-arg-and-from-interact
ARG r_version
ARG os_version

ENTRYPOINT ["sh"]

# Copy installation scripts
RUN mkdir -p /tmp_setup 
COPY .github/workflows/*.sh /tmp_setup/
RUN chmod ugo+x /tmp_setup/*.sh
RUN mkdir -p /RE_Source
COPY R /RE_Source/
COPY tests /RE_Source/
COPY .github/workflows/testRE.R /RE_Source/
RUN ls -la /RE_Source/

# Install OSP R packages
RUN /tmp_setup/install_packages_${os_version}.sh 

# Cleanup
RUN rm -rf /tmp_setup

# run package tests
RUN R --no-save -e "setwd('/RE_Source/'); source('testRE.R')"

