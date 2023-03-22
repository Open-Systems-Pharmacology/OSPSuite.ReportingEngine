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
COPY . /RE_Source/
COPY .github/workflows/testRE.R /RE_Source/

# Install OSP R packages
RUN /tmp_setup/install_packages_cran.sh 
RUN /tmp_setup/install_packages_${os_version}.sh 

# Cleanup
RUN rm -rf /tmp_setup

# run lint
# RUN R --no-save -e "lintr::lint_dir(path = 'R', linters=linters_with_defaults(object_name_linter=NULL, line_length_linter=NULL, object_length_linter=NULL, brace_linter=NULL, paren_body_linter=NULL, trailing_whitespace_linter=NULL, trailing_blank_lines_linter=NULL, spaces_left_parentheses_linter=NULL, infix_spaces_linter=NULL))"
RUN R --no-save -e "lintr::lint_dir(path = 'R', linters=c(vector_logic_linter(), object_usage_linter(), seq_linter()))"

# run package tests
RUN R --no-save -e "setwd('/RE_Source/'); source('testRE.R')"

