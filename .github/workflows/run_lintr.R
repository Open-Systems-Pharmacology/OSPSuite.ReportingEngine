pkgload::load_all()

lintr::lint_package(
  linters=lintr::linters_with_defaults(
    object_name_linter=lintr::object_name_linter(
      styles=c("camelCase","CamelCase","symbols"), 
      regexes=c(na="na.rm", randomSeed=".Random.seed", tracelib="re.t", linuxCores="cfs_")
      ),
    line_length_linter=NULL, 
    object_length_linter=NULL, 
    brace_linter=NULL, 
    paren_body_linter=NULL, 
    trailing_whitespace_linter=NULL, 
    trailing_blank_lines_linter=NULL, 
    spaces_left_parentheses_linter=NULL, 
    infix_spaces_linter=NULL
  ),
  exclusions = list("tests/dev")
)