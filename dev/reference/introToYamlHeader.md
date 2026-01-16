# introToYamlHeader

Translate an markdown introduction file into yaml header In order to
include introduction before the table of content, it needs to be
included as cover page features through a yaml header. A yaml header
provides additional arguments to pandoc when translating the md report.
Cover page features can be created with each their own style in the
reference doc

## Usage

``` r
introToYamlHeader(introContent)
```

## Arguments

- introContent:

  Character array of the intro content

## Value

A character array of yaml content
