# sectionsAsDataFrame

Recursively parse Sections field of configuration plan to create a
data.frame easier to handle by the workflow

## Usage

``` r
sectionsAsDataFrame(
  sectionsIn,
  sectionsOut = data.frame(),
  parentFolder = "images",
  sectionLevel = 1
)
```

## Arguments

- sectionsIn:

  list including Id and Title of section

- sectionsOut:

  data.frame including id, path, title

- parentFolder:

  For subsections only, path of parent section

- sectionLevel:

  Section level defining the level of markdown title

## Value

A data.frame including information about every section and subsection
