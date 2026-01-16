# saveTaskResults

Save task results within a `TaskResults` object

## Usage

``` r
saveTaskResults(
  id = NULL,
  sectionId = NULL,
  plot = NULL,
  plotCaption = NULL,
  includePlot = NULL,
  table = NULL,
  tableCaption = NULL,
  includeTable = NULL,
  textChunk = NULL,
  includeTextChunk = NULL,
  taskResults = NULL
)
```

## Arguments

- id:

  unique identifier of results, helps in deriving final plot name

- sectionId:

  unique identifier of section results, used only by qualification
  workflows

- plot:

  `ggplot` object corresponding to the figure to be saved

- plotCaption:

  text included into the report explaining the figure

- includePlot:

  logical indicating if the plot should be included in final report

- table:

  data.frame corresponding to the table to be saved

- tableCaption:

  text included into the report explaining the table

- includeTable:

  logical indicating if the table should be included in final report

- textChunk:

  text included into the report explaining the table

- includeTextChunk:

  logical indicating if the text chunk should be included in final
  report

- taskResults:

  A `TaskResults` object

## Value

A `TaskResults` object
