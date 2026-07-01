# TaskResults

R6 class for TaskResults settings

## Public fields

- `id`:

  unique identifier of results, helps in deriving final plot name

- `sectionId`:

  unique identifier of section results, used only by qualification
  workflows

- `plot`:

  `ggplot` object corresponding to the figure to be saved

- `plotCaption`:

  text included into the report explaining the figure

- `includePlot`:

  logical indicating if the plot should be included in final report

- `table`:

  data.frame corresponding to the table to be saved

- `tableCaption`:

  text included into the report explaining the table

- `includeTable`:

  logical indicating if the table should be included in final report

- `textChunk`:

  text included into the report explaining the table

- `includeTextChunk`:

  logical indicating if the text chunk should be included in final
  report

## Methods

### Public methods

- [`TaskResults$saveFigure()`](#method-TaskResults-saveFigure)

- [`TaskResults$saveTable()`](#method-TaskResults-saveTable)

- [`TaskResults$addFigureToReport()`](#method-TaskResults-addFigureToReport)

- [`TaskResults$addTableToReport()`](#method-TaskResults-addTableToReport)

- [`TaskResults$addTextChunkToReport()`](#method-TaskResults-addTextChunkToReport)

------------------------------------------------------------------------

### Method `saveFigure()`

Save ggplot figure, available as plot task result, into a file

#### Usage

    TaskResults$saveFigure(fileName)

#### Arguments

- `fileName`:

  path of file corresponding to the figure to save

------------------------------------------------------------------------

### Method `saveTable()`

Save data.frame, available as table task result, into a csv file

#### Usage

    TaskResults$saveTable(fileName)

#### Arguments

- `fileName`:

  path of csv file corresponding to the table to save

------------------------------------------------------------------------

### Method `addFigureToReport()`

Write markdown content that adds a figure, available as plot task
result, into a markdown report To be displayed, figure path must be
relative to report location

#### Usage

    TaskResults$addFigureToReport(reportFile, fileRelativePath, fileRootDirectory)

#### Arguments

- `reportFile`:

  markdown file in which the figure and its caption should be added

- `fileRelativePath`:

  figure path relative to `reportFile` location

- `fileRootDirectory`:

  root/working directory needed by `tracelib` package

------------------------------------------------------------------------

### Method `addTableToReport()`

Write markdown content that adds a data.frame, available as table task
result, into a markdown report

#### Usage

    TaskResults$addTableToReport(
      reportFile,
      fileRelativePath,
      fileRootDirectory,
      digits = NULL,
      scientific = NULL
    )

#### Arguments

- `reportFile`:

  markdown file in which the table and its caption should be added

- `fileRelativePath`:

  table path relative to `reportFile` location

- `fileRootDirectory`:

  root/working directory needed by `tracelib` package

- `digits`:

  number of decimal digits in displayed numbers

- `scientific`:

  logical defining if displayed numbers use scientific writing

------------------------------------------------------------------------

### Method `addTextChunkToReport()`

Write markdown content that adds text, available as textChunk task
result, into a markdown report

#### Usage

    TaskResults$addTextChunkToReport(reportFile)

#### Arguments

- `reportFile`:

  markdown file in which the text should be added
