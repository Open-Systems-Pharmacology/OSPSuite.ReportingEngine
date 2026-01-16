# DataSource

R6 class managing observed data in simulation sets

## Public fields

- `dataFile`:

  file name of the observed dataset

- `metaDataFile`:

  file name of the observed data dictionary

- `caption`:

  displayed legend of the data in the report

- `descriptor`:

  displayed data source descriptor in the report

## Methods

### Public methods

- [`DataSource$new()`](#method-DataSource-new)

- [`DataSource$getCaption()`](#method-DataSource-getCaption)

- [`DataSource$getRelativeDataPath()`](#method-DataSource-getRelativeDataPath)

- [`DataSource$clone()`](#method-DataSource-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `DataSource` object.

#### Usage

    DataSource$new(dataFile, metaDataFile, caption = NULL)

#### Arguments

- `dataFile`:

  file name of the observed dataset

- `metaDataFile`:

  file name of the observed data dictionary

- `caption`:

  displayed legend of the data in the report

#### Returns

A new `DataSource` object

------------------------------------------------------------------------

### Method `getCaption()`

Get the report caption that informs about the data source

#### Usage

    DataSource$getCaption(workflowFolder = NULL)

#### Arguments

- `workflowFolder`:

  Path of workflow folder

#### Returns

A character string

------------------------------------------------------------------------

### Method `getRelativeDataPath()`

Get the relative data path compared to workflow folder

#### Usage

    DataSource$getRelativeDataPath(workflowFolder)

#### Arguments

- `workflowFolder`:

  Path of workflow folder

#### Returns

A character string

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DataSource$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
