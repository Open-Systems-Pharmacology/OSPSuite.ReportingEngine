# StudyDesign

StudyDesign

## Public fields

- `source`:

  expressions used on source data

- `targets`:

  list of targets of expressions and associated values

## Methods

### Public methods

- [`StudyDesign$new()`](#method-StudyDesign-new)

- [`StudyDesign$print()`](#method-StudyDesign-print)

------------------------------------------------------------------------

### Method `new()`

Create a new `StudyDesign` object.

#### Usage

    StudyDesign$new(data, population, simulation)

#### Arguments

- `data`:

  data.frame read from study design file

- `population`:

  `Population` object

- `simulation`:

  `Simulation` object

#### Returns

`StudyDesign` class object

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print study design features

#### Usage

    StudyDesign$print()

#### Returns

data.frame
