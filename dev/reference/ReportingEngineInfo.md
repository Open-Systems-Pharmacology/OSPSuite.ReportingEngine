# ReportingEngineInfo

R6 class representing system information for Reporting Engine

## Public fields

- `Date`:

  Date at which the class is initialized

## Methods

### Public methods

- [`ReportingEngineInfo$new()`](#method-ReportingEngineInfo-new)

- [`ReportingEngineInfo$print()`](#method-ReportingEngineInfo-print)

- [`ReportingEngineInfo$isValidated()`](#method-ReportingEngineInfo-isValidated)

- [`ReportingEngineInfo$clone()`](#method-ReportingEngineInfo-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `ReportingEngineInfo` object.

#### Usage

    ReportingEngineInfo$new()

#### Returns

A new `ReportingEngineInfo` object

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print system information

#### Usage

    ReportingEngineInfo$print()

#### Returns

A text with system information

------------------------------------------------------------------------

### Method `isValidated()`

Is the system validated

#### Usage

    ReportingEngineInfo$isValidated()

#### Returns

A logical reporting if the system is included in the validated systems

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ReportingEngineInfo$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
