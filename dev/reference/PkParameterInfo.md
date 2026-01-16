# PkParameterInfo

R6 class representing workflow selected PK parameter.

## Public fields

- `pkParameter`:

  PK Parameter name for the simulation (e.g. `AUC_inf`)

- `displayName`:

  display name for `pkParameter`

- `displayUnit`:

  display unit for `pkParameter`

- `group`:

  Grouping identifier

## Methods

### Public methods

- [`PkParameterInfo$new()`](#method-PkParameterInfo-new)

------------------------------------------------------------------------

### Method `new()`

Create a new `PkParameterInfo` object.

#### Usage

    PkParameterInfo$new(
      pkParameter,
      displayName = NULL,
      displayUnit = NULL,
      group = NULL
    )

#### Arguments

- `pkParameter`:

  PK Parameter name for the simulation (e.g. `AUC_inf`) Tips: use
  ospsuite::allPKParameterNames() to get a list of available PK
  parameters

- `displayName`:

  display name for `pkParameter`

- `displayUnit`:

  display unit for `pkParameter`

- `group`:

  Grouping identifier. Default group associate `pkParameter`.

#### Returns

A new `PkParameterInfo` object
