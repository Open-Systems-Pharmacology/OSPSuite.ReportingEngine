# SensitivityAnalysisSettings

R6 class for Population Sensitivity Analysis Settings

## Active bindings

- `variationRange`:

  variationRange of the sensitivity analysis

- `numberOfCores`:

  number of cores for parallel computation

- `quantileVec`:

  vector of quantiles to be calculated

- `variableParameterPaths`:

  vector of paths of parameters to vary when performing sensitivity
  analysis

- `showProgress`:

  is a logical input. TRUE shows progress of sensitivity analysis

- `allowedCores`:

  is the number of cores assigned to the user session.

## Methods

### Public methods

- [`SensitivityAnalysisSettings$new()`](#method-SensitivityAnalysisSettings-new)

- [`SensitivityAnalysisSettings$clone()`](#method-SensitivityAnalysisSettings-clone)

------------------------------------------------------------------------

### Method `new()`

Create a `SensitivityAnalysisSettings` object

#### Usage

    SensitivityAnalysisSettings$new(
      variationRange = NULL,
      numberOfCores = NULL,
      quantileVec = NULL,
      variableParameterPaths = NULL,
      showProgress = FALSE
    )

#### Arguments

- `variationRange`:

  variation range for sensitivity analysis

- `numberOfCores`:

  number of cores for parallel computation

- `quantileVec`:

  vector of quantiles to be calculated

- `variableParameterPaths`:

  vector of paths of parameters to vary when performing sensitivity
  analysis

- `showProgress`:

  sensitivity analysis progress printed to console if TRUE

#### Returns

A new `SensitivityAnalysisSettings` object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SensitivityAnalysisSettings$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
