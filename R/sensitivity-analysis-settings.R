#' @title SensitivityAnalysisSettings
#' @description  R6 class for Population Sensitivity Analysis Settings
#' @export
SensitivityAnalysisSettings <- R6::R6Class(
  "SensitivityAnalysisSettings",
  public = list(

    #' @description
    #' Create a `SensitivityAnalysisSettings` object
    #' @param variationRange variation range for sensitivity analysis
    #' @param numberOfCores number of cores for parallel computation
    #' @param quantileVec vector of quantiles to be calculated
    #' @param variableParameterPaths vector of paths of parameters to vary when performing sensitivity analysis
    #' @param pkParameterSelection list of selected PK parameters for sensitivity analysis
    #' @param totalSensitivityThreshold cut-off used for plots of the most sensitive parameters
    #' @param showProgress sensitivity analysis progress printed to console if TRUE
    #' @return A new `SensitivityAnalysisSettings` object
    initialize = function(variationRange = NULL,
                          numberOfCores = NULL,
                          quantileVec = NULL,
                          variableParameterPaths = NULL,
                          pkParameterSelection = NULL,
                          totalSensitivityThreshold = NULL,
                          showProgress = FALSE) {
      self$variationRange <- variationRange %||% defaultVariationRange
      self$numberOfCores <- numberOfCores %||% defaultSensitivityAnalysisNumberOfCores
      self$quantileVec <- quantileVec %||% defaultQuantileVec
      self$variableParameterPaths <- variableParameterPaths
      self$pkParameterSelection <- pkParameterSelection
      self$totalSensitivityThreshold <- getDefaultTotalSensitivityThreshold(totalSensitivityThreshold = totalSensitivityThreshold,
                                                                            variableParameterPaths = variableParameterPaths)
      self$showProgress <- showProgress
    }
  ),

  active = list(
    #' @field variationRange variationRange of the sensitivity analysis
    variationRange = function(value) {
      if (missing(value)) {
        private$.variationRange
      } else {
        validateIsNumeric(value)
        validateIsOfLength(value, nbElements = 1)
        private$.variationRange <- value
      }
    },

    #' @field numberOfCores number of cores for parallel computation
    numberOfCores = function(value) {
      if (missing(value)) {
        private$.numberOfCores
      } else {
        validateIsInteger(value)
        validateIsOfLength(object = value, nbElements = 1)
        private$.numberOfCores <- value
      }
    },

    #' @field quantileVec vector of quantiles to be calculated
    quantileVec = function(value) {
      if (missing(value)) {
        private$.quantileVec
      } else {
        validateIsNumeric(value)
        validateNoDuplicatedEntries(value)
        private$.quantileVec <- value
      }
    },

    #' @field variableParameterPaths vector of paths of parameters to vary when performing sensitivity analysis
    variableParameterPaths = function(value) {
      if (missing(value)) {
        private$.variableParameterPaths
      } else {
        if (!is.null(value)) {
          validateIsString(value)
          validateNoDuplicatedEntries(value)
          private$.variableParameterPaths <- value
          private$.totalSensitivityThreshold <- 1
        }
      }
    },

    #' @field pkParameterSelection list of selected PK parameters for sensitivity analysis
    pkParameterSelection = function(value) {
      if (missing(value)) {
        private$.pkParameterSelection
      } else {
        if (!is.null(value)) {
          validateIsString(value)
          validateNoDuplicatedEntries(value)
          validateIsIncluded(values = value, parentValues = ospsuite::allPKParameterNames())
          private$.pkParameterSelection <- value
        }
      }
    },

    #' @field totalSensitivityThreshold cut-off used for plots of the most sensitive parameters
    totalSensitivityThreshold = function(value) {
      if (missing(value)) {
        private$.totalSensitivityThreshold
      } else {
        if (!is.null(value)) {
          validateIsInRange('totalSensitivityThreshold',value,0,1)
          private$.totalSensitivityThreshold <- value
        }
      }
    },

    #' @field showProgress is a logical input.  TRUE shows progress of sensitivity analysis
    showProgress = function(value) {
      if (missing(value)) {
        private$.
      } else {
        if (!is.null(value)) {
          validateIsLogical(value)
          private$.showProgress <- value
        }
      }
    }
  ),

  private = list(
    .variationRange = NULL,
    .numberOfCores = NULL,
    .quantileVec = NULL,
    .variableParameterPaths = NULL,
    .pkParameterSelection = NULL,
    .totalSensitivityThreshold = NULL,
    .showProgress = NULL
  )
)
