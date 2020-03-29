#' @title SensitivityAnalysisSettings
#' @description  R6 class for Population Sensitivity Analysis Settings
#' @field variationRange variation range for sensitivity analysis
#' @field numberOfCores number of cores for parallel computation
#' @field quantileVec vector of quantiles to be calculated
#' @field variableParameterPaths vector of paths of parameters to vary when performing sensitivity analysis
#' @field pkParameterSelection list of selected PK parameters for sensitivity analysis
#' @field showProgress sensitivity analysis progress printed to console if TRUE
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
    #' @param showProgress sensitivity analysis progress printed to console if TRUE
    #' @return A new `SensitivityAnalysisSettings` object
    initialize = function(variationRange = NULL,
                          numberOfCores = NULL,
                          quantileVec = NULL,
                          variableParameterPaths = NULL,
                          pkParameterSelection = NULL,
                          showProgress = FALSE) {
      self$variationRange <- variationRange %||% defaultVariationRange
      self$numberOfCores <- numberOfCores %||% defaultSensitivityAnalysisNumberOfCores
      self$quantileVec <- quantileVec %||% defaultQuantileVec
      self$variableParameterPaths <- variableParameterPaths
      self$pkParameterSelection <- pkParameterSelection
      self$showProgress <- showProgress
    }
  ),

  active = list(

    #' @description
    #' Update variation range of sensitivity analysis
    #' @param value variation range for sensitivity analysis
    variationRange = function(value) {
      if (missing(value)) {
        private$.variationRange
      } else {
        validateIsNumeric(value)
        validateIsOfLength(value, nbElements = 1)
        private$.variationRange <- value
      }
    },

    #' @description
    #' Update number of cores to be used during the sensitivity analysis
    #' @param value number of cores for parallel computation
    numberOfCores = function(value) {
      if (missing(value)) {
        private$.numberOfCores
      } else {
        validateIsInteger(value)
        validateIsOfLength(object = value, nbElements = 1)
        private$.numberOfCores <- value
      }
    },

    #' @description
    #' Update vector of quantiles to be calculated for sensitivity analysis
    #' @param value vector of quantiles to be calculated
    quantileVec = function(value) {
      if (missing(value)) {
        private$.quantileVec
      } else {
        validateIsNumeric(value)
        validateNoDuplicatedEntries(value)
        private$.quantileVec <- value
      }
    },

    #' @description
    #' Update vector of paths of parameters to vary during sensitivity analysis
    #' @param value vector of paths of parameters to vary when performing sensitivity analysis
    variableParameterPaths = function(value) {
      if (missing(value)) {
        private$.variableParameterPaths
      } else {
        if (!is.null(value)) {
          validateIsString(value)
          validateNoDuplicatedEntries(value)
          private$.variableParameterPaths <- value
        }
      }
    },

    #' @description
    #' Update list of selected PK parameters for sensitivity analysis
    #' @param value list of selected PK parameters for sensitivity analysis
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

    #' @description
    #' Update `showProgress`
    #' @param value is a logical input.  TRUE shows progress of sensitivity analysis
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
    .showProgress = NULL
  )
)
