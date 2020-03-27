#' @title PopulationSensitivityAnalysisSettings
#' @description  R6 class for Population Sensitivity Analysis Settings
#' @field variationRange variation range for sensitivity analysis
#' @field numberOfCores number of cores for parallel computation
#' @field quantileVec vector of quantiles to be calculated
#' @field variableParameterPaths vector of paths of parameters to vary when performing sensitivity analysis
#' @field pkParameterSelection list of selected PK parameters for sensitivity analysis
PopulationSensitivityAnalysisSettings <- R6::R6Class(
  "PopulationSensitivityAnalysisSettings",
  public = list(
    variationRange = NULL,
    numberOfCores = NULL,
    quantileVec = NULL,
    variableParameterPaths = NULL,
    pkParameterSelection = NULL,
    showProgress = NULL,

    #' @description
    #' Create a `PopulationSensitivityAnalysisSettings` object
    #' @param variationRange variation range for sensitivity analysis
    #' @param numberOfCores number of cores for parallel computation
    #' @param quantileVec vector of quantiles to be calculated
    #' @param variableParameterPaths vector of paths of parameters to vary when performing sensitivity analysis
    #' @param pkParameterSelection list of selected PK parameters for sensitivity analysis
    #' @return A new `PopulationSensitivityAnalysisSettings` object
    initialize = function(variationRange = NULL,
                          numberOfCores = NULL,
                          quantileVec = NULL,
                          variableParameterPaths = NULL,
                          pkParameterSelection = NULL,
                          showProgress = NULL) {
      self$updateVariationRange(variationRange %||% defaultVariationRange)
      self$updateNumberOfCores(numberOfCores %||% defaultSensitivityAnalysisNumberOfCores)
      self$updateQuantileVec(quantileVec %||% defaultQuantileVec)
      self$updateVariableParameterPaths(variableParameterPaths)
      self$updatePKParameterSelection(pkParameterSelection)
      self$updateShowProgress(showProgress)
    },


    #' @description
    #' Update variation range of sensitivity analysis
    #' @param variationRange variation range for sensitivity analysis
    updateVariationRange = function(variationRange) {
      validateIsNumeric(variationRange)
      validateIsOfLength(variationRange, nbElements = 1)
      self$variationRange <- variationRange
    },

    #' @description
    #' Update number of cores to be used during the sensitivity analysis
    #' @param numberOfCores number of cores for parallel computation
    updateNumberOfCores = function(numberOfCores) {
      validateIsInteger(numberOfCores)
      validateIsOfLength(object = numberOfCores, nbElements = 1)
      self$numberOfCores <- numberOfCores
    },

    #' @description
    #' Update vector of quantiles to be calculated for sensitivity analysis
    #' @param quantileVec vector of quantiles to be calculated
    updateQuantileVec = function(quantileVec) {
      validateIsNumeric(quantileVec)
      validateNoDuplicatedEntries(quantileVec)
      self$quantileVec <- quantileVec
    },

    #' @description
    #' Update vector of paths of parameters to vary during sensitivity analysis
    #' @param variableParameterPaths vector of paths of parameters to vary when performing sensitivity analysis
    updateVariableParameterPaths = function(variableParameterPaths) {
      if (!is.null(variableParameterPaths)) {
        validateIsString(variableParameterPaths)
        validateNoDuplicatedEntries(variableParameterPaths)
        self$variableParameterPaths <- variableParameterPaths
      }
    },

    #' @description
    #' Update list of selected PK parameters for sensitivity analysis
    #' @param pkParameterSelection list of selected PK parameters for sensitivity analysis
    updatePKParameterSelection = function(pkParameterSelection) {
      if (!is.null(pkParameterSelection)) {
        validateIsString(pkParameterSelection)
        validateNoDuplicatedEntries(pkParameterSelection)
        validateIsIncluded(values = pkParameterSelection, parentValues = ospsuite::allPKParameterNames())
        self$pkParameterSelection <- pkParameterSelection
      }
    },

    #' @description
    #' Update `showProgress`
    #' @param showProgress is a logical input.  TRUE shows progress of sensitivity analysis
    updateShowProgress = function(showProgress) {
      if (!is.null(showProgress)) {
        validateIsLogical(showProgress)
        self$showProgress <- showProgress
      }
    }


  )
)
