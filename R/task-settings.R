#' @title TaskSettings
#' @description  R6 class defining properties of plot task settings
#' @importFrom ospsuite.utils %||%
#' @keywords internal
TaskSettings <- R6::R6Class(
  "TaskSettings",
  # This allows the R6 class to accept new fields
  lock_objects = FALSE,
  public = list(
    #' @description
    #' Create a `TaskSettings` object
    #' @param taskName name of the task using the settings
    #' @return A new `TaskSettings` object
    initialize = function(taskName) {
      validateIsIncluded(taskName, AllAvailableTasks)

      private$.bins <- reEnv$defaultBins
      private$.stairstep <- reEnv$defaultStairstep
      private$.digits <- reEnv$formatNumericsDigits
      private$.nsmall <- reEnv$formatNumericsSmall
      private$.scientific <- reEnv$formatNumericsScientific
      private$.scales <- list(Linear = TRUE, Logarithmic = TRUE)
      private$.plotConfigurations <- list()

      # Plot configurations fields depend on the task
      if (isIncluded(taskName, AllAvailableTasks$plotAbsorption)) {
        private$.plotConfigurations <- list(absorptionPlot = NULL)
      }
      if (isIncluded(taskName, AllAvailableTasks$plotMassBalance)) {
        private$.plotConfigurations <- list(
          timeProfile = NULL,
          cumulativeTimeProfile = NULL,
          normalizedTimeProfile = NULL,
          normalizedCumulativeTimeProfile = NULL,
          pieChart = NULL
        )
      }
      if (isIncluded(taskName, AllAvailableTasks$plotDemography)) {
        private$.plotConfigurations <- list(
          histogram = NULL,
          vpcParameterPlot = NULL,
          comparisonVpcPlot = NULL
        )
        private$.dodge <- TRUE
        private$.referenceGlobalRange <- TRUE
        private$.scales <- list(Linear = TRUE, Logarithmic = FALSE)
      }
      if (isIncluded(taskName, AllAvailableTasks$plotPKParameters)) {
        private$.plotConfigurations <- list(
          boxplotPKParameters = NULL,
          boxplotPKParametersLog = NULL,
          boxplotPKRatios = NULL,
          vpcParameterPlot = NULL,
          comparisonVpcPlot = NULL
        )
        self$mcRepetitions <- getDefaultMCRepetitions()
        self$mcRandomSeed <- getDefaultMCRandomSeed()
      }
      if (isIncluded(taskName, AllAvailableTasks$plotTimeProfilesAndResiduals)) {
        private$.plotConfigurations <- list(
          timeProfile = NULL,
          obsVsPred = NULL,
          resVsPred = NULL,
          resVsTime = NULL,
          resHisto = NULL,
          resQQPlot = NULL,
          histogram = NULL,
          qqPlot = NULL
        )
      }
    }
  ),
  active = list(
    #' @field bins number or edges of bins when aggregation is used
    bins = function(value) {
      if (missing(value)) {
        return(private$.bins)
      }
      validateIsNumeric(value, nullAllowed = TRUE)
      private$.bins <- value %||% private$.bins
      return(invisible())
    },
    #' @field stairstep logical defining if plot uses stairstep when aggregation is used
    stairstep = function(value) {
      if (missing(value)) {
        return(private$.stairstep)
      }
      validateIsLogical(value, nullAllowed = TRUE)
      private$.stairstep <- value %||% private$.stairstep
      return(invisible())
    },
    #' @field digits number significant digits used in reporting of numbers
    digits = function(value) {
      if (missing(value)) {
        return(private$.digits)
      }
      validateIsInteger(value, nullAllowed = TRUE)
      private$.digits <- value %||% private$.digits
      return(invisible())
    },
    #' @field nsmall number decimal digits a accordingly with digits used in reporting of numbers
    nsmall = function(value) {
      if (missing(value)) {
        return(private$.nsmall)
      }
      validateIsInteger(value, nullAllowed = TRUE)
      private$.nsmall <- value %||% private$.nsmall
      return(invisible())
    },
    #' @field scientific logical defining scientific notation is used in reporting of numbers
    scientific = function(value) {
      if (missing(value)) {
        return(private$.scientific)
      }
      validateIsLogical(value, nullAllowed = TRUE)
      private$.scientific <- value %||% private$.scientific
      return(invisible())
    },
    #' @field dodge logical defining if observed data bars dodge simulated data bars in demography histogram
    dodge = function(value) {
      if (missing(value)) {
        return(private$.dodge)
      }
      validateIsLogical(value, nullAllowed = TRUE)
      private$.dodge <- value %||% private$.dodge
      return(invisible())
    },
    #' @field referenceGlobalRange logical defining if reference population is plotted as range
    referenceGlobalRange = function(value) {
      if (missing(value)) {
        return(private$.referenceGlobalRange)
      }
      validateIsLogical(value, nullAllowed = TRUE)
      private$.referenceGlobalRange <- value %||% private$.referenceGlobalRange
      return(invisible())
    },
    #' @field scales named list of logicals defining if log/linear plots are included
    scales = function(value) {
      if (missing(value)) {
        return(private$.scales)
      }
      private$.scales <- value %||% private$.scales
      return(invisible())
    },
    #' @field plotConfigurations named list of `PlotConfiguration` objects
    #' defining aesthetic properties of reported plots
    plotConfigurations = function(value) {
      if (missing(value)) {
        return(private$.plotConfigurations)
      }
      private$.plotConfigurations <- value %||% private$.plotConfigurations
      return(invisible())
    }
  ),
  private = list(
    .plotConfigurations = NULL,
    .bins = NULL,
    .stairstep = NULL,
    .digits = NULL,
    .nsmall = NULL,
    .scientific = NULL,
    .scales = NULL,
    .dodge = NULL,
    .referenceGlobalRange = FALSE
  )
)


#' @title GofTaskSettings
#' @description  R6 class defining properties of time profiles and residuals plot task settings
#' @keywords internal
GofTaskSettings <- R6::R6Class(
  "GofTaskSettings",
  inherit = TaskSettings,
  public = list(
    #' @field referenceData Data results obtained by TimeProfilesAndResiduals task corresponding to referencePopulation
    referenceData = NULL,

    #' @description
    #' Create a `GofTaskSettings` object
    #' @param taskName name of the task using the settings
    #' @param outputSelections subset of simulationSet outputs to be used in GoF plot
    #' @param statisticsType Statistics summarizing time profile simulated data
    #' @return A new `GofTaskSettings` object
    initialize = function(taskName = AllAvailableTasks$plotTimeProfilesAndResiduals,
                          outputSelections = NULL,
                          statisticsType = NULL) {
      validateIsIncluded(taskName, AllAvailableTasks$plotTimeProfilesAndResiduals)
      validateIsIncluded(statisticsType, StatisticsTypes, nullAllowed = TRUE)

      super$initialize(taskName)

      private$.includeReferenceData <- TRUE
      private$.outputSelections <- outputSelections
      private$.statistics <- reEnv$defaultTimeProfileStatistics
      self$setStatistics(statisticsType = statisticsType)
    },

    #' @description Set statistics used in population time profiles and residuals plots
    #' @param statisticsType Name of statistics type as defined in enum `StatisticsTypes`
    #' @param y Function or function name for middle values statistics
    #' @param ymin Function or function name for min values statistics
    #' @param ymax Function or function name for max values statistics
    #' @param yCaption Legend caption for middle values statistics
    #' @param rangeCaption Legend caption for range values statistics
    #' @examples \dontrun{
    #' # Set the statistics as geometric mean
    #' workflow$plotTimeProfilesAndResiduals$settings$setStatistics(
    #' statisticsType = StatisticsTypes$`Geometric mean`
    #' )
    #'
    #' # Set the legend caption displayed for range
    #' workflow$plotTimeProfilesAndResiduals$settings$setStatistics(
    #' statisticsType = StatisticsTypes$`Geometric mean`,
    #' rangeCaption = "90% population range"
    #' )
    #'
    #' }
    #'
    setStatistics = function(statisticsType = NULL,
                             y = NULL,
                             ymin = NULL,
                             ymax = NULL,
                             yCaption = NULL,
                             rangeCaption = NULL) {
      validateIsIncluded(statisticsType, StatisticsTypes, nullAllowed = TRUE)
      # Allow user to enter the function directly
      validateIsOfType(y, c("character", "closure"), nullAllowed = TRUE)
      validateIsOfType(ymin, c("character", "closure"), nullAllowed = TRUE)
      validateIsOfType(ymax, c("character", "closure"), nullAllowed = TRUE)

      if (!isEmpty(statisticsType)) {
        private$.statistics <- getStatisticsFromType(statisticsType)
      }
      # Assign variables to reEnv only if defined
      eval(parseVariableToObject(
        objectName = "private$.statistics",
        variableName = c("y", "ymin", "ymax", "yCaption", "rangeCaption"),
        keepIfNull = TRUE
      ))
      return(invisible())
    },

    #' @description Get statistics used in population time profiles and residuals plots
    #' @examples \dontrun{
    #' # Get the statistics of time profiles task
    #' workflow$plotTimeProfilesAndResiduals$settings$getStatistics()
    #' }
    #'
    getStatistics = function() {
      return(private$.statistics)
    }
  ),
  active = list(
    #' @field includeReferenceData logical defining if reference population should be included in
    #' time profiles and residual plots when applicable
    includeReferenceData = function(value) {
      if (missing(value)) {
        return(private$.includeReferenceData)
      }
      validateIsLogical(value, nullAllowed = TRUE)
      private$.includeReferenceData <- value %||% private$.includeReferenceData
      return(invisible())
    },

    #' @field outputSelections is a subset of paths of all outputs belonging to a simulation set, to be used to generate a plot with only selected outputs
    outputSelections = function(value) {
      if (missing(value)) {
        return(private$.outputSelections)
      }
      validateIsString(value, nullAllowed = TRUE)
      private$.outputSelections <- value %||% private$.outputSelections
      return(invisible())
    }
  ),
  private = list(
    .includeReferenceData = NULL,
    .outputSelections = NULL,
    .statistics = NULL
  )
)
