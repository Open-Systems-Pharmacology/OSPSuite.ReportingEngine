#' @title QualificationWorkflow
#' @description  R6 class for Reporting Engine Qualification Workflow
#' @field configurationPlan `ConfigurationPlan` object
#' @field simulate `SimulationTask` object for time profile simulations
#' @field calculatePKParameters `CalculatePKParametersTask` object for PK parameters calculation
#' @field plotTimeProfiles `PlotTask` object for time profile plots
#' @field plotComparisonTimeProfile `PlotTask` object for comparison of time profiles plots
#' @field plotGOFMerged `PlotTask` object for goodness of fit plots
#' @field plotPKRatio `PlotTask` object for PK ratio plot
#' @field plotDDIRatio `PlotTask` object for DDI ratio plot
#' @export
#' @import tlf
#' @import ospsuite.utils
QualificationWorkflow <- R6::R6Class(
  "QualificationWorkflow",
  inherit = Workflow,

  public = list(
    configurationPlan = NULL,
    simulate = NULL,
    calculatePKParameters = NULL,
    plotTimeProfiles = NULL,
    plotGOFMerged = NULL,
    plotComparisonTimeProfile = NULL,
    plotPKRatio = NULL,
    plotDDIRatio = NULL,

    #' @description
    #' Create a new `QualificationWorkflow` object.
    #' @param ... input parameters inherited from R6 class object `Workflow`.
    #' @param configurationPlan A `ConfigurationPlan` object
    #' @return A new `QualificationWorkflow` object
    initialize = function(configurationPlan,
                          ...) {
      super$initialize(...)
      validateIsOfType(configurationPlan, "ConfigurationPlan")
      # Include global plot & axes settings at this stage
      # Global settings are included using theme concept,
      # they can be updated using setting$plotConfigurations within tasks
      configurationPlan$updateTheme()
      self$configurationPlan <- configurationPlan

      self$simulate <- loadSimulateTask(self, active = TRUE)
      self$calculatePKParameters <- loadCalculatePKParametersTask(self, active = TRUE)

      self$plotTimeProfiles <- loadQualificationTimeProfilesTask(self, configurationPlan)
      self$plotGOFMerged <- loadGOFMergedTask(self, configurationPlan)
      self$plotComparisonTimeProfile <- loadQualificationComparisonTimeProfileTask(self, configurationPlan)
      self$plotPKRatio <- loadPlotPKRatioTask(self, configurationPlan)
      self$plotDDIRatio <- loadPlotDDIRatioTask(self, configurationPlan)

      self$taskNames <- enum(self$getAllTasks())
    },

    #' @description
    #' Run qualification workflow tasks for all simulation sets if tasks are activated
    #' The order of tasks is as follows:
    #' # 1) Run simulations
    #' # 2) Perform PK analyses
    #' # 3) Perform plot tasks
    #' ## 3.a) time profiles and residual plots
    #' ## 3.b) comparison time profiles plots
    #' ## 3.c) PK ratio tables and plots
    #' ## 3.d) DDI ratio tables and plots
    #' # 4) Render report
    #' @return All results and plots as a structured output in the workflow folder
    runWorkflow = function() {
      logWorkflow(
        message = "Starting run of qualification workflow",
        pathFolder = self$workflowFolder
      )

      # Before running the actual workflow,
      # Create Outputs for sections and copy intro and section content
      mdFiles <- createSectionOutput(self$configurationPlan, logFolder = self$workflowFolder)
      if (self$simulate$active) {
        self$simulate$runTask(self$simulationStructures)
      }

      if (self$calculatePKParameters$active) {
        self$calculatePKParameters$runTask(self$simulationStructures)
      }

      # The Configuration Plan replaces SimulationStructures for Qualification Workflows
      # since it directly indicates where to save and include results
      for (plotTask in self$getAllPlotTasks()) {
        if (self[[plotTask]]$active) {
          self[[plotTask]]$runTask(self$configurationPlan)
        }
      }

      # Merge appendices into final report
      mergeMarkdowndFiles(mdFiles$appendices, self$reportFileName, logFolder = self$workflowFolder)
      renderReport(
        fileName = self$reportFileName,
        logFolder = self$workflowFolder,
        createWordReport = self$createWordReport,
        numberSections = self$numberSections,
        intro = mdFiles$intro
        )
    },

    #' @description
    #' Update the content of the workflow `configurationPlan`.
    #' Caution, updating the `configurationPlan` using this method won't update the workflow simulations and their results.
    #' Use the method only to bypass reloading a full workflow if only plot aesthetics or section content is changed.
    #' @param configurationPlanFile path to the json file corresponding to the Configuration Plan of a Qualification workflow
    updateConfigurationPlan = function(configurationPlanFile){
      configurationPlan <- loadConfigurationPlan(configurationPlanFile, self$workflowFolder)
      # Update the default plot properties using `tlf` concept of theme
      configurationPlan$updateTheme()
      # Overwrite the properties of the configurationPlan
      self$configurationPlan <- configurationPlan
    }
  )
)
