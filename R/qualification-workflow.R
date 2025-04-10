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
#' @family workflows
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
      logCatch({
        validateIsOfType(configurationPlan, "ConfigurationPlan")
        # Include global plot & axes settings at this stage
        # Global settings are included using theme concept,
        # they can be updated using setting$plotConfigurations within tasks
        configurationPlan$updateTheme()
        self$configurationPlan <- configurationPlan

        self$simulate <- loadSimulateTask(self, active = TRUE)

        self$plotTimeProfiles <- loadQualificationTimeProfilesTask(self, configurationPlan)
        self$plotGOFMerged <- loadGOFMergedTask(self, configurationPlan)
        self$plotComparisonTimeProfile <- loadQualificationComparisonTimeProfileTask(self, configurationPlan)
        self$plotPKRatio <- loadPlotPKRatioTask(self, configurationPlan)
        self$plotDDIRatio <- loadPlotDDIRatioTask(self, configurationPlan)
        # PK Parameters need to be calculated only if PKRatio or DDIRatio are plotted
        self$calculatePKParameters <- loadCalculatePKParametersTask(self, active = any(self$plotPKRatio$active, self$plotDDIRatio$active))

        self$taskNames <- enum(self$getAllTasks())
      })
    },

    #' @description
    #' Run qualification workflow tasks for all simulation sets if tasks are activated
    #' The order of tasks is as follows:
    #' 1) Run simulations
    #' 2) Perform PK analyses
    #' 3) Perform plot tasks<br>
    #' a. time profiles and residual plots<br>
    #' b. comparison time profiles plots<br>
    #' c. PK ratio tables and plots<br>
    #' d. DDI ratio tables and plots
    #' 4) Render report
    #' @return All results and plots as a structured output in the workflow folder
    runWorkflow = function() {
      # Prevent crashes if folder was deleted before (re) running a worflow
      dir.create(self$workflowFolder, showWarnings = FALSE, recursive = TRUE)
      # In case other logs were saved before running workflow
      setLogFolder(self$workflowFolder)
      logInfo(messages$runStarting("Qualification Workflow"))
      t0 <- tic()

      logCatch({
        # Before running the actual workflow,
        # Create Outputs for sections and copy intro and section content
        mdFiles <- createSectionOutput(self$configurationPlan)
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
        initialReportPath <- file.path(self$workflowFolder, self$reportFileName)
        mergeMarkdownFiles(mdFiles$appendices, initialReportPath)
        renderReport(
          fileName = initialReportPath,
          createWordReport = self$createWordReport,
          numberSections = self$numberSections,
          intro = mdFiles$intro,
          wordConversionTemplate = self$wordConversionTemplate
        )
        # Move report if a non-default path is provided
        copyReport(from = initialReportPath, to = self$reportFilePath, copyWordReport = self$createWordReport, keep = TRUE)
      })
      logInfo(messages$runCompleted(getElapsedTime(t0), "Qualification Workflow"))
      # Stop logging messages in workflowFolder after run is completed
      # Prevents potential logging of new messages in previous workflowFolder
      setLogFolder()
    },

    #' @description
    #' Update the content of the workflow `configurationPlan`.
    #' Caution, updating the `configurationPlan` using this method won't update the workflow simulations and their results.
    #' Use the method only to bypass reloading a full workflow if only plot aesthetics or section content is changed.
    #' @param configurationPlanFile path to the json file corresponding to the Configuration Plan of a Qualification workflow
    updateConfigurationPlan = function(configurationPlanFile) {
      setLogFolder(self$workflowFolder)
      logCatch({
        configurationPlan <- loadConfigurationPlan(configurationPlanFile, self$workflowFolder)
        # Update the default plot properties using `tlf` concept of theme
        configurationPlan$updateTheme()
        # Overwrite the properties of the configurationPlan
        self$configurationPlan <- configurationPlan
      })
    }
  )
)
