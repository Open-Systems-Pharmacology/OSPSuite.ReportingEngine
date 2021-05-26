#' @title QualificationWorkflow
#' @description  R6 class for Reporting Engine Qualification Workflow
#' @field configurationPlan `ConfigurationPlan` object
#' @field simulate `SimulationTask` object for time profile simulations
#' @field calculatePKParameters `CalculatePKParametersTask` object for PK parameters calculation
#' @field plotTimeProfiles `PlotTask` object for time profile plots
#' @field plotComparisonTimeProfiles `PlotTask` object for comparison of time profiles plots
#' @field plotGOFMerged `PlotTask` object for goodness of fit plots
#' @field plotPKRatio `PlotTask` object for PK ratio plot
#' @field plotDDIRatio `PlotTask` object for DDI ratio plot
#' @export
#' @import tlf
#' @import ospsuite
QualificationWorkflow <- R6::R6Class(
  "QualificationWorkflow",
  inherit = Workflow,

  public = list(
    configurationPlan = NULL,
    simulate = NULL,
    calculatePKParameters = NULL,
    plotTimeProfiles = NULL,
    plotGOFMerged = NULL,
    plotComparisonTimeProfiles = NULL,
    plotPKRatio = NULL,
    plotDDIRatio = NULL,

    #' @description
    #' Create a new `QualificationWorkflow` object.
    #' @param ... input parameters inherited from R6 class object `Workflow`.
    #' @param configurationPlan A `ConfigurationPlan` object
    #' @return A new `QualificationWorkflow` object
    #' @import ospsuite
    initialize = function(configurationPlan,
                              ...) {
      super$initialize(...)
      validateIsOfType(configurationPlan, "ConfigurationPlan")
      self$configurationPlan <- configurationPlan

      self$simulate <- loadSimulateTask(self)
      self$calculatePKParameters <- loadCalculatePKParametersTask(self)

      self$plotTimeProfiles <- PlotTask$new(workflowFolder = self$workflowFolder)

      self$taskNames <- ospsuite::enum(self$getAllTasks())
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
      appendices <- createSectionOutput(self$configurationPlan, logFolder = self$workflowFolder)
      if (self$simulate$active) {
        self$simulate$runTask(self$simulationStructures)
      }

      if (self$calculatePKParameters$active) {
        self$calculatePKParameters$runTask(self$simulationStructures)
      }

      # The Configuration Plan becomes an inpuut of the task to run
      # This will indicates where the files need to be saved
      # and in which sections to drop each result
      # TODO: the following lines will be wrapped by a QualificationTask object
      if (self$plotTimeProfiles$active) {
        taskResults <- plotQualificationTimeProfiles(self$configurationPlan,
          logFolder = self$workflowFolder,
          settings = NULL
        )
        # TODO A TaskResult class should be created at some point
        # improving all the task saving workflows
        for (resultIndex in seq_along(taskResults$plots)) {
          plotName <- names(taskResults$plots)[resultIndex]
          plotSection <- taskResults$sections[[resultIndex]]
          # Get files for save

          figureFilePath <- file.path(
            self$configurationPlan$getSectionPath(plotSection),
            paste0(plotName, ".png")
          )
          # A file relative to the md is necessary for the report
          figureFileRelativePath <- gsub(pattern = paste0(self$workflowFolder, "/"),
                                         replacement = "", 
                                         x= figureFilePath)

          ggplot2::ggsave(
            filename = figureFilePath,
            plot = taskResults$plots[[plotName]],
            width = ExportPlotConfiguration$width, height = ExportPlotConfiguration$height, units = ExportPlotConfiguration$units
          )
          addTextChunk(self$configurationPlan$getSectionMarkdown(plotSection),
            paste0("Figure: ", taskResults$captions[[plotName]]),
            logFolder = self$workflowFolder
          )
          addFigureChunk(
            fileName = self$configurationPlan$getSectionMarkdown(plotSection),
            figureFileRelativePath = figureFileRelativePath,
            figureFileRootDirectory = self$workflowFolder,
            logFolder = self$workflowFolder
          )
        }
      }

      # Merge appendices into final report
      mergeMarkdowndFiles(appendices, self$reportFileName, logFolder = self$workflowFolder)
      renderReport(self$reportFileName, logFolder = self$workflowFolder, createWordReport = self$createWordReport)
    }
  )
)
