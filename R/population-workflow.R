#' @title PopulationWorkflow
#' @description R6 class for Reporting Engine Population Workflow
#' @field workflowType Type of population workflow
#' @field simulate `SimulationTask` object for time profile simulations
#' @field calculatePKParameters `CalculatePKParametersTask` object for PK parameters calculation
#' @field calculateSensitivity `SensitivityAnalysisTask` object for sensitivity analysis
#' @field plotDemography R6 class `Task` for demography plots
#' @field plotTimeProfilesAndResiduals `PlotTask` object for goodness of fit plots
#' @field plotPKParameters R6 class `Task` for PK parameters plot
#' @field plotSensitivity R6 class `Task` for sensitivity plot
#' @export
#' @import tlf
#' @import ospsuite
#' @family workflows
PopulationWorkflow <- R6::R6Class(
  "PopulationWorkflow",
  inherit = Workflow,
  public = list(
    workflowType = NULL,
    simulate = NULL,
    calculatePKParameters = NULL,
    calculateSensitivity = NULL,
    plotDemography = NULL,
    plotTimeProfilesAndResiduals = NULL,
    plotPKParameters = NULL,
    plotSensitivity = NULL,

    #' @description
    #' Create a new `PopulationWorkflow` object.
    #' @param workflowType Type of population workflow. Use enum `PopulationWorkflowTypes` to get list of workflow types.
    #' @param simulationSets list of `SimulationSet` R6 class objects
    #' @param workflowFolder path of the output folder created or used by the Workflow.
    #' @param createWordReport logical of option for creating Markdwon-Report only but not a Word-Report.
    #' @param watermark displayed watermark in every plot background
    #' @param simulationSetDescriptor character Descriptor of simulation sets indicated in reports
    #' @param numberSections logical defining if the report sections should be numbered
    #' @param theme A `Theme` object from `{tlf}` package
    #' @return A new `PopulationWorkflow` object
    initialize = function(workflowType,
                          simulationSets,
                          workflowFolder,
                          createWordReport = TRUE,
                          watermark = NULL,
                          simulationSetDescriptor = NULL,
                          numberSections = TRUE,
                          theme = NULL) {
      super$initialize(
        simulationSets = simulationSets,
        workflowFolder = workflowFolder,
        createWordReport = createWordReport,
        watermark = watermark,
        simulationSetDescriptor = simulationSetDescriptor,
        numberSections = numberSections,
        theme = theme
      )

      tryCatch(
        {
          validateIsOfType(c(simulationSets), "PopulationSimulationSet")
          if (!isOfType(simulationSets, "list")) {
            simulationSets <- list(simulationSets)
          }
          validateIsIncluded(workflowType, PopulationWorkflowTypes)
          self$workflowType <- workflowType
          # Pediatric and ratio comparison workflows need ONE reference population
          validateHasReferencePopulation(workflowType, simulationSets)

          self$simulate <- loadSimulateTask(self)
          self$calculatePKParameters <- loadCalculatePKParametersTask(self)
          self$calculateSensitivity <- loadCalculateSensitivityTask(self)

          self$plotTimeProfilesAndResiduals <- loadPlotTimeProfilesAndResidualsTask(self)
          self$plotDemography <- loadPlotDemographyTask(self)
          self$plotPKParameters <- loadPlotPKParametersTask(self)
          self$plotSensitivity <- loadPlotSensitivityTask(self)

          self$taskNames <- enum(self$getAllTasks())
        },
        error = function(e) {
          logErrorThenStop(e)
        }
      )
    },

    #' @description
    #' Run population workflow tasks for all simulation sets if tasks are activated
    #' The order of tasks is as follows:
    #' # 1) Run simulations
    #' # 2) Perform PK and sensitivity analyses
    #' # 3) Perform plot tasks
    #' ## 3.a) time profiles and residual plots
    #' ## 3.b) demography plots
    #' ## 3.c) PK and sensitivity analyses tables and plots
    #' # 4) Render report
    #' @return All results and plots as a structured output in the workflow folder
    runWorkflow = function() {
      # Prevent crashes if folder was deleted before (re) running a worflow
      dir.create(self$workflowFolder, showWarnings = FALSE, recursive = TRUE)
      # In case other logs were saved before running workflow
      setLogFolder(self$workflowFolder)
      actionToken1 <- re.tStartMetadataCapture(metaDataCapture = TRUE)
      actionToken2 <- re.tStartAction(actionType = "Run")
      logInfo(messages$runStarting("Population Workflow", self$workflowType))
      t0 <- tic()

      tryCatch(
        {
          if (self$simulate$active) {
            self$simulate$runTask(self$simulationStructures)
          }

          if (self$calculatePKParameters$active) {
            self$calculatePKParameters$runTask(self$simulationStructures)
          }

          if (self$calculateSensitivity$active) {
            self$calculateSensitivity$runTask(self$simulationStructures)
          }

          for (plotTask in self$getAllPlotTasks()) {
            if (self[[plotTask]]$active) {
              self[[plotTask]]$runTask(self$simulationStructures)
            }
          }

          for (userDefinedTask in self$userDefinedTasks) {
            if (userDefinedTask$active) {
              userDefinedTask$runTask(self$simulationStructures)
            }
          }

          # Merge appendices into final report
          appendices <- c(
            as.character(sapply(self$getAllPlotTasks(), function(taskName) {
              self[[taskName]]$fileName
            })),
            as.character(sapply(self$userDefinedTasks, function(userDefinedTask) {
              userDefinedTask$fileName
            }))
          )
          appendices <- appendices[file.exists(appendices)]
          if (length(appendices) > 0) {
            initialReportPath <- file.path(self$workflowFolder, self$reportFileName)
            mergeMarkdownFiles(appendices, initialReportPath)
            renderReport(
              file.path(self$workflowFolder, self$reportFileName),
              createWordReport = self$createWordReport,
              numberSections = self$numberSections,
              wordConversionTemplate = self$wordConversionTemplate
            )
            # Move report if a non-default path is provided
            copyReport(from = initialReportPath, to = self$reportFilePath, copyWordReport = self$createWordReport, keep = TRUE)
          }

          re.tStoreFileMetadata(access = "write", filePath = file.path(self$workflowFolder, defaultFileNames$logInfoFile()))
          re.tStoreFileMetadata(access = "write", filePath = file.path(self$workflowFolder, defaultFileNames$logDebugFile()))
          if (file.exists(file.path(self$workflowFolder, defaultFileNames$logErrorFile()))) {
            re.tStoreFileMetadata(access = "write", filePath = file.path(self$workflowFolder, defaultFileNames$logErrorFile()))
          }

          re.tEndAction(actionToken = actionToken2)
          re.tEndMetadataCapture(outputFolder = "./", actionToken = actionToken1)
        },
        error = function(e) {
          logErrorThenStop(e)
        }
      )
      logInfo(messages$runCompleted(getElapsedTime(t0), "Population Workflow", self$workflowType))
    }
  )
)

#' @title PopulationWorkflowTypes
#' @description List of population workflow available types
#' @export
#' @family enum helpers
#' @examples
#'
#' # Lists available Population Workflow types
#' PopulationWorkflowTypes
#'
PopulationWorkflowTypes <- enum(c(
  "pediatric",
  "parallelComparison",
  "ratioComparison"
))
