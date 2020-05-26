#' @title PopulationWorkflow
#' @description R6 class for Reporting Engine Population Workflow
#' @field workflowType Type of population workflow
#' @field simulatePopulation R6 class `Task` for population simulation
#' @field populationPKParameters R6 class `Task` for PK parameters calculation
#' @field populationSensitivityAnalysis R6 class `Task` for sensitivity analysis
#' @field plotDemography R6 class `Task` for demography plots
#' @field plotGoF R6 class `Task` for goodness of fit plots
#' @field plotPKParameters R6 class `Task` for PK parameters plot
#' @field plotSensitivity R6 class `Task` for sensitivity plot
#' @export
#' @import tlf
#' @import ospsuite
#' @format NULL
PopulationWorkflow <- R6::R6Class(
  "PopulationWorkflow",
  inherit = Workflow,

  public = list(
    workflowType = NULL,
    simulatePopulation = NULL, # TO DO: rename with simpler task name simulate
    populationPKParameters = NULL, # TO DO: rename with simpler task name calculatePKParameters
    populationSensitivityAnalysis = NULL,
    plotDemography = NULL,
    plotGoF = NULL,
    plotPKParameters = NULL,
    plotSensitivity = NULL,

    #' @description
    #' Create a new `PopulationWorkflow` object.
    #' @param workflowType Type of population workflow. Use enum `PopulationWorkflowTypes` to get list of workflow types.
    #' @param simulationSets list of `SimulationSet` R6 class objects
    #' @param workflowFolder path of the output folder created or used by the Workflow.
    #' @return A new `PopulationWorkflow` object
    initialize = function(workflowType = PopulationWorkflowTypes$parallelComparison,
                              simulationSets,
                              workflowFolder) {
      super$initialize(
        simulationSets = simulationSets,
        workflowFolder = workflowFolder
      )

      validateIsOfType(c(simulationSets), "PopulationSimulationSet")
      if (!isOfType(simulationSets, "list")) {
        simulationSets <- list(simulationSets)
      }

      validateIsIncluded(workflowType, PopulationWorkflowTypes)
      self$workflowType <- workflowType

      # Pediatric and ratio comparison workflows need ONE reference population
      if (isIncluded(self$workflowType, c(PopulationWorkflowTypes$pediatric, PopulationWorkflowTypes$ratioComparison))) {
        allSimulationReferences <- sapply(simulationSets, function(set) {
          set$referencePopulation
        })
        validateIsOfLength(allSimulationReferences[allSimulationReferences], 1)
      }

      # TO DO: include task parameters from initialization ?
      self$simulatePopulationSettings()
      self$populationPKParameterSettings()
      self$populationSensitivityAnalysisSettings()
      self$plotDemographySettings()
      self$plotGoFSettings()
      self$plotPKParametersSettings()
      self$plotSensitivitySettings()
      self$taskNames <- enum(self$getAllTasks())
    },

    #' @description
    #' Define simulate `task` settings
    #' @param taskFunction function performed by `Task` to get results
    #' @param outputFolder folder where `Task` output is saved
    #' @param active logical defining if `Task` will be run by workflow
    #' Default value is `TRUE`
    #' @param settings specific settings for `Task`
    #' @param message message/title of the `Task`
    #' @return A new `Task` object
    simulatePopulationSettings = function(taskFunction = simulateModelForPopulation,
                                              outputFolder = defaultTaskOutputFolders$simulate,
                                              settings = NULL,
                                              active = TRUE,
                                              message = defaultWorkflowMessages$simulate) {
      self$simulatePopulation <- SimulationTask$new(
        getTaskResults = taskFunction,
        outputFolder = outputFolder,
        workflowFolder = self$workflowFolder,
        active = active,
        settings = settings,
        message = message
      )
    },

    #' @description
    #' Define calculate PK parameters `task` settings
    #' @param taskFunction function performed by `Task` to get results
    #' @param outputFolder folder where `Task` output is saved
    #' @param active logical defining if `Task` will be run by workflow
    #' Default value is `FALSE`
    #' @param settings specific settings for `Task`
    #' @param message message/title of the `Task`
    #' @return A new `Task` object
    populationPKParameterSettings = function(taskFunction = calculatePKParameters,
                                                 outputFolder = defaultTaskOutputFolders$calculatePKParameters,
                                                 settings = NULL,
                                                 active = FALSE,
                                                 message = defaultWorkflowMessages$calculatePKParameters) {
      self$populationPKParameters <- CalculatePKParametersTask$new(
        getTaskResults = taskFunction,
        outputFolder = outputFolder,
        workflowFolder = self$workflowFolder,
        settings = settings,
        active = active,
        message = message %||% "Calculate PK parameters for population"
      )
    },

    #' @description
    #' Define population Sensitivity Analysis `task` settings
    #' @param taskFunction function performed by `Task` to get results
    #' @param outputFolder folder where `Task` output is saved
    #' @param active logical defining if `Task` will be run by workflow
    #' Default value is `FALSE`
    #' @param settings specific settings for `Task`
    #' @param message message/title of the `Task`
    #' @return A new `SensitivityAnalysisTask` object
    populationSensitivityAnalysisSettings = function(taskFunction = runPopulationSensitivityAnalysis,
                                                         outputFolder = defaultTaskOutputFolders$sensitivityAnalysis,
                                                         settings = NULL,
                                                         active = FALSE,
                                                         message = NULL) {
      self$populationSensitivityAnalysis <- PopulationSensitivityAnalysisTask$new(
        getTaskResults = taskFunction,
        outputFolder = outputFolder,
        workflowFolder = self$workflowFolder,
        settings = settings,
        active = active,
        message = message %||% "Sensitivity analysis for population"
      )
    },

    # TO DO: Define the tasks settings for plots
    #' @description
    #' Define plot demography `PlotTask` settings
    #' @param reportTitle section title of plot task result within report
    #' @param fileName name of report appendix file associated to task
    #' @param taskFunction function called by task to get the results as a list of `plots` and `tables`
    #' @param outputFolder folder where `Task` output is saved
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `FALSE`
    #' @param message message indicating what the `task` does
    #' @param xParameters list of parameters to be plotted along x axis
    #' @param yParameters list of parameters to be plotted along y axis
    #' @param settings specific settings for task
    #' @return A `PlotTask` object for goodness of fit plots
    plotDemographySettings = function(reportTitle = defaultWorkflowTitles$plotDemography,
                                          fileName = defaultWorkflowAppendices$plotDemography,
                                          taskFunction = plotDemographyParameters,
                                          outputFolder = defaultTaskOutputFolders$plotDemography,
                                          active = FALSE,
                                          message = defaultWorkflowMessages$plotDemography,
                                          xParameters = getDefaultDemographyXParameters(self$workflowType),
                                          yParameters = NULL,
                                          settings = NULL) {
      self$plotDemography <- PopulationPlotTask$new(
        workflowType = self$workflowType,
        xParameters = xParameters,
        yParameters = yParameters,
        reportTitle = reportTitle,
        fileName = fileName,
        getTaskResults = taskFunction,
        outputFolder = outputFolder,
        workflowFolder = self$workflowFolder,
        active = active,
        message = message,
        settings = settings
      )
    },

    #' @description
    #' Define Goodness of fit `PlotTask` settings
    #' @param reportTitle section title of plot task result within report
    #' @param fileName name of report appendix file associated to task
    #' @param taskFunction function called by task to get the results as a list of `plots` and `tables`
    #' @param outputFolder folder where `Task` output is saved
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `FALSE`
    #' @param message message indicating what the `task` does
    #' @param settings specific settings for task
    #' @return A `PlotTask` object for goodness of fit plots
    plotGoFSettings = function(reportTitle = defaultWorkflowTitles$plotGoF,
                                   fileName = defaultWorkflowAppendices$plotGoF,
                                   taskFunction = plotPopulationGoodnessOfFit,
                                   outputFolder = defaultTaskOutputFolders$plotGoF,
                                   active = FALSE,
                                   message = defaultWorkflowMessages$plotGoF,
                                   settings = NULL) {
      self$plotGoF <- PlotTask$new(
        reportTitle = reportTitle,
        fileName = fileName,
        getTaskResults = taskFunction,
        outputFolder = outputFolder,
        workflowFolder = self$workflowFolder,
        active = active,
        message = message,
        settings = settings
      )
    },

    #' @description
    #' Define PK parameters `PlotPKParametersTask` settings
    #' @param reportTitle section title of plot task result within report
    #' @param fileName name of report appendix file associated to task
    #' @param taskFunction function called by task to get the results as a list of `plots` and `tables`
    #' @param outputFolder folder where `Task` output is saved
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `FALSE`
    #' @param message message indicating what the `task` does
    #' @param xParameters list of parameters to be plotted along x axis
    #' @param yParameters list of parameters to be plotted along y axis
    #' @param settings specific settings for task
    #' @return A `PlotPKParametersTask` object for PK parameters tables
    plotPKParametersSettings = function(reportTitle = defaultWorkflowTitles$plotPKParameters,
                                            fileName = defaultWorkflowAppendices$plotPKParameters,
                                            taskFunction = plotPopulationPKParameters,
                                            outputFolder = defaultTaskOutputFolders$plotPKParameters,
                                            active = FALSE,
                                            message = defaultWorkflowMessages$plotPKParameters,
                                            xParameters = getDefaultPkParametersXParameters(self$workflowType),
                                            yParameters = NULL,
                                            settings = NULL) {
      self$plotPKParameters <- PopulationPlotTask$new(
        workflowType = self$workflowType,
        xParameters = xParameters,
        yParameters = yParameters,
        reportTitle = reportTitle,
        fileName = fileName,
        getTaskResults = taskFunction,
        outputFolder = outputFolder,
        workflowFolder = self$workflowFolder,
        active = active,
        message = message,
        settings = settings
      )
    },

    #' @description
    #' Define sensitivity analysis `PlotTask` settings
    #' @param reportTitle section title of plot task result within report
    #' @param fileName name of report appendix file associated to task
    #' @param taskFunction function called by task to get the results as a list of `plots` and `tables`
    #' @param outputFolder folder where `Task` output is saved
    #' @param active logical indicating if `task` is performed in worklfow.
    #' Default value is `FALSE`
    #' @param message message indicating what the `task` does
    #' @param xParameters list of parameters to be plotted along x axis - unused in this task
    #' @param yParameters list of parameters to be plotted along y axis - unused in this task
    #' @param settings specific settings for task
    #' @return A `PopulationPlotTask` object for sensitivity plots for a single population
    plotSensitivitySettings = function(reportTitle = defaultWorkflowTitles$plotSensitivity,
                                           fileName = defaultWorkflowAppendices$plotSensitivity,
                                           taskFunction = plotPopulationSensitivity,
                                           outputFolder = defaultTaskOutputFolders$plotSensitivity,
                                           active = FALSE,
                                           message = defaultWorkflowMessages$plotSensitivity,
                                           xParameters = NULL,
                                           yParameters = NULL,
                                           settings = NULL) {
      self$plotSensitivity <- PopulationPlotTask$new(
        workflowType = self$workflowType,
        xParameters = xParameters,
        yParameters = yParameters,
        reportTitle = reportTitle,
        fileName = fileName,
        getTaskResults = taskFunction,
        outputFolder = outputFolder,
        workflowFolder = self$workflowFolder,
        active = active,
        message = message,
        settings = settings %||% self$populationSensitivityAnalysis$settings %||% SensitivityPlotSettings$new()
      )
    },

    #' @description
    #' Loop through all simulation sets and run active population model workflow tasks for each.
    #' # POPULATION WORKFLOW
    #' # CORE STAGE 0:  DEMOGRAPHY SUMMARY
    #' # 0a - Demography Plots/Tables
    #' # CORE STAGE 1:  SIMULATION
    #' # 1a - Time profile Plot
    #' # CORE STAGE 2:  CALCULATE PK PARAMETER
    #' # 2a - PK parameter Plot
    #' # CORE STAGE 3:  CALCULATE SENSITIVITY
    #' # 3a - Plots and Tables based on sensitivity results
    runWorkflow = function() {
      logWorkflow(
        message = "Starting run of population workflow",
        pathFolder = self$workflowFolder
      )

      if (self$simulatePopulation$active) {
        self$simulatePopulation$runTask(self$simulationStructures)
      }

      if (self$populationPKParameters$active) {
        self$populationPKParameters$runTask(self$simulationStructures)
      }


      if (self$populationSensitivityAnalysis$active) {
        self$populationSensitivityAnalysis$runTask(self$simulationStructures)
      }

      for (plotTask in self$getAllPlotTasks()) {
        if (self[[plotTask]]$active) {
          self[[plotTask]]$runTask(self$simulationStructures)
        }
      }
      # Merge appendices into final report
      appendices <- as.character(sapply(self$getAllPlotTasks(), function(taskName) {
        self[[taskName]]$fileName
      }))
      appendices <- appendices[file.exists(appendices)]
      if (!is.null(appendices)) {
        mergeMarkdowndFiles(appendices, self$reportFileName, logFolder = self$workflowFolder)
        renderReport(self$reportFileName, logFolder = self$workflowFolder)
      }
    }
  )
)


#' @export
PopulationWorkflowTypes <- enum(c(
  "pediatric",
  "parallelComparison",
  "ratioComparison"
))
