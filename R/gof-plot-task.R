#' @title GofPlotTask
#' @description  R6 class for GofPlotTask settings
#' @export
GofPlotTask <- R6::R6Class(
  "GofPlotTask",
  inherit = PlotTask,
  public = list(
    #' @description
    #' Save the task results related to a `structureSet`.
    #' @param structureSet A `SimulationStructure` object defining the properties of a simulation set
    #' @param taskResults list of results from task run.
    #' Currently, results contains at least 2 fields: `plots` and `tables`
    #' They are to be deprecated and replaced using `TaskResults` objects
    saveResults = function(structureSet, taskResults) {
      addTextChunk(
        self$fileName,
        paste0("## ", self$title, " for ", structureSet$simulationSet$simulationSetName),
        logFolder = self$workflowFolder
      )
      # For mutliple applications, taskResults$plots has 3 fields named as ApplicationRanges
      # Sub sections are created if more than one field are kept
      hasMultipleApplications <- (length(taskResults$plots) > 1)

      for (timeRange in ApplicationRanges) {
        listOfPlots <- taskResults$plots[[timeRange]]
        listOfPlotCaptions <- taskResults$captions[[timeRange]]

        if (ospsuite.utils::isOfLength(listOfPlots, 0)) {
          next
        }
        if (hasMultipleApplications) {
          addTextChunk(self$fileName, getTimeRangeCaption(timeRange), logFolder = self$workflowFolder)
        }
        # Save and include plot paths to report
        for (plotName in names(listOfPlots)) {
          plotFileName <- getDefaultFileName(structureSet$simulationSet$simulationSetName,
            suffix = paste0(plotName, "-", timeRange),
            extension = reEnv$defaultPlotFormat$format
          )

          ggplot2::ggsave(
            filename = self$getAbsolutePath(plotFileName),
            plot = listOfPlots[[plotName]],
            width = reEnv$defaultPlotFormat$width,
            height = reEnv$defaultPlotFormat$height,
            units = reEnv$defaultPlotFormat$units,
            dpi = reEnv$defaultPlotFormat$dpi
          )

          re.tStoreFileMetadata(access = "write", filePath = self$getAbsolutePath(plotFileName))

          logWorkflow(
            message = paste0("Plot '", self$getRelativePath(plotFileName), "' was successfully saved."),
            pathFolder = self$workflowFolder,
            logTypes = LogTypes$Debug
          )

          if (!ospsuite.utils::isOfLength(listOfPlotCaptions[[plotName]], 0)) {
            addTextChunk(self$fileName, paste0("Figure: ", listOfPlotCaptions[[plotName]]), logFolder = self$workflowFolder)
          }

          addFigureChunk(
            fileName = self$fileName,
            figureFileRelativePath = self$getRelativePath(plotFileName),
            figureFileRootDirectory = self$workflowFolder,
            logFolder = self$workflowFolder
          )
        }
      }

      for (tableName in names(taskResults$tables)) {
        tableFileName <- getDefaultFileName(structureSet$simulationSet$simulationSetName,
          suffix = tableName,
          extension = "csv"
        )

        write.csv(taskResults$tables[[tableName]],
          file = self$getAbsolutePath(tableFileName),
          row.names = FALSE,
          fileEncoding = "UTF-8"
        )

        re.tStoreFileMetadata(access = "write", filePath = self$getAbsolutePath(tableFileName))

        logWorkflow(
          message = paste0("Table '", self$getAbsolutePath(tableFileName), "' was successfully saved."),
          pathFolder = self$workflowFolder,
          logTypes = LogTypes$Debug
        )
      }
    },

    #' @description
    #' Run task and save its output results
    #' @param structureSets list of `SimulationStructure` objects
    runTask = function(structureSets) {
      actionToken <- re.tStartAction(actionType = "TLFGeneration", actionNameExtension = self$nameTaskResults)
      logWorkflow(
        message = paste0("Starting: ", self$message),
        pathFolder = self$workflowFolder
      )
      resetReport(self$fileName, self$workflowFolder)
      addTextChunk(
        self$fileName,
        paste0("# ", self$title),
        logFolder = self$workflowFolder
      )
      if (!is.null(self$outputFolder)) {
        dir.create(file.path(self$workflowFolder, self$outputFolder), showWarnings = FALSE)
      }

      # Goodness of fit task creates a histogram of residuals merged accross the simulaiton
      residualsAcrossAllSimulations <- NULL
      taskResults <- list()

      # Check if a reference population is defined and get it as first set to be run
      referencePopulationIndex <- which(sapply(structureSets, function(structureSet) {
        isTRUE(structureSet$simulationSet$referencePopulation)
      }))
      if (ospsuite.utils::isOfLength(referencePopulationIndex, 1)) {
        referenceSet <- structureSets[referencePopulationIndex]
        structureSets <- c(
          referenceSet,
          structureSets[setdiff(seq_along(structureSets), referencePopulationIndex)]
        )
      }

      for (set in structureSets) {
        logWorkflow(
          message = paste0(self$message, " for ", set$simulationSet$simulationSetName),
          pathFolder = self$workflowFolder
        )
        if (self$validateStructureSetInput(set)) {
          taskResults <- self$getTaskResults(
            set,
            self$workflowFolder,
            self$settings
          )
          # If first simulation set was a reference population,
          # its simulated, observed and lloq data are added for the next plots through settings
          # the option plotReferenceObsData from the simulation set will take care of the actual inclusion within the plots
          if (all(isTRUE(set$simulationSet$referencePopulation), isTRUE(self$settings$includeReferenceData))) {
            self$settings$referenceData <- taskResults$referenceData
          }

          # Specific to goodness of fit task:
          # taskResults include `residuals`
          if (!is.null(taskResults[["residuals"]])) {
            residualsInSimulation <- taskResults$residuals$data
            residualsInSimulation$Legend <- set$simulationSet$simulationSetName
            residualsAcrossAllSimulations <- rbind.data.frame(
              residualsAcrossAllSimulations,
              residualsInSimulation
            )
          }
          self$saveResults(set, taskResults)
        }
      }

      if (!is.null(residualsAcrossAllSimulations)) {
        histogramFileName <- getDefaultFileName(
          suffix = "residuals-histogram",
          extension = reEnv$defaultPlotFormat$format,
          sep = ""
        )
        qqPlotFileName <- getDefaultFileName(
          suffix = "residuals-qqplot",
          extension = reEnv$defaultPlotFormat$format,
          sep = ""
        )
        tableFileName <- getDefaultFileName(
          suffix = "residuals",
          extension = "csv",
          sep = ""
        )

        write.csv(residualsAcrossAllSimulations,
          file = self$getAbsolutePath(tableFileName),
          row.names = FALSE
        )
        re.tStoreFileMetadata(access = "write", filePath = self$getAbsolutePath(tableFileName))
        logWorkflow(
          message = paste0("Table '", self$getAbsolutePath(tableFileName), "' was successfully saved."),
          pathFolder = self$workflowFolder,
          logTypes = LogTypes$Debug
        )

        residualHistogramPlot <- plotResidualsHistogram(
          data = residualsAcrossAllSimulations,
          metaData = taskResults$residuals$metaData,
          plotConfiguration = self$settings$plotConfigurations[["histogram"]],
          bins = self$settings$bins
        )

        residualQQPlot <- plotResidualsQQPlot(
          data = residualsAcrossAllSimulations,
          metaData = taskResults$residuals$metaData,
          plotConfiguration = self$settings$plotConfigurations[["qqPlot"]]
        )

        residualHistogramPlotFileName <- self$getAbsolutePath(histogramFileName)
        ggplot2::ggsave(
          filename = residualHistogramPlotFileName,
          plot = residualHistogramPlot,
          width = reEnv$defaultPlotFormat$width,
          height = reEnv$defaultPlotFormat$height,
          units = reEnv$defaultPlotFormat$units,
          dpi = reEnv$defaultPlotFormat$dpi
        )
        re.tStoreFileMetadata(access = "write", filePath = residualHistogramPlotFileName)

        ggplot2::ggsave(
          filename = self$getAbsolutePath(qqPlotFileName),
          plot = residualQQPlot,
          width = reEnv$defaultPlotFormat$width,
          height = reEnv$defaultPlotFormat$height,
          units = reEnv$defaultPlotFormat$units,
          dpi = reEnv$defaultPlotFormat$dpi
        )
        re.tStoreFileMetadata(access = "write", filePath = self$getAbsolutePath(qqPlotFileName))

        logWorkflow(
          message = paste0("Plots '", self$getRelativePath(histogramFileName), "', '", self$getRelativePath(qqPlotFileName), "' were successfully saved."),
          pathFolder = self$workflowFolder,
          logTypes = LogTypes$Debug
        )

        addTextChunk(
          self$fileName,
          "## Residuals across all simulations",
          logFolder = self$workflowFolder
        )

        simulationSetNames <- as.character(sapply(structureSets, function(set) {
          set$simulationSet$simulationSetName
        }))
        histogramCaption <- captions$plotGoF$histogram(simulationSetNames, structureSets[[1]]$simulationSetDescriptor)
        addTextChunk(self$fileName, paste0("Figure: ", histogramCaption), logFolder = self$workflowFolder)

        addFigureChunk(
          fileName = self$fileName,
          figureFileRelativePath = self$getRelativePath(histogramFileName),
          figureFileRootDirectory = self$workflowFolder,
          logFolder = self$workflowFolder
        )

        qqPlotCaption <- captions$plotGoF$qqPlot(simulationSetNames, structureSets[[1]]$simulationSetDescriptor)
        addTextChunk(self$fileName, paste0("Figure: ", qqPlotCaption), logFolder = self$workflowFolder)

        addFigureChunk(
          fileName = self$fileName,
          figureFileRelativePath = self$getRelativePath(qqPlotFileName),
          figureFileRootDirectory = self$workflowFolder,
          logFolder = self$workflowFolder
        )
      }
      re.tEndAction(actionToken = actionToken)
    }
  )
)
