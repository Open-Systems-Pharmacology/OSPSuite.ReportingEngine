#' @title GofPlotTask
#' @description  R6 class for GofPlotTask settings
#' @export
GofPlotTask <- R6::R6Class(
  "GofPlotTask",
  inherit = PlotTask,

  public = list(

    #' @description
    #' Save results from task run.
    #' @param set R6 class `SimulationStructure`
    #' @param taskResults list of results from task run.
    #' Results contains at least 2 fields: `plots` and `tables`
    saveResults = function(set,
                               taskResults) {
      addTextChunk(
        self$fileName,
        paste0("## ", self$title, " for ", set$simulationSet$simulationSetName),
        logFolder = self$workflowFolder
      )

      for (timeRange in c("totalRange", "firstApplicationRange", "lastApplicationRange")) {
        # Captions only for multi admin
        if (length(taskResults$plots) > 1) {
          addTextChunk(
            self$fileName,
            getTimeRangeCaption(timeRange),
            logFolder = self$workflowFolder
          )
        }
        listOfPlots <- taskResults$plots[[timeRange]]
        listOfPlotCaptions <- taskResults$captions[[timeRange]]

        if (!isOfLength(listOfPlots, 0)) {
          for (plotName in names(listOfPlots)) {
            plotFileName <- getDefaultFileName(set$simulationSet$simulationSetName,
              suffix = paste0(plotName, "-", timeRange),
              extension = ExportPlotConfiguration$format
            )

            ggplot2::ggsave(
              filename = self$getAbsolutePath(plotFileName),
              plot = listOfPlots[[plotName]],
              width = ExportPlotConfiguration$width, height = ExportPlotConfiguration$height, units = ExportPlotConfiguration$units
            )

            re.tStoreFileMetadata(access = "write", filePath = self$getAbsolutePath(plotFileName))

            logWorkflow(
              message = paste0("Plot '", self$getRelativePath(plotFileName), "' was successfully saved."),
              pathFolder = self$workflowFolder,
              logTypes = LogTypes$Debug
            )

            if (!isOfLength(listOfPlotCaptions[[plotName]], 0)) {
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
      }

      for (tableName in names(taskResults$tables)) {
        tableFileName <- getDefaultFileName(set$simulationSet$simulationSetName,
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
    #' Run task and save its output
    #' @param structureSets list of `SimulationStructure` R6 class
    #' @param self$fileName name of report file
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
      if (isOfLength(referencePopulationIndex, 1)) {
        referenceSet <- structureSets[referencePopulationIndex]
        structureSets <- c(
          referenceSet,
          structureSets[setdiff(seq_along(structureSets), referencePopulationIndex)]
        )
      }

      for (set in structureSets) {
        logWorkflow(
          message = paste0(self$message, " for ", set$simulationSet$simulationName),
          pathFolder = self$workflowFolder
        )
        if (self$validateStructureSetInput(set)) {
          taskResults <- self$getTaskResults(
            set,
            self$workflowFolder,
            self$settings
          )
          # If first simulation set was a reference population,
          # its data will be added to next plots through settings
          if (isTRUE(set$simulationSet$referencePopulation)) {
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
          extension = ExportPlotConfiguration$format,
          sep = ""
        )

        qqPlotFileName <- getDefaultFileName(
          suffix = "residuals-qqplot",
          extension = ExportPlotConfiguration$format,
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
          width = ExportPlotConfiguration$width, height = ExportPlotConfiguration$height, units = ExportPlotConfiguration$units
        )
        re.tStoreFileMetadata(access = "write", filePath = residualHistogramPlotFileName)

        ggplot2::ggsave(
          filename = self$getAbsolutePath(qqPlotFileName),
          plot = residualQQPlot,
          width = ExportPlotConfiguration$width, height = ExportPlotConfiguration$height, units = ExportPlotConfiguration$units
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

        simulationNames <- paste0(as.character(sapply(structureSets, function(set) {
          set$simulationSet$simulationSetName
        })), collapse = ", ")
        addTextChunk(self$fileName, paste0("Figure: Distribution of residuals for ", simulationNames), logFolder = self$workflowFolder)

        addFigureChunk(
          fileName = self$fileName,
          figureFileRelativePath = self$getRelativePath(histogramFileName),
          figureFileRootDirectory = self$workflowFolder,
          logFolder = self$workflowFolder
        )

        addTextChunk(self$fileName, paste0("Figure: Residuals for ", simulationNames, " as quantile-quantile plot."), logFolder = self$workflowFolder)

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
