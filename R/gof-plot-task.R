#' @title GofPlotTask
#' @description  R6 class for GofPlotTask settings
#' @export
#' @family workflow tasks
GofPlotTask <- R6::R6Class(
  "GofPlotTask",
  inherit = PlotTask,
  public = list(
    #' @description
    #' Save the task results related to a `structureSet`.
    #' @param structureSet A `SimulationStructure` object defining the properties of a simulation set
    #' @param taskResults list of `TaskResults` objects
    saveResults = function(structureSet, taskResults) {
      simulationSetName <- structureSet$simulationSet$simulationSetName
      addTextChunk(
        fileName = self$fileName,
        text = c(
          anchor(paste0(self$reference, "-", removeForbiddenLetters(simulationSetName))), "",
          paste0("## ", self$title, " for ", simulationSetName)
        )
      )
      for (result in taskResults) {
        # Get both absolute and relative paths for figures and tables
        # Absolute path is required to always find the figure/table
        # Relative path is required by the final md report
        plotFileName <- getDefaultFileName(
          removeForbiddenLetters(simulationSetName),
          suffix = result$id,
          extension = reEnv$defaultPlotFormat$format
        )
        figureFilePath <- self$getAbsolutePath(plotFileName)
        figureFileRelativePath <- self$getRelativePath(plotFileName)

        tableFileName <- getDefaultFileName(
          removeForbiddenLetters(simulationSetName),
          suffix = result$id,
          extension = "csv"
        )
        tableFilePath <- self$getAbsolutePath(tableFileName)
        tableFileRelativePath <- self$getRelativePath(tableFileName)

        # Provide error message indicating which file could not be saved
        tryCatch(
          {
            result$saveFigure(fileName = figureFilePath)
          },
          error = function(e) {
            stop(messages$ggsaveError(figureFilePath, simulationSetName, e))
          }
        )
        result$addFigureToReport(
          reportFile = self$fileName,
          fileRelativePath = figureFileRelativePath,
          fileRootDirectory = self$workflowFolder
        )

        result$saveTable(fileName = tableFilePath)
        result$addTableToReport(
          reportFile = self$fileName,
          fileRelativePath = tableFileRelativePath,
          fileRootDirectory = self$workflowFolder,
          digits = self$settings$digits,
          scientific = self$settings$scientific
        )

        result$addTextChunkToReport(reportFile = self$fileName)
      }
      return(invisible())
    },

    #' @description
    #' Save the task results related to residuals across all simulations
    #' @param taskResults list of `TaskResults` objects
    saveResidualsResults = function(taskResults) {
      addTextChunk(
        fileName = self$fileName,
        text = c(
          anchor(paste0(self$reference, "-residuals-across-all-simulations")), "",
          "## Residuals across all simulations"
        )
      )
      for (result in taskResults) {
        # Get both absolute and relative paths for figures and tables
        # Absolute path is required to always find the figure/table
        # Relative path is required by the final md report
        plotFileName <- getDefaultFileName(
          suffix = result$id,
          extension = reEnv$defaultPlotFormat$format
        )
        figureFilePath <- self$getAbsolutePath(plotFileName)
        figureFileRelativePath <- self$getRelativePath(plotFileName)

        tableFileName <- getDefaultFileName(
          suffix = result$id,
          extension = "csv"
        )
        tableFilePath <- self$getAbsolutePath(tableFileName)
        tableFileRelativePath <- self$getRelativePath(tableFileName)

        # Provide error message indicating which file could not be saved
        tryCatch(
          {
            result$saveFigure(fileName = figureFilePath)
          },
          error = function(e) {
            stop(messages$ggsaveError(figureFilePath, simulationSetName, e))
          }
        )
        result$addFigureToReport(
          reportFile = self$fileName,
          fileRelativePath = figureFileRelativePath,
          fileRootDirectory = self$workflowFolder
        )

        result$saveTable(fileName = tableFilePath)
        result$addTableToReport(
          reportFile = self$fileName,
          fileRelativePath = tableFileRelativePath,
          fileRootDirectory = self$workflowFolder,
          digits = self$settings$digits,
          scientific = self$settings$scientific
        )

        result$addTextChunkToReport(reportFile = self$fileName)
      }
      return(invisible())
    },

    #' @description
    #' Run task and save its output results
    #' @param structureSets list of `SimulationStructure` objects
    runTask = function(structureSets) {
      actionToken <- re.tStartAction(actionType = "TLFGeneration", actionNameExtension = self$nameTaskResults)
      logInfo(messages$runStarting(self$message))
      t0 <- tic()
      resetReport(self$fileName)
      addTextChunk(
        fileName = self$fileName,
        text = c(anchor(self$reference), "", paste0("# ", self$title))
      )
      if (!is.null(self$outputFolder)) {
        dir.create(file.path(self$workflowFolder, self$outputFolder), showWarnings = FALSE)
      }

      # Goodness of fit task creates a histogram of residuals merged across the simulations
      residualsAcrossAllSimulations <- NULL

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
        logInfo(messages$runStarting(self$message, set$simulationSet$simulationSetName))
        if (self$validateStructureSetInput(set)) {
          taskResults <- self$getTaskResults(
            set,
            self$settings
          )
          self$saveResults(set, taskResults$results)

          # If first simulation set was a reference population,
          # its simulated, observed and lloq data are added for the next plots through settings
          # the option plotReferenceObsData from the simulation set will take care of the actual inclusion within the plots
          if (all(isTRUE(set$simulationSet$referencePopulation), isTRUE(self$settings$includeReferenceData))) {
            self$settings$referenceData <- taskResults$referenceData
          }

          # Specific to goodness of fit task:
          # taskResults include "residuals" field, residuals from all simulations are plotted
          if (isEmpty(taskResults[["residuals"]])) {
            next
          }
          residualsInSimulation <- taskResults$residuals$data
          residualsMetaData <- taskResults$residuals$metaData
          residualsInSimulation$Legend <- set$simulationSet$simulationSetName
          residualsAcrossAllSimulations <- rbind.data.frame(
            residualsAcrossAllSimulations,
            residualsInSimulation
          )
        }
      }

      if (isEmpty(residualsAcrossAllSimulations)) {
        re.tEndAction(actionToken = actionToken)
        logInfo(messages$runCompleted(getElapsedTime(t0), self$message))
        return(invisible())
      }

      residualsResults <- self$getResidualsResults(
        structureSets = structureSets,
        data = residualsAcrossAllSimulations,
        metaData = residualsMetaData,
        settings = self$settings
      )
      self$saveResidualsResults(residualsResults)
      re.tEndAction(actionToken = actionToken)
      logInfo(messages$runCompleted(getElapsedTime(t0), self$message))
      return(invisible())
    },

    #' @description
    #' Get plot results for residuals across all simulations
    #' @param structureSets A list of `SimulationStructure` objects defining the properties of a simulation set
    #' @param data data.frame
    #' @param metaData meta data on `data`
    #' @param settings List of settings such as `PlotConfiguration` R6 class objects for each goodness of fit plot
    #' @return A list of `TaskResults` objects
    getResidualsResults = function(structureSets, data, metaData, settings) {
      residualsResults <- list()
      simulationSetNames <- sapply(structureSets, function(set) {
        set$simulationSet$simulationSetName
      })
      simulationSetDescriptor <- structureSets[[1]]$simulationSetDescriptor

      # Save residuals as a task result to save it as a csv file
      resultID <- defaultFileNames$resultID(
        "residuals-across-all-simulations-data"
      )
      residualsResults[[resultID]] <- saveTaskResults(
        id = resultID,
        table = data,
        includeTable = FALSE
      )

      # Plot the residuals across the simulations
      resultID <- defaultFileNames$resultID(
        "residuals-across-all-simulations-histogram"
      )

      residualsHistogramPlot <- tlf::plotHistogram(
        data = data,
        metaData = metaData,
        dataMapping = tlf::HistogramDataMapping$new(
          x = "Residuals",
          fill = "Legend",
          stack = TRUE,
          distribution = "normal"
        ),
        plotConfiguration = settings$plotConfigurations[["histogram"]],
        bins = settings$bins %||% reEnv$defaultBins
      )
      residualsHistogramPlot <- tlf::setPlotLabels(
        residualsHistogramPlot,
        ylabel = reEnv$residualsHistogramLabel
      )

      residualsResults[[resultID]] <- saveTaskResults(
        id = resultID,
        plot = residualsHistogramPlot,
        plotCaption = captions$plotGoF$histogram(simulationSetNames, simulationSetDescriptor)
      )

      resultID <- defaultFileNames$resultID(
        "residuals-across-all-simulations-qq-plot"
      )

      residualsQQPlot <- tlf::plotQQ(
        data = data,
        metaData = metaData,
        dataMapping = tlf::QQDataMapping$new(
          y = "Residuals",
          group = "Legend"
        ),
        plotConfiguration = settings$plotConfigurations[["qqPlot"]]
      )
      residualsQQPlot <- tlf::setPlotLabels(
        residualsQQPlot,
        ylabel = reEnv$residualsQQLabel
      )

      residualsResults[[resultID]] <- saveTaskResults(
        id = resultID,
        plot = residualsQQPlot,
        plotCaption = captions$plotGoF$qqPlot(simulationSetNames, simulationSetDescriptor)
      )

      return(residualsResults)
    }
  )
)
