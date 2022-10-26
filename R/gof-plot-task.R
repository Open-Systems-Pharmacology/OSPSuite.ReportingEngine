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
          residualsInSimulation <- taskResults$residuals
          # Update residuals data for residuals accross all simulations
          residualsInSimulation$Legend <- reportSimulationSet(
            simulationSetName = set$simulationSet$simulationSetName,
            descriptor = set$simulationSetDescriptor
          )

          residualsAcrossAllSimulations <- rbind.data.frame(
            residualsAcrossAllSimulations,
            residualsInSimulation
          )
        }
      }

      # Conditions where residuals across all simulations are not reported
      noResidualsAcrossAllSimulations <- any(
        isOfLength(structureSets, 1),
        isEmpty(residualsAcrossAllSimulations)
      )

      if (noResidualsAcrossAllSimulations) {
        re.tEndAction(actionToken = actionToken)
        logInfo(messages$runCompleted(getElapsedTime(t0), self$message))
        return(invisible())
      }

      residualsResults <- self$getResidualsResults(
        structureSets = structureSets,
        data = residualsAcrossAllSimulations,
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
    #' @param settings List of settings such as `PlotConfiguration` R6 class objects for each goodness of fit plot
    #' @return A list of `TaskResults` objects
    getResidualsResults = function(structureSets, data, settings) {
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

      # Get all unique output paths
      # TODO: group them by unique ID once introduced
      allOutputs <- sapply(
        structureSets,
        FUN = function(set) {
          set$simulationSet$outputs
        }
      )
      allOutputPaths <- sapply(
        allOutputs,
        FUN = function(output) {
          output$path
        }
      )
      allOutputs <- allOutputs[!duplicated(allOutputPaths)]

      # Plot the residuals across the simulations by output path
      for (output in allOutputs) {
        Legend <- "Residuals\nlog(Observed)-log(Simulated)"
        if (isIncluded(output$residualScale, ResidualScales$Logarithmic)) {
          Legend <- "Residuals\nObserved-Simulated"
        }
        residualsMetaData <- list(
          "Time" = list(dimension = "Time", unit = structureSets[[1]]$simulationSet$timeUnit),
          "Observed" = list(dimension = "Observed data", unit = output$displayUnit),
          "Simulated" = list(dimension = "Simulated value", unit = output$displayUnit),
          "Residuals" = list(unit = "", dimension = Legend)
        )
        
        outputData <- data[data$Path %in% output$path, ]
        if(isEmpty(outputData)){
          next
        }

        # Obs vs pred
        obsVsPredPlot <- tlf::plotObsVsPred(
          data = outputData,
          metaData = residualsMetaData,
          dataMapping = tlf::ObsVsPredDataMapping$new(
            x = "Observed",
            y = "Simulated",
            group = "Legend"
          ),
          plotConfiguration = settings$plotConfigurations[["obsVsPred"]]
        )
        obsVsPredPlot <- setQuadraticDimension(
          obsVsPredPlot, 
          plotConfiguration = settings$plotConfigurations[["obsVsPred"]]
        )

        resultID <- defaultFileNames$resultID(
          "residuals-across-all-simulations",
          length(residualsResults),
          "obsVsPred",
          output$path
        )
        residualsResults[[resultID]] <- saveTaskResults(
          id = resultID,
          plot = obsVsPredPlot,
          plotCaption = captions$plotGoF$obsVsPred(
            simulationSetName = simulationSetNames,
            descriptor = simulationSetDescriptor,
            pathName = output$displayName
          )
        )

        # Obs vs Pred log scale
        selectedLogData <- outputData$Simulated > 0 & outputData$Observed > 0
        if (sum(selectedLogData) > 0) {
          obsVsPredRange <- autoAxesLimits(c(
            data$Simulated[selectedLogData & data$Path %in% output$path],
            data$Observed[selectedLogData & data$Path %in% output$path]
          ),
          scale = "log"
          )
          obsVsPredBreaks <- autoAxesTicksFromLimits(obsVsPredRange)

          obsVsPredPlotLog <- tlf::plotObsVsPred(
            data = data[selectedLogData & data$Path %in% output$path, ],
            metaData = residualsMetaData,
            dataMapping = tlf::ObsVsPredDataMapping$new(
              x = "Observed",
              y = "Simulated",
              group = "Legend"
            ),
            plotConfiguration = settings$plotConfigurations[["obsVsPredLog"]]
          )
          obsVsPredPlotLog <- tlf::setXAxis(
            plotObject = obsVsPredPlotLog,
            scale = tlf::Scaling$log,
            limits = obsVsPredRange,
            ticks = obsVsPredBreaks
          )
          obsVsPredPlotLog <- tlf::setYAxis(
            plotObject = obsVsPredPlotLog,
            scale = tlf::Scaling$log,
            limits = obsVsPredRange,
            ticks = obsVsPredBreaks
          )
          obsVsPredPlotLog <- setQuadraticDimension(
            obsVsPredPlotLog, 
            plotConfiguration = settings$plotConfigurations[["obsVsPredLog"]]
          )

          resultID <- defaultFileNames$resultID(
            "residuals-across-all-simulations",
            length(residualsResults),
            "obsVsPredLog",
            output$path
          )
          residualsResults[[resultID]] <- saveTaskResults(
            id = resultID,
            plot = obsVsPredPlotLog,
            plotCaption = captions$plotGoF$obsVsPred(
              simulationSetName = simulationSetNames,
              descriptor = simulationSetDescriptor,
              plotScale = "logarithmic",
              pathName = output$displayName
            )
          )
        }

        # Res vs pred
        resVsPredPlot <- tlf::plotResVsPred(
          data = outputData,
          metaData = residualsMetaData,
          dataMapping = tlf::ResVsPredDataMapping$new(
            x = "Simulated",
            y = "Residuals",
            group = "Legend"
          ),
          plotConfiguration = settings$plotConfigurations[["resVsPred"]]
        )

        resultID <- defaultFileNames$resultID(
          "residuals-across-all-simulations",
          length(residualsResults),
          "res-vs-pred"
        )
        residualsResults[[resultID]] <- saveTaskResults(
          id = resultID,
          plot = resVsPredPlot,
          plotCaption = captions$plotGoF$resVsPred(
            simulationSetName = simulationSetNames,
            descriptor = simulationSetDescriptor,
            plotScale = output$residualScale,
            pathName = output$displayName
          )
        )

        # Res vs time
        residualTimeTicks <- getTimeTicksFromUnit(
          residualsMetaData$Time$unit,
          timeValues = data$Time
        )
        resVsTimePlot <- tlf::plotResVsTime(
          data = outputData,
          metaData = residualsMetaData,
          dataMapping = tlf::ResVsTimeDataMapping$new(
            x = "Time",
            y = "Residuals",
            group = "Legend"
          ),
          plotConfiguration = settings$plotConfigurations[["resVsTime"]]
        )
        resVsTimePlot <- tlf::setXAxis(
          plotObject = resVsTimePlot,
          ticks = residualTimeTicks$ticks,
          ticklabels = residualTimeTicks$ticklabels
        )

        resultID <- defaultFileNames$resultID(
          "residuals-across-all-simulations",
          length(residualsResults),
          "res-vs-time"
        )
        residualsResults[[resultID]] <- saveTaskResults(
          id = resultID,
          plot = resVsTimePlot,
          plotCaption = captions$plotGoF$resVsTime(
            simulationSetName = simulationSetNames,
            descriptor = simulationSetDescriptor,
            plotScale = output$residualScale,
            pathName = output$displayName
          )
        )

        # Histogram
        residualsHistogramPlot <- tlf::plotHistogram(
          data = outputData,
          metaData = residualsMetaData,
          dataMapping = tlf::HistogramDataMapping$new(
            x = "Residuals",
            fill = "Legend",
            stack = TRUE,
            distribution = "normal",
            frequency = TRUE
          ),
          plotConfiguration = settings$plotConfigurations[["histogram"]],
          bins = settings$bins %||% reEnv$defaultBins
        )
        residualsHistogramPlot <- tlf::setPlotLabels(
          residualsHistogramPlot,
          ylabel = reEnv$residualsHistogramLabel
        )

        resultID <- defaultFileNames$resultID(
          "residuals-across-all-simulations",
          length(residualsResults),
          "histogram"
        )
        residualsResults[[resultID]] <- saveTaskResults(
          id = resultID,
          plot = residualsHistogramPlot,
          plotCaption = captions$plotGoF$resHisto(
            simulationSetName = simulationSetNames,
            descriptor = simulationSetDescriptor,
            plotScale = output$residualScale,
            pathName = output$displayName
          )
        )

        # QQ Plot
        residualsQQPlot <- tlf::plotQQ(
          data = outputData,
          metaData = residualsMetaData,
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

        resultID <- defaultFileNames$resultID(
          "residuals-across-all-simulations",
          length(residualsResults),
          "qq-plot"
        )
        residualsResults[[resultID]] <- saveTaskResults(
          id = resultID,
          plot = residualsQQPlot,
          plotCaption = captions$plotGoF$resQQPlot(
            simulationSetName = simulationSetNames,
            descriptor = simulationSetDescriptor,
            plotScale = output$residualScale,
            pathName = output$displayName
          )
        )
      }

      return(residualsResults)
    }
  )
)
