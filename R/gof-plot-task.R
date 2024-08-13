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
        text = paste(
          "##", self$title, "for", simulationSetName,
          anchor(paste0(self$reference, "-", removeForbiddenLetters(simulationSetName)))
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
        text = paste(
          "## Residuals across all simulations",
          anchor(paste0(self$reference, "-residuals-across-all-simulations"))
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
        text = paste("#", self$title, anchor(self$reference))
      )
      if (!is.null(self$outputFolder)) {
        dir.create(file.path(self$workflowFolder, self$outputFolder), showWarnings = FALSE)
      }

      # Goodness of fit task creates a histogram of residuals merged across the simulations
      residualsAcrossAllSimulations <- NULL
      residualsMetaData <- NULL
      simulationColors <- tlf::ColorMaps$ospDefault

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
          # its simulated and observed data are added for the next plots through settings
          # the option plotReferenceObsData from the simulation set will take care of the actual inclusion within the plots
          if (all(isTRUE(set$simulationSet$referencePopulation), isTRUE(self$settings$includeReferenceData))) {
            self$settings$referenceData <- taskResults$referenceData
          }

          # Specific to goodness of fit task:
          # taskResults include "residuals" and "metaData" fields, residuals from all simulations are plotted
          if (isEmpty(taskResults[["residuals"]])) {
            next
          }
          residualsInSimulation <- taskResults$residuals
          metaDataInSimulation <- taskResults$metaData
          # Update residuals data for residuals across all simulations
          simulationSetLegend <- reportSimulationSet(
            simulationSetName = set$simulationSet$simulationSetName,
            descriptor = set$simulationSetDescriptor
          )
          residualsInSimulation$Legend <- simulationSetLegend
          # Provide new set of colors to split between simulation sets
          colorIndex <- 1 + (length(unique(residualsAcrossAllSimulations$Legend)) %% length(simulationColors))
          metaDataInSimulation$residualsLegend <- simulationSetLegend
          metaDataInSimulation$color <- simulationColors[colorIndex]
          metaDataInSimulation$fill <- simulationColors[colorIndex]

          residualsAcrossAllSimulations <- rbind.data.frame(
            residualsAcrossAllSimulations,
            residualsInSimulation
          )
          residualsMetaData <- rbind.data.frame(
            residualsMetaData,
            metaDataInSimulation
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
    #' @param metaData A data.frame containing relevant information about the residuals
    #' @param settings List of settings such as `PlotConfiguration` R6 class objects for each goodness of fit plot
    #' @return A list of `TaskResults` objects
    getResidualsResults = function(structureSets, data, metaData, settings) {
      residualsResults <- list()
      # Update settings to include the simulation set names and descriptor
      simulationSetNames <- sapply(
        structureSets,
        function(set) {
          set$simulationSet$simulationSetName
        }
      )
      residualsSettings <- list(
        simulationSetNames = simulationSetNames,
        simulationSetDescriptor = structureSets[[1]]$simulationSetDescriptor,
        timeUnit = structureSets[[1]]$simulationSet$timeUnit
      )

      # Save residuals as a task result to save it as a csv file
      resultID <- defaultFileNames$resultID("residuals-across-all-simulations-data")
      residualsResults[[resultID]] <- saveTaskResults(
        id = resultID,
        table = data,
        includeTable = FALSE
      )

      # Leverage getResidualsPlotResultsInGroup
      outputGroups <- getOutputGroups(metaData)
      outputId <- 1

      for (outputGroup in outputGroups) {
        residualsSettings$groupID <- utils::head(outputGroup$displayName, 1)
        residualsPlotResults <- getResidualsPlotResultsInGroup(
          data = data,
          metaData = outputGroup,
          outputId = outputId,
          structureSet = NULL,
          settings = residualsSettings
        )
        outputId <- outputId + 1
        if (isEmpty(residualsPlotResults$plots)) {
          next
        }
        for (plotID in names(residualsPlotResults$plots)) {
          resultID <- defaultFileNames$resultID(
            "residuals-across-all-simulations",
            length(residualsResults),
            plotID
          )
          residualsResults[[resultID]] <- saveTaskResults(
            id = resultID,
            plot = residualsPlotResults$plots[[plotID]],
            plotCaption = residualsPlotResults$captions[[plotID]]
          )
        }
      }
      return(residualsResults)
    }
  )
)
