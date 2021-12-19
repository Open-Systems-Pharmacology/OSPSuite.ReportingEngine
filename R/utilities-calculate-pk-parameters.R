#' @title calculatePKParameters
#' @description Calculate PK parameters from simulated time profiles
#' @param structureSet `SimulationStructure` R6 class object contain paths of files to be used
#' @param settings list of options to be passed on the function
#' @param logFolder folder where the logs are saved
#' @return pkAnalysis object
#' @import ospsuite
#' @keywords internal
calculatePKParameters <- function(structureSet,
                                  settings = NULL,
                                  logFolder = getwd()) {
  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$simulationFile)
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)

  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationResultFileNames)
  simulationResults <- ospsuite::importResultsFromCSV(
    simulation = simulation,
    filePaths = structureSet$simulationResultFileNames
  )

  logWorkflow(
    message = paste0("Simulation results '", structureSet$simulationResultFileNames, "' successfully loaded"),
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )

  pkAnalyses <- calculatePKAnalyses(results = simulationResults)
  logWorkflow(
    message = "Calculation of PK parameters complete",
    pathFolder = logFolder,
    logTypes = LogTypes$Debug
  )
  return(pkAnalyses)
}

#' @title plotMeanPKParameters
#' @description Plot PK parameters table
#' @param structureSet `SimulationStructure` R6 class object
#' @param logFolder folder where the logs are saved
#' @param settings list of settings for the output table/plot
#' @return data.frame with calculated PK parameters for the simulation set
#' @import ospsuite
#' @keywords internal
plotMeanPKParameters <- function(structureSet,
                                 logFolder = getwd(),
                                 settings = NULL) {
  pkParametersData <- NULL

  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$simulationFile)
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet, loadFromCache = TRUE)

  re.tStoreFileMetadata(access = "read", filePath = structureSet$pkAnalysisResultsFileNames)
  pkAnalyses <- ospsuite::importPKAnalysesFromCSV(
    structureSet$pkAnalysisResultsFileNames,
    simulation
  )

  pkParametersTable <- ospsuite::pkAnalysesAsDataFrame(pkAnalyses)
  for (output in structureSet$simulationSet$outputs) {
    molWeight <- simulation$molWeightFor(output$path)

    pkParametersData <- rbind.data.frame(
      pkParametersData,
      getMeanPKAnalysesFromOutput(pkParametersTable, output, molWeight)
    )
  }
  pkParametersData$Value <- replaceInfWithNA(pkParametersData$Value, logFolder)
  pkParameterCaptions <- captions$plotPKParameters$mean(
    structureSet$simulationSet$simulationSetName,
    structureSet$simulationSetDescriptor
  )
  return(list(
    plots = NULL,
    tables = list(pkAnalysis = pkParametersData),
    captions = list(pkAnalysis = pkParameterCaptions)
  ))
}

#' @title getMeanPKAnalysesFromOutput
#' @description Get PK analyses from an `Output` object
#' @param data A data.frame of PK Analyses
#' @param output An `Output` object defining `pkParameters`
#' @param molWeight Molecular weight for converting into PK Parameter `displayUnit`
#' @return A data.frame with `Path`, `Parameter`, `Value` and `Unit` to display in final report
#' @import ospsuite
#' @keywords internal
getMeanPKAnalysesFromOutput <- function(data, output, molWeight = NULL) {
  pkAnalysesFromOutput <- NULL
  ospsuite.utils::validateIsIncluded(output$path, unique(data$QuantityPath))
  outputData <- data[data$QuantityPath %in% output$path, ]

  for (pkParameter in output$pkParameters) {
    displayName <- pkParameter$displayName
    displayUnit <- pkParameter$displayUnit

    ospsuite.utils::validateIsIncluded(pkParameter$pkParameter, unique(outputData$Parameter))
    selectedParameter <- outputData$Parameter %in% pkParameter$pkParameter
    pkParameterObject <- ospsuite::pkParameterByName(pkParameter$pkParameter)

    # Need to switch back to base unit first if a display unit is provided
    pkParameterValue <- outputData$Value[selectedParameter]

    if (!is.null(displayUnit)) {
      pkParameterValue <- ospsuite::toUnit(
        quantityOrDimension = pkParameterObject$dimension,
        values = as.numeric(outputData$Value[selectedParameter]),
        targetUnit = displayUnit,
        molWeight = molWeight,
        sourceUnit = pkParameterObject$displayUnit
      )
    }

    pkAnalysesFromOutput <- rbind.data.frame(
      pkAnalysesFromOutput,
      data.frame(
        Path = output$displayName,
        Parameter = displayName %||% outputData$Parameter[selectedParameter],
        Value = pkParameterValue,
        Unit = displayUnit %||% outputData$Unit[selectedParameter]
      )
    )
  }
  return(pkAnalysesFromOutput)
}

#' @title plotPopulationPKParameters
#' @description Plot PK parameters box plots and tables
#' @param structureSets `SimulationStructure` R6 class object
#' @param logFolder folder where the logs are saved
#' @param settings list of settings for the output table/plot
#' @param workflowType workflowType Type of population workflow.
#' Use enum `PopulationWorkflowTypes` to get list of workflow types.
#' @param xParameters list of parameters to be plotted along x axis
#' @param yParameters list of parameters to be plotted along y axis
#' @return list of plots and tables with summary of PK parameters
#' @import ospsuite
#' @import tlf
#' @import ggplot2
#' @keywords internal
plotPopulationPKParameters <- function(structureSets,
                                       logFolder = getwd(),
                                       settings = NULL,
                                       workflowType = PopulationWorkflowTypes$parallelComparison,
                                       xParameters = getDefaultPkParametersXParameters(workflowType),
                                       yParameters = NULL) {
  ospsuite.utils::validateIsIncluded(workflowType, PopulationWorkflowTypes)
  ospsuite.utils::validateIsOfType(structureSets, "list")
  ospsuite.utils::validateIsOfType(c(structureSets), "SimulationStructure")
  ospsuite.utils::validateIsString(c(xParameters), nullAllowed = TRUE)
  ospsuite.utils::validateIsOfType(c(yParameters), "Output", nullAllowed = TRUE)
  validateSameOutputsBetweenSets(
    c(lapply(structureSets, function(set) {
      set$simulationSet
    })), logFolder
  )

  # Use first structure set as reference
  yParameters <- yParameters %||% structureSets[[1]]$simulationSet$outputs
  # Get first simulation, in case mol weight is needed
  simulation <- loadSimulationWithUpdatedPaths(structureSets[[1]]$simulationSet, loadFromCache = TRUE)
  simulationSetDescriptor <- structureSets[[1]]$simulationSetDescriptor

  pkRatioTableAcrossPopulations <- NULL
  pkParametersPlots <- list()
  pkParametersCaptions <- list()
  pkParametersTables <- list()
  pkParametersCaptionTables <- list()

  pkParametersMapping <- tlf::BoxWhiskerDataMapping$new(
    x = "simulationSetName",
    y = "Value"
  )

  pkParametersAcrossPopulations <- getPKParametersAcrossPopulations(structureSets)
  pkParametersDataAcrossPopulations <- pkParametersAcrossPopulations$data
  pkParametersMetaDataAcrossPopulations <- pkParametersAcrossPopulations$metaData
  simulationSetNames <- unique(as.character(pkParametersDataAcrossPopulations$simulationSetName))

  checkIsIncluded(xParameters, names(pkParametersDataAcrossPopulations), nullAllowed = TRUE, groupName = "PK parameters variable names across simulation sets", logFolder = logFolder)
  xParameters <- intersect(xParameters, names(pkParametersDataAcrossPopulations))

  # Enforce factors for population names with order as input by the user
  pkParametersDataAcrossPopulations$simulationSetName <- factor(pkParametersDataAcrossPopulations$simulationSetName,
    levels = unique(pkParametersDataAcrossPopulations$simulationSetName)
  )

  if (workflowType %in% c(PopulationWorkflowTypes$ratioComparison, PopulationWorkflowTypes$pediatric)) {
    referenceSimulationSetName <- getReferencePopulationName(structureSets)
  }

  # Standard boxplots for each pkParameters of each output
  for (output in yParameters) {
    molWeight <- simulation$molWeightFor(output$path)
    pathLabel <- lastPathElement(output$path)
    for (pkParameter in output$pkParameters) {
      yParameterLabel <- lastPathElement(pkParameter$pkParameter)
      plotID <- paste0(pathLabel, "-", yParameterLabel)

      pkParameterFromOutput <- getPopulationPKAnalysesFromOutput(
        pkParametersDataAcrossPopulations,
        pkParametersMetaDataAcrossPopulations,
        output,
        pkParameter,
        molWeight
      )

      pkParameterData <- pkParameterFromOutput$data
      pkParameterMetaData <- pkParameterFromOutput$metaData

      # NA and Inf values are removed to prevent crash in computation of percentiles
      pkParameterData <- removeMissingValues(pkParameterData, "Value", logFolder)
      if (nrow(pkParameterData) == 0) {
        logWorkflow(
          message = paste0(pkParameter$pkParameter, " of ", output$path, ": not enough available data to perform plot."),
          pathFolder = logFolder,
          logTypes = c(LogTypes$Info, LogTypes$Error, LogTypes$Debug)
        )
        next
      }

      boxplotPkParameter <- tlf::plotBoxWhisker(
        data = pkParameterData,
        metaData = pkParameterMetaData,
        dataMapping = pkParametersMapping,
        plotConfiguration = settings$plotConfigurations[["boxplotPkParameters"]]
      ) + ggplot2::xlab(NULL)

      pkParametersPlots[[plotID]] <- boxplotPkParameter
      parameterCaption <- pkParameterMetaData$Value$dimension
      pkParametersCaptions[[plotID]] <- captions$plotPKParameters$boxplot(parameterCaption, output$displayName, simulationSetNames, simulationSetDescriptor)

      if (!hasPositiveValues(pkParameterData$Value)) {
        logWorkflow(
          message = messages$warningLogScaleNoPositiveData(paste0(pkParameter$pkParameter, " of ", output$path)),
          pathFolder = logFolder,
          logTypes = c(LogTypes$Info, LogTypes$Error, LogTypes$Debug)
        )
      }
      if (hasPositiveValues(pkParameterData$Value)) {
        positiveValues <- pkParameterData$Value > 0
        boxRange <- autoAxesLimits(pkParameterData$Value[positiveValues], scale = "log")
        boxBreaks <- autoAxesTicksFromLimits(boxRange)

        pkParametersPlots[[paste0(plotID, "-log")]] <- tlf::setYAxis(
          plotObject = boxplotPkParameter,
          scale = tlf::Scaling$log,
          limits = boxRange,
          ticks = boxBreaks
        )
        pkParametersCaptions[[paste0(plotID, "-log")]] <- captions$plotPKParameters$boxplot(
          parameterCaption,
          output$displayName,
          simulationSetNames,
          simulationSetDescriptor,
          plotScale = "logarithmic"
        )
      }

      # Report tables summarizing the distributions
      pkParameterTable <- tlf::getBoxWhiskerMeasure(
        data = pkParameterData,
        dataMapping = pkParametersMapping
      )

      # Row names are added as factor to data.frames by default
      # This line ensures that the order of the rows is kept for the tables and plots
      pkParameterTableRows <- factor(row.names(pkParameterTable),
        levels = row.names(pkParameterTable)
      )
      pkParameterTable <- cbind(
        Population = pkParameterTableRows,
        pkParameterTable
      )

      # A different table needs to be created here because of ratio comparison of the table values
      savedPKParameterTable <- addDescriptorToTable(pkParameterTable, simulationSetDescriptor)
      pkParametersTables[[paste0(plotID)]] <- savedPKParameterTable
      pkParametersCaptionTables[[plotID]] <- captions$plotPKParameters$summaryTable(
        parameterCaption,
        output$displayName,
        simulationSetNames,
        simulationSetDescriptor,
        pkParameterMetaData$Value$unit
      )

      # Range plots on PK parameters vs xParameters
      for (demographyParameter in setdiff(xParameters, pkParameter$pkParameter)) {
        if (pkParametersMetaDataAcrossPopulations[[demographyParameter]]$class %in% "character") {
          next
        }
        xParameterLabel <- lastPathElement(demographyParameter)
        vpcMetaData <- list(
          "x" = pkParameterMetaData[[demographyParameter]],
          "median" = pkParameterMetaData$Value
        )

        # For pediatric workflow, range plots compare reference population to the other populations
        if (workflowType %in% c(PopulationWorkflowTypes$pediatric)) {
          # Get the table for reference population
          referenceData <- data.frame(
            x = c(-Inf, Inf),
            "Population" = paste("Simulated", AggregationConfiguration$names$middle, "and", AggregationConfiguration$names$range, "of", referenceSimulationSetName)
          )
          referenceData[, c("ymin", "median", "ymax")] <- pkParameterTable[referenceSimulationSetName, c(3, 5, 7)]

          referenceVpcPlot <- vpcParameterPlot(
            data = referenceData,
            metaData = vpcMetaData,
            plotConfiguration = settings$plotConfigurations[["comparisonVpcPlot"]]
          )

          for (simulationSetName in simulationSetNames[!simulationSetNames %in% referenceSimulationSetName]) {
            plotID <- paste0(simulationSetName, "-vs-ref-", yParameterLabel, "-vs-", xParameterLabel)
            comparisonData <- pkParameterData[pkParameterData$simulationSetName %in% simulationSetName, ]
            comparisonData <- getDemographyAggregatedData(
              data = comparisonData,
              xParameterName = demographyParameter,
              yParameterName = "Value",
              bins = settings$bins,
              stairstep = settings$stairstep
            )
            comparisonData$Population <- paste("Simulated", AggregationConfiguration$names$middle, "and", AggregationConfiguration$names$range)

            comparisonVpcPlot <- vpcParameterPlot(
              data = comparisonData,
              metaData = vpcMetaData,
              plotObject = referenceVpcPlot
            )

            pkParametersPlots[[plotID]] <- comparisonVpcPlot
            pkParametersPlots[[paste0(plotID, "-log")]] <- tlf::setYAxis(plotObject = comparisonVpcPlot, scale = tlf::Scaling$log)

            xParameterCaption <- vpcMetaData$x$dimension
            yParameterCaption <- vpcMetaData$median$dimension
            pkParametersCaptions[[plotID]] <- captions$plotPKParameters$rangePlot(
              xParameterCaption,
              yParameterCaption,
              simulationSetName,
              simulationSetDescriptor,
              referenceSetName = referenceSimulationSetName
            )
            pkParametersCaptions[[paste0(plotID, "-log")]] <- captions$plotPKParameters$rangePlot(
              xParameterCaption,
              yParameterCaption,
              simulationSetName,
              simulationSetDescriptor,
              referenceSetName = referenceSimulationSetName,
              plotScale = "logarithmic"
            )
          }
        }

        # Regular range plots not associated to workflow type
        for (simulationSetName in simulationSetNames) {
          plotID <- paste0(simulationSetName, "-", yParameterLabel, "-vs-", xParameterLabel)
          vpcData <- pkParameterData[pkParameterData$simulationSetName %in% simulationSetName, ]
          vpcData <- getDemographyAggregatedData(
            data = vpcData,
            xParameterName = demographyParameter,
            yParameterName = "Value",
            bins = settings$bins,
            stairstep = settings$stairstep
          )

          vpcData$Population <- paste("Simulated", AggregationConfiguration$names$middle, "and", AggregationConfiguration$names$range)
          vpcPlot <- vpcParameterPlot(
            data = vpcData,
            metaData = vpcMetaData,
            plotConfiguration = settings$plotConfigurations[["vpcParameterPlot"]]
          )
          pkParametersPlots[[plotID]] <- vpcPlot
          pkParametersPlots[[paste0(plotID, "-log")]] <- tlf::setYAxis(plotObject = vpcPlot, scale = tlf::Scaling$log)

          xParameterCaption <- vpcMetaData$x$dimension
          yParameterCaption <- vpcMetaData$median$dimension
          pkParametersCaptions[[plotID]] <- captions$plotPKParameters$rangePlot(
            xParameterCaption,
            yParameterCaption,
            simulationSetName,
            simulationSetDescriptor
          )
          pkParametersCaptions[[paste0(plotID, "-log")]] <- captions$plotPKParameters$rangePlot(
            xParameterCaption,
            yParameterCaption,
            simulationSetName,
            simulationSetDescriptor,
            plotScale = "logarithmic"
          )
        }
      }

      # For Ratio Comparison create boxplots of boxplot hinges ratios
      if (workflowType %in% PopulationWorkflowTypes$ratioComparison) {
        plotID <- paste0(pathLabel, "-", yParameterLabel, "-ratio")
        # Get the tables and compute the ratios using reference population name
        pkRatiosTable <- getPKRatiosTable(pkParameterTable, referenceSimulationSetName)

        pkRatiosData <- pkRatiosTable
        pkRatiosData[, c("ymin", "lower", "middle", "upper", "ymax")] <- pkRatiosTable[, c(3:7)]

        boxplotPkRatios <- ratioBoxplot(
          data = pkRatiosData,
          plotConfiguration = settings$plotConfigurations[["boxplotPkRatios"]]
        ) + ggplot2::ylab(paste0(pkParameterMetaData$Value$dimension, " [fraction of ", referenceSimulationSetName, "]"))

        ratioRange <- autoAxesLimits(c(pkRatiosData$ymin, pkRatiosData$ymax), scale = "log")
        ratioBreaks <- autoAxesTicksFromLimits(ratioRange)

        pkParametersPlots[[plotID]] <- boxplotPkRatios
        pkParametersPlots[[paste0(plotID, "-log")]] <- tlf::setYAxis(
          plotObject = boxplotPkRatios,
          scale = tlf::Scaling$log,
          limits = ratioRange,
          ticks = ratioBreaks
        )

        parameterCaption <- pkParameterMetaData$Value$dimension
        pkParametersCaptions[[plotID]] <- captions$plotPKParameters$ratioPlot(
          parameterCaption,
          output$displayName,
          setdiff(simulationSetNames, referenceSimulationSetName),
          simulationSetDescriptor,
          referenceSetName = referenceSimulationSetName
        )
        pkParametersCaptions[[paste0(plotID, "-log")]] <- captions$plotPKParameters$ratioPlot(
          parameterCaption,
          output$displayName,
          setdiff(simulationSetNames, referenceSimulationSetName),
          simulationSetDescriptor,
          referenceSetName = referenceSimulationSetName,
          plotScale = "logarithmic"
        )

        pkRatiosTable <- addDescriptorToTable(pkRatiosTable, simulationSetDescriptor)
        pkParametersTables[[plotID]] <- pkRatiosTable
        pkParametersCaptionTables[[plotID]] <- captions$plotPKParameters$ratioTable(
          parameterCaption,
          output$displayName,
          setdiff(simulationSetNames, referenceSimulationSetName),
          simulationSetDescriptor,
          referenceSetName = referenceSimulationSetName
        )
      }
    }
  }

  return(list(
    plots = pkParametersPlots,
    tables = pkParametersTables,
    captions = pkParametersCaptions,
    tableCaptions = pkParametersCaptionTables
  ))
}

#' @title ratioBoxplot
#' @description Plot box-whiskers of ratios as is
#' @param data data.frame of the ratios
#' @param plotConfiguration PlotConfiguration R6 class object
#' @return ggplot object
#' @export
#' @import ospsuite
#' @import tlf
#' @import ggplot2
ratioBoxplot <- function(data,
                         plotConfiguration = NULL) {
  ratioPlot <- tlf::initializePlot(plotConfiguration)

  ratioPlot <- ratioPlot +
    ggplot2::geom_boxplot(
      data = data,
      mapping = ggplot2::aes_string(
        x = "Population",
        ymin = "ymin",
        lower = "lower",
        middle = "middle",
        upper = "upper",
        ymax = "ymax"
      ),
      stat = "identity",
      fill = "#999999",
      alpha = 0.8,
      size = 1
    )
  ratioPlot <- ratioPlot + ggplot2::xlab(NULL)
  return(ratioPlot)
}

#' @title vpcParameterPlot
#' @description Plot vpc like plot of yParameter along xParameter
#' @param data data.frame
#' @param metaData list of metaData about `data`
#' @param plotConfiguration PlotConfiguration R6 class object
#' @param plotObject ggplot object to which layer is added
#' @return ggplot object
#' @export
#' @import ospsuite
#' @import tlf
#' @import ggplot2
vpcParameterPlot <- function(data,
                             metaData = NULL,
                             plotConfiguration = NULL,
                             plotObject = NULL) {
  vpcDataMapping <- tlf::XYGDataMapping$new(
    x = "x",
    y = "median",
    data = data
  )

  vpcPlotConfiguration <- plotConfiguration %||% tlf::PlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = vpcDataMapping
  )
  vpcPlot <- plotObject %||% tlf::initializePlot(vpcPlotConfiguration)

  vpcPlot <- vpcPlot +
    ggplot2::geom_ribbon(
      data = data,
      mapping = ggplot2::aes_string(
        x = "x",
        ymin = "ymin",
        ymax = "ymax",
        fill = "Population",
        color = "Population"
      ),
      size = 0.5,
      alpha = 0.8
    ) +
    ggplot2::geom_line(
      data = data,
      mapping = ggplot2::aes_string(
        x = "x",
        y = "median",
        color = "Population"
      ),
      linetype = "solid",
      size = 1
    )
  vpcPlot <- tlf::setLegendPosition(plotObject = vpcPlot, position = reDefaultLegendPosition)
  vpcPlot <- vpcPlot + ggplot2::theme(legend.title = ggplot2::element_blank())

  return(vpcPlot)
}

#' @title getPKParametersAcrossPopulations
#' @description Get the values of PK parameters across Population Simulation sets
#' @param structureSets list of `SimulationStructures` objects
#' @return list of data.frame and its metaData including the values of PK parameters across Population Simulation sets
#' @keywords internal
getPKParametersAcrossPopulations <- function(structureSets) {
  pkParametersTableAcrossPopulations <- NULL
  for (structureSet in structureSets) {
    simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet, loadFromCache = TRUE)
    population <- loadWorkflowPopulation(structureSet$simulationSet)
    pkAnalyses <- ospsuite::importPKAnalysesFromCSV(
      structureSet$pkAnalysisResultsFileNames,
      simulation
    )
    pkParametersTable <- ospsuite::pkAnalysesAsDataFrame(pkAnalyses)
    populationTable <- getPopulationAsDataFrame(population, simulation)

    pkParametersTable <- formatPKParametersTable(
      structureSet,
      pkParametersTable,
      populationTable
    )
    pkParametersTableAcrossPopulations <- rbindPKParametersTables(
      pkParametersTableAcrossPopulations,
      pkParametersTable
    )
  }
  metaData <- getPopulationMetaData(population, simulation, structureSet$parameterDisplayPaths)

  return(list(
    data = pkParametersTableAcrossPopulations,
    metaData = metaData
  ))
}

#' @title formatPKParametersTable
#' @description Format data.frame of PK and Population Parameters from a simulation set
#' @param structureSets A `SimulationStructure` objects
#' @param pkParametersTable A data.frame of PK parameters
#' @param populationTable A data.frame of population parameters
#' @return A data.frame and its metaData including the values of PK parameters across Population Simulation sets
#' @keywords internal
formatPKParametersTable <- function(structureSet, pkParametersTable, populationTable) {
  # Use merge instead of cbind which uses the intersect in names
  # Data conseuently match by IndividualId
  pkParametersTable <- merge.data.frame(
    pkParametersTable,
    populationTable
  )
  # Add the simulationSetName for the legend captions
  # And group identifier to get the appropriate PK parameters
  pkParametersTable <- cbind.data.frame(
    simulationSetName = structureSet$simulationSet$simulationSetName,
    group = NA,
    pkParametersTable
  )
  # Map groups of pkParameter objects
  for (output in structureSet$simulationSet$outputs) {
    for (pkParameter in output$pkParameters) {
      selectedRows <- (pkParametersTable$QuantityPath %in% output$path) & (pkParametersTable$Parameter %in% pkParameter$pkParameter)
      pkParametersTable$group[selectedRows] <- pkParameter$group
    }
  }
  return(pkParametersTable)
}

#' @title rbindPKParametersTables
#' @description Concatenate data.frames of PK and Population Parameters across simulation sets
#' @param pkParametersTableAcrossPopulations A data.frame of PK and Population Parameters across simulation sets
#' @param pkParametersTable A data.frame of PK and Population Parameters for a simulation set
#' @return A data.frame of PK and Population Parameters across simulation sets
#' @keywords internal
rbindPKParametersTables <- function(pkParametersTableAcrossPopulations, pkParametersTable) {
  if (ospsuite.utils::isOfLength(pkParametersTableAcrossPopulations, 0)) {
    return(pkParametersTable)
  }
  # Prevent crash when merging populations with different columns
  # Unmatched variables are filled with NAs
  naVariablesForPKParametersTableAcrossPopulations <- setdiff(
    names(pkParametersTable),
    names(pkParametersTableAcrossPopulations)
  )
  naVariablesForPKParametersTable <- setdiff(
    names(pkParametersTableAcrossPopulations),
    names(pkParametersTable)
  )
  pkParametersTableAcrossPopulations[, naVariablesForPKParametersTableAcrossPopulations] <- NA
  pkParametersTable[, naVariablesForPKParametersTable] <- NA

  pkParametersTableAcrossPopulations <- rbind.data.frame(
    pkParametersTableAcrossPopulations,
    pkParametersTable
  )
  return(pkParametersTableAcrossPopulations)
}

getPKRatiosTable <- function(pkParametersTable,
                             referenceSimulationSetName) {
  simulationSetNames <- pkParametersTable$Population

  pkRatiosTable <- pkParametersTable[simulationSetNames != referenceSimulationSetName, ]
  referencePkParametersTable <- pkParametersTable[rep(referenceSimulationSetName, length(pkRatiosTable$Population)), ]

  pkRatiosTable[, seq(3, ncol(pkRatiosTable))] <- pkRatiosTable[, seq(3, ncol(pkRatiosTable))] / referencePkParametersTable[, seq(3, ncol(pkRatiosTable))]

  return(pkRatiosTable)
}

#' @title getDefaultPkParametersXParameters
#' @description Get names of default parameters in x axis of pk parameters plots.
#' @param workflowType Name of workflow type.
#' Use enum `PopulationWorkflowTypes` to get a list of available workflow types.
#' @return names of default parameters
#' @export
#' @examples
#' getDefaultPkParametersXParameters(PopulationWorkflowTypes$pediatric)
getDefaultPkParametersXParameters <- function(workflowType) {
  ospsuite.utils::validateIsIncluded(workflowType, PopulationWorkflowTypes)
  if (workflowType %in% PopulationWorkflowTypes$pediatric) {
    return(DemographyDefaultParameters)
  }
  return(NULL)
}

#' @title getPopulationPKAnalysesFromOutput
#' @description Get the values of PK parameters specified by an `Output` object from a data.frame
#' @param data data.frame of the PK Analyses across Population Simulation sets
#' @param metaData metaData (dimension and unit) of the PK Analyses across Population Simulation sets
#' @param output An `Output ` object
#' @param pkParameter `pkParameter` from `Output ` object
#' @param molWeight Molecular weight of compound (if unit conversion needed)
#' @return list of data.frame and its metaData including the values of PK parameters specified by `pkParameter` and `Output` objects
#' @keywords internal
getPopulationPKAnalysesFromOutput <- function(data, metaData, output, pkParameter, molWeight = NULL) {
  ospsuite.utils::validateIsIncluded(output$path, unique(data$QuantityPath))
  outputData <- data[data$QuantityPath %in% output$path, ]

  displayName <- pkParameter$displayName
  displayUnit <- pkParameter$displayUnit

  # Caution: now using group instead of pkParameter and Parameter
  ospsuite.utils::validateIsIncluded(pkParameter$group, unique(outputData$group))
  selectedParameter <- outputData$group %in% pkParameter$group
  pkParameterObject <- ospsuite::pkParameterByName(pkParameter$pkParameter)

  # Need to switch back to base unit first if a display unit is provided
  pkParameterValue <- outputData$Value[selectedParameter]

  if (!is.null(displayUnit)) {
    pkParameterValue <- ospsuite::toUnit(
      quantityOrDimension = pkParameterObject$dimension,
      values = as.numeric(outputData$Value[selectedParameter]),
      targetUnit = displayUnit,
      molWeight = molWeight,
      sourceUnit = pkParameterObject$displayUnit
    )
  }

  pkAnalysesFromOutput <- outputData[selectedParameter, ]
  pkAnalysesFromOutput$Value <- pkParameterValue

  pkAnalysesFromOutputMetaData <- metaData
  pkAnalysesFromOutputMetaData$Value <- list(
    dimension = displayName %||% pkAnalysesFromOutput$Parameter[1],
    unit = displayUnit %||% pkAnalysesFromOutput$Unit[1]
  )

  return(list(
    data = pkAnalysesFromOutput,
    metaData = pkAnalysesFromOutputMetaData
  ))
}

#' @title getXParametersForPkParametersPlot
#' @param workflow `PopulationWorkflow` R6 class object
#' @return list of x-parameters used for PK parameters range plots
#' @export
getXParametersForPkParametersPlot <- function(workflow) {
  ospsuite.utils::validateIsOfType(workflow, "PopulationWorkflow")
  return(workflow$plotPKParameters$xParameters)
}

#' @title getYParametersForPkParametersPlot
#' @param workflow `PopulationWorkflow` R6 class object
#' @return list of y-parameters used for PK parameters range plots and boxplots
#' @export
getYParametersForPkParametersPlot <- function(workflow) {
  ospsuite.utils::validateIsOfType(workflow, "PopulationWorkflow")
  yParameters <- workflow$plotPKParameters$yParameters %||% workflow$simulationStructures[[1]]$simulationSet$outputs

  return(yParameters)
}

#' @title setXParametersForPkParametersPlot
#' @description Set x-parameters for range plots of PK parameters plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of parameters to be used as x-parameters
#' @export
setXParametersForPkParametersPlot <- function(workflow, parameters) {
  ospsuite.utils::validateIsOfType(workflow, "PopulationWorkflow")
  ospsuite.utils::validateIsString(c(parameters))

  workflow$plotPKParameters$xParameters <- parameters

  logWorkflow(
    message = paste0(
      "X-parameters: '",
      paste0(c(parameters), collapse = "', '"),
      "' set for PK parameters plot."
    ),
    pathFolder = workflow$workflowFolder,
    logTypes = LogTypes$Debug
  )
  return(invisible())
}

#' @title addXParametersForPkParametersPlot
#' @description Apppend x-parameters for range plots of PK parameters plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of parameters to be used as x-parameters
#' @export
addXParametersForPkParametersPlot <- function(workflow, parameters) {
  updatedParameters <- c(getXParametersForPkParametersPlot(workflow), parameters)
  setXParametersForPkParametersPlot(workflow, updatedParameters)
}

#' @title setYParametersForPkParametersPlot
#' @description Set y-parameters for boxplots and range plots of PK parameters plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of R6 class `Output` objects to be used as y-parameters
#' @export
setYParametersForPkParametersPlot <- function(workflow, parameters) {
  ospsuite.utils::validateIsOfType(workflow, "PopulationWorkflow")
  ospsuite.utils::validateIsOfType(c(parameters), "Output")

  workflow$plotPKParameters$yParameters <- parameters

  for (output in c(parameters)) {
    logWorkflow(
      message = paste0(
        "Y-parameters: '",
        paste0(c(output$pkParameters), collapse = "', '"),
        "' for '", output$path, "' set for PK parameters plot."
      ),
      pathFolder = workflow$workflowFolder,
      logTypes = LogTypes$Debug
    )
  }
  return(invisible())
}

#' @title addYParametersForPkParametersPlot
#' @description Apppend y-parameters for range plots of PK parameters plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of parameters to be used as y-parameters
#' @export
addYParametersForPkParametersPlot <- function(workflow, parameters) {
  updatedParameters <- c(getYParametersForPkParametersPlot(workflow), parameters)
  setYParametersForPkParametersPlot(workflow, updatedParameters)
}
