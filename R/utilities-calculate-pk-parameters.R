#' @title calculatePKParameters
#' @description Calculate PK parameters from simulated time profiles
#' @param structureSet `SimulationStructure` R6 class object contain paths of files to be used
#' @param settings list of options to be passed on the function
#' @param logFolder folder where the logs are saved
#' @return pkAnalysis object
#' @export
#' @import ospsuite
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
#' @export
#' @import ospsuite
plotMeanPKParameters <- function(structureSet,
                                 logFolder = getwd(),
                                 settings = NULL) {
  pkParametersData <- NULL

  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$simulationFile)
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)

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
      getMeanPkAnalysesFromOuptut(pkParametersTable, output, molWeight)
    )
  }
  pkParametersData$Value <- replaceInfWithNA(pkParametersData$Value, logFolder)
  pkParametersData$Value <- formatNumerics(numerics = pkParametersData$Value,  
                                           digits = settings$digits,
                                           nsmall = settings$nsmall,
                                           scientific = settings$scientific)
  return(list(
    plots = NULL,
    tables = list(pkAnalysis = pkParametersData)
  ))
}

getMeanPkAnalysesFromOuptut <- function(data, output, molWeight = NULL) {
  pkAnalysesFromOuptut <- NULL
  validateIsIncluded(output$path, unique(data$QuantityPath))
  outputData <- data[data$QuantityPath %in% output$path, ]

  for (pkParameter in output$pkParameters) {
    displayName <- pkParameter$displayName
    displayUnit <- pkParameter$displayUnit

    validateIsIncluded(pkParameter$pkParameter, unique(outputData$Parameter))
    selectedParameter <- outputData$Parameter %in% pkParameter$pkParameter
    pkParameterObject <- ospsuite::pkParameterByName(pkParameter$pkParameter)

    # Need to switch back to base unit first if a display unit is provided
    pkParameterValue <- outputData$Value[selectedParameter]

    if (!is.null(displayUnit)) {
      pkParameterValueInBaseUnit <- ospsuite::toBaseUnit(
        pkParameterObject$dimension,
        outputData$Value[selectedParameter],
        pkParameterObject$displayUnit,
        molWeight
      )
      pkParameterValue <- ospsuite::toUnit(
        pkParameterObject$dimension,
        pkParameterValueInBaseUnit,
        displayUnit,
        molWeight
      )
    }

    pkAnalysesFromOuptut <- rbind.data.frame(
      pkAnalysesFromOuptut,
      data.frame(
        Path = output$displayName,
        Parameter = displayName %||% outputData$Parameter[selectedParameter],
        Value = pkParameterValue,
        Unit = displayUnit %||% outputData$Unit[selectedParameter]
      )
    )
  }
  return(pkAnalysesFromOuptut)
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
#' @export
#' @import ospsuite
#' @import tlf
#' @import ggplot2
plotPopulationPKParameters <- function(structureSets,
                                       logFolder = getwd(),
                                       settings = NULL,
                                       workflowType = PopulationWorkflowTypes$parallelComparison,
                                       xParameters = getDefaultPkParametersXParameters(workflowType),
                                       yParameters = NULL) {
  validateIsIncluded(workflowType, PopulationWorkflowTypes)
  validateIsOfType(structureSets, "list")
  validateIsOfType(c(structureSets), "SimulationStructure")
  validateIsString(c(xParameters), nullAllowed = TRUE)
  validateIsOfType(c(yParameters), "Output", nullAllowed = TRUE)
  validateSameOutputsBetweenSets(c(lapply(structureSets, function(set) {
    set$simulationSet
  })), logFolder)

  # Use first structure set as reference
  yParameters <- yParameters %||% structureSets[[1]]$simulationSet$outputs
  # Get first simulation, in case mol weight is needed
  simulation <- loadSimulationWithUpdatedPaths(structureSets[[1]]$simulationSet)

  pkRatioTableAcrossPopulations <- NULL
  pkParametersPlots <- list()
  pkParametersCaptions <- list()
  pkParametersTables <- list()

  pkParametersMapping <- tlf::BoxWhiskerDataMapping$new(
    x = "simulationSetName",
    y = "Value"
  )

  pkParametersAcrossPopulations <- getPkParametersAcrossPopulations(structureSets)
  pkParametersDataAcrossPopulations <- pkParametersAcrossPopulations$data
  pkParametersMetaDataAcrossPopulations <- pkParametersAcrossPopulations$metaData

  checkIsIncluded(xParameters, names(pkParametersDataAcrossPopulations), nullAllowed = TRUE, groupName = "population variables", logFolder = logFolder)
  xParameters <- intersect(xParameters, names(pkParametersDataAcrossPopulations))

  # Enforce factors for population names with order as input by the user
  pkParametersDataAcrossPopulations$simulationSetName <- factor(pkParametersDataAcrossPopulations$simulationSetName,
                                                                levels = unique(pkParametersDataAcrossPopulations$simulationSetName))

  if (workflowType %in% c(PopulationWorkflowTypes$ratioComparison, PopulationWorkflowTypes$pediatric)) {
    referencePopulationName <- getReferencePopulationName(structureSets)
  }

  # Standard boxplots for each pkParameters of each output
  for (output in yParameters) {
    molWeight <- simulation$molWeightFor(output$path)
    pathLabel <- lastPathElement(output$path)
    for (pkParameter in output$pkParameters) {
      yParameterLabel <- lastPathElement(pkParameter$pkParameter)

      pkParameterFromOutput <- getPopulationPkAnalysesFromOuptut(
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
          message = paste0(
            pkParameter$pkParameter, " of ", output$path,
            ": not enough available data to perform plot."
          ),
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

      pkParametersPlots[[paste0(pathLabel, "-", yParameterLabel)]] <- boxplotPkParameter
      pkParametersCaptions[[paste0(pathLabel, "-", yParameterLabel)]] <- getPkParametersCaptions("boxplot", output$displayName, pkParameterMetaData[["Value"]])

      if (!hasPositiveValues(pkParameterData$Value)) {
        logWorkflow(
          message = messages$warningLogScaleNoPositiveData(paste0(pkParameter$pkParameter, " of ", output$path)),
          pathFolder = logFolder,
          logTypes = c(LogTypes$Info, LogTypes$Error, LogTypes$Debug)
        )
      }
      if (hasPositiveValues(pkParameterData$Value)) {
        positiveValues <- pkParameterData$Value > 0
        boxRange <- getLogLimitsForBoxPlot(pkParameterData$Value[positiveValues])
        boxBreaks <- getLogBreaksForBoxPlot(boxRange)

        pkParametersPlots[[paste0(pathLabel, "-", yParameterLabel, "-log")]] <- tlf::setYAxis(
          plotObject = boxplotPkParameter,
          scale = tlf::Scaling$log10,
          limits = boxRange,
          ticks = boxBreaks
        )
        pkParametersCaptions[[paste0(pathLabel, "-", yParameterLabel, "-log")]] <- getPkParametersCaptions("boxplot", output$displayName, pkParameterMetaData[["Value"]], plotScale = "log")
      }

      pkParameterTable <- tlf::getBoxWhiskerMeasure(
        data = pkParameterData,
        dataMapping = pkParametersMapping
      )

      # Row names are added as factor to data.frames by default
      # This line ensures that the order of the rows is kept for the tables and plots
      pkParameterTableRows <- factor(row.names(pkParameterTable), 
                                     levels = row.names(pkParameterTable))
      pkParameterTable <- cbind(
        Population = pkParameterTableRows,
        pkParameterTable
      )

      # A different table needs to be created here because of ratio comparison of the table values 
      savedPKParameterTable <- pkParameterTable 
      savedPKParameterTable[, 3:ncol(pkParameterTable)] <- sapply(
        pkParameterTable[, 3:ncol(pkParameterTable)], 
        function(values) {
          formatNumerics(values, digits = settings$digits, nsmall = settings$nsmall, scientific = settings$scientific)
          }
        ) 
      pkParametersTables[[paste0(pathLabel, "-", yParameterLabel)]] <- savedPKParameterTable

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

        populationNames <- levels(factor(pkParameterData$simulationSetName))

        # For pediatric workflow, range plots compare reference population to the other populations
        if (workflowType %in% c(PopulationWorkflowTypes$pediatric)) {
          # Get the table for reference population
          referenceData <- data.frame(
            x = c(-Inf, Inf),
            "Population" = paste("Simulated", AggregationConfiguration$names$middle, "and", AggregationConfiguration$names$range, "of", referencePopulationName)
          )
          referenceData[, c("ymin", "median", "ymax")] <- pkParameterTable[referencePopulationName, c(3, 5, 7)]

          referenceVpcPlot <- vpcParameterPlot(
            data = referenceData,
            metaData = vpcMetaData,
            plotConfiguration = settings$plotConfigurations[["comparisonVpcPlot"]]
          )

          for (populationName in populationNames[!populationNames %in% referencePopulationName]) {
            comparisonData <- pkParameterData[pkParameterData$simulationSetName %in% populationName, ]
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

            pkParametersPlots[[paste0(populationName, "-vs-ref-", yParameterLabel, "-vs-", xParameterLabel)]] <- comparisonVpcPlot
            pkParametersPlots[[paste0(populationName, "-vs-ref-", yParameterLabel, "-vs-", xParameterLabel, "-log")]] <- tlf::setYAxis(plotObject = comparisonVpcPlot, scale = tlf::Scaling$log10)

            pkParametersCaptions[[paste0(populationName, "-vs-ref-", yParameterLabel, "-vs-", xParameterLabel)]] <- getPkParametersCaptions("rangePlot", populationName, vpcMetaData, referencePopulationName)
            pkParametersCaptions[[paste0(populationName, "-vs-ref-", yParameterLabel, "-vs-", xParameterLabel, "-log")]] <- getPkParametersCaptions("rangePlot", populationName, vpcMetaData, referencePopulationName, plotScale = "log")
          }
        }

        # Regular range plots not associated to workflow type
        for (populationName in populationNames) {
          vpcData <- pkParameterData[pkParameterData$simulationSetName %in% populationName, ]
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
          pkParametersPlots[[paste0(populationName, "-", yParameterLabel, "-vs-", xParameterLabel)]] <- vpcPlot
          pkParametersPlots[[paste0(populationName, "-", yParameterLabel, "-vs-", xParameterLabel, "-log")]] <- tlf::setYAxis(plotObject = vpcPlot, scale = tlf::Scaling$log10)

          pkParametersCaptions[[paste0(populationName, "-", yParameterLabel, "-vs-", xParameterLabel)]] <- getPkParametersCaptions("rangePlot", populationName, vpcMetaData)
          pkParametersCaptions[[paste0(populationName, "-", yParameterLabel, "-vs-", xParameterLabel, "-log")]] <- getPkParametersCaptions("rangePlot", populationName, vpcMetaData, plotScale = "log")
        }
      }

      # For Ratio Comparison create boxplots of boxplot hinges ratios
      if (workflowType %in% PopulationWorkflowTypes$ratioComparison) {
        # Get the tables and compute the ratios using reference population name
        pkRatiosTable <- getPkRatiosTable(pkParameterTable, referencePopulationName)

        pkRatiosData <- pkRatiosTable
        pkRatiosData[, c("ymin", "lower", "middle", "upper", "ymax")] <- pkRatiosTable[, c(3:7)]

        boxplotPkRatios <- ratioBoxplot(
          data = pkRatiosData,
          plotConfiguration = settings$plotConfigurations[["boxplotPkRatios"]]
        ) + ggplot2::ylab(paste0(pkParameterMetaData$Value$dimension, " [fraction of ", referencePopulationName, "]"))

        ratioRange <- getLogLimitsForBoxPlot(c(pkRatiosData$ymin, pkRatiosData$ymax))
        ratioBreaks <- getLogBreaksForBoxPlot(ratioRange)

        pkParametersPlots[[paste0(pathLabel, "-", yParameterLabel, "-ratio")]] <- boxplotPkRatios
        pkParametersPlots[[paste0(pathLabel, "-", yParameterLabel, "-ratio-log")]] <- tlf::setYAxis(
          plotObject = boxplotPkRatios,
          scale = tlf::Scaling$log10,
          limits = ratioRange,
          ticks = ratioBreaks
        )

        pkParametersCaptions[[paste0(pathLabel, "-", yParameterLabel, "-ratio")]] <- getPkParametersCaptions("ratioPlot", output$displayName, pkParameterMetaData[["Value"]])
        pkParametersCaptions[[paste0(pathLabel, "-", yParameterLabel, "-ratio-log")]] <- getPkParametersCaptions("ratioPlot", output$displayName, pkParameterMetaData[["Value"]], plotScale = "log")

        pkRatiosTable[, 3:ncol(pkRatiosTable)] <- sapply(
          pkRatiosTable[, 3:ncol(pkRatiosTable)], 
          function(values) {
            formatNumerics(values, digits = settings$digits, nsmall = settings$nsmall, scientific = settings$scientific)
          }
        ) 
        pkParametersTables[[paste0(pathLabel, "-", yParameterLabel, "-ratio")]] <- pkRatiosTable
      }
    }
  }

  return(list(
    plots = pkParametersPlots,
    tables = pkParametersTables,
    captions = pkParametersCaptions
  ))
}

getLogLimitsForBoxPlot <- function(values) {
  boxRange <- c(min(values, na.rm = TRUE) * 0.8, max(values, na.rm = TRUE) * 1.2)
  if (diff(log10(boxRange)) >= 1) {
    return(boxRange)
  }
  boxRange <- c(min(values, na.rm = TRUE) / 3, max(values, na.rm = TRUE) * 3)
  return(boxRange)
}

getLogBreaksForBoxPlot <- function(limits) {
  logLimits <- round(log10(limits))
  breakOrder <- 10^seq(min(logLimits, na.rm = TRUE), max(logLimits, na.rm = TRUE))
  breakValues <- rep(c(1, 2, 5), length(breakOrder))
  breakOrder <- sort(rep(breakOrder, 3))

  breakValues <- breakValues * breakOrder
  return(breakValues)
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

#' @title getPkParametersAcrossPopulations
#' @description Get the values of PK parameters across Population Simulation sets
#' @param structureSets list of `SimulationStructures` objects
#' @return list of data.frame and its metaData including the values of PK parameters across Population Simulation sets
#' @export
getPkParametersAcrossPopulations <- function(structureSets) {
  pkParametersTableAcrossPopulations <- NULL
  for (structureSet in structureSets)
  {
    simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)
    population <- loadWorkflowPopulation(structureSet$simulationSet)

    pkAnalyses <- ospsuite::importPKAnalysesFromCSV(
      structureSet$pkAnalysisResultsFileNames,
      simulation
    )

    pkParametersTable <- ospsuite::pkAnalysesAsDataFrame(pkAnalyses)
    populationTable <- getPopulationAsDataFrame(population, simulation)

    # Use merge instead of cbind as there is a same variable IndividualId
    fullPkParametersTable <- merge.data.frame(
      pkParametersTable,
      populationTable
    )
    fullPkParametersTable <- cbind.data.frame(
      simulationSetName = structureSet$simulationSet$simulationSetName,
      fullPkParametersTable
    )
    # Prevent crash when merging populations with different parameters by filling unexisting with NA
    newNamesPkParametersTableAcrossPopulations <- setdiff(names(fullPkParametersTable), names(pkParametersTableAcrossPopulations))
    newNamesPkParametersTable <- setdiff(names(pkParametersTableAcrossPopulations), names(fullPkParametersTable))
    if (!is.null(pkParametersTableAcrossPopulations)) {
      pkParametersTableAcrossPopulations[, newNamesPkParametersTableAcrossPopulations] <- NA
    }
    fullPkParametersTable[, newNamesPkParametersTable] <- NA

    pkParametersTableAcrossPopulations <- rbind.data.frame(
      pkParametersTableAcrossPopulations,
      fullPkParametersTable
    )
  }
  metaData <- getPopulationMetaData(population, simulation, structureSet$parameterDisplayPaths)

  return(list(
    data = pkParametersTableAcrossPopulations,
    metaData = metaData
  ))
}

getPkRatiosTable <- function(pkParametersTable,
                             referencePopulationName) {
  populationNames <- pkParametersTable$Population

  pkRatiosTable <- pkParametersTable[populationNames != referencePopulationName, ]
  referencePkParametersTable <- pkParametersTable[rep(referencePopulationName, length(pkRatiosTable$Population)), ]

  pkRatiosTable[, seq(3, ncol(pkRatiosTable))] <- pkRatiosTable[, seq(3, ncol(pkRatiosTable))] / referencePkParametersTable[, seq(3, ncol(pkRatiosTable))]

  return(pkRatiosTable)
}

#' @title getDefaultPkParametersXParameters
#' @description Get names of default parameters in x axis of pk parameters plots.
#' @param workflowType Name of workflow type.
#' Use enum `PopulationWorkflowTypes` to get a list of available workflow types.
#' @return names of default parameters
getDefaultPkParametersXParameters <- function(workflowType) {
  validateIsIncluded(workflowType, PopulationWorkflowTypes)
  if (workflowType %in% PopulationWorkflowTypes$pediatric) {
    return(DemographyDefaultParameters)
  }
  return(NULL)
}

#' @title getPopulationPkAnalysesFromOuptut
#' @description Get the values of PK parameters specified by an `Output` object from a data.frame
#' @param data data.frame of the PK Analyses across Population Simulation sets
#' @param metaData metaData (dimension and unit) of the PK Analyses across Population Simulation sets
#' @param output `Output ` object
#' @param pkParameter `pkParameter` from `Output ` object
#' @param molWeight Molecular weight of compound (if unit conversion needed)
#' @return list of data.frame and its metaData including the values of PK parameters specified by `pkParameter` and `Output` objects
#' @export
getPopulationPkAnalysesFromOuptut <- function(data, metaData, output, pkParameter, molWeight = NULL) {
  validateIsIncluded(output$path, unique(data$QuantityPath))
  outputData <- data[data$QuantityPath %in% output$path, ]

  displayName <- pkParameter$displayName
  displayUnit <- pkParameter$displayUnit

  validateIsIncluded(pkParameter$pkParameter, unique(outputData$Parameter))
  selectedParameter <- outputData$Parameter %in% pkParameter$pkParameter
  pkParameterObject <- ospsuite::pkParameterByName(pkParameter$pkParameter)

  # Need to switch back to base unit first if a display unit is provided
  pkParameterValue <- outputData$Value[selectedParameter]

  if (!is.null(displayUnit)) {
    pkParameterValueInBaseUnit <- ospsuite::toBaseUnit(
      pkParameterObject$dimension,
      outputData$Value[selectedParameter],
      pkParameterObject$displayUnit,
      molWeight
    )
    pkParameterValue <- ospsuite::toUnit(
      pkParameterObject$dimension,
      pkParameterValueInBaseUnit,
      displayUnit,
      molWeight
    )
  }

  pkAnalysesFromOuptut <- outputData[selectedParameter, ]
  pkAnalysesFromOuptut$Value <- pkParameterValue

  pkAnalysesFromOuptutMetaData <- metaData
  pkAnalysesFromOuptutMetaData$Value <- list(
    dimension = displayName %||% pkAnalysesFromOuptut$Parameter[1],
    unit = displayUnit %||% pkAnalysesFromOuptut$Unit[1]
  )

  return(list(
    data = pkAnalysesFromOuptut,
    metaData = pkAnalysesFromOuptutMetaData
  ))
}

#' @title getXParametersForPkParametersPlot
#' @param workflow `PopulationWorkflow` R6 class object
#' @return list of x-parameters used for PK parameters range plots
#' @export
getXParametersForPkParametersPlot <- function(workflow) {
  validateIsOfType(workflow, "PopulationWorkflow")
  return(workflow$plotPKParameters$xParameters)
}

#' @title getYParametersForPkParametersPlot
#' @param workflow `PopulationWorkflow` R6 class object
#' @return list of y-parameters used for PK parameters range plots and boxplots
#' @export
getYParametersForPkParametersPlot <- function(workflow) {
  validateIsOfType(workflow, "PopulationWorkflow")
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
  validateIsOfType(workflow, "PopulationWorkflow")
  validateIsString(c(parameters))

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
  validateIsOfType(workflow, "PopulationWorkflow")
  validateIsOfType(c(parameters), "Output")

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
