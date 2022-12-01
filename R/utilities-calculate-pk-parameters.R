#' @title calculatePKParameters
#' @description Calculate PK parameters from simulated time profiles
#' @param structureSet `SimulationStructure` R6 class object contain paths of files to be used
#' @param settings list of options to be passed on the function
#' @return pkAnalysis object
#' @import ospsuite
#' @keywords internal
calculatePKParameters <- function(structureSet, settings = NULL) {
  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$simulationFile)
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)

  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationResultFileNames)
  simulationResults <- ospsuite::importResultsFromCSV(
    simulation = simulation,
    filePaths = structureSet$simulationResultFileNames
  )

  logDebug(paste0("Simulation results '", structureSet$simulationResultFileNames, "' successfully loaded"))

  pkAnalyses <- calculatePKAnalyses(results = simulationResults)
  logDebug("Calculation of PK parameters complete")
  return(pkAnalyses)
}

#' @title plotMeanPKParameters
#' @description Plot PK parameters table
#' @param structureSet `SimulationStructure` R6 class object
#' @param settings list of settings for the output table/plot
#' @return data.frame with calculated PK parameters for the simulation set
#' @import ospsuite
#' @keywords internal
plotMeanPKParameters <- function(structureSet, settings = NULL) {
  pkParametersData <- NULL
  pkParametersResults <- list()

  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$simulationFile)
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet, loadFromCache = TRUE)

  re.tStoreFileMetadata(access = "read", filePath = structureSet$pkAnalysisResultsFileNames)
  pkAnalyses <- ospsuite::importPKAnalysesFromCSV(
    structureSet$pkAnalysisResultsFileNames,
    simulation
  )

  pkParametersTable <- ospsuite::pkAnalysesToDataFrame(pkAnalyses)
  for (output in structureSet$simulationSet$outputs) {
    molWeight <- simulation$molWeightFor(output$path)

    pkParametersData <- rbind.data.frame(
      pkParametersData,
      getMeanPKAnalysesFromOutput(pkParametersTable, output, molWeight)
    )
  }
  pkParametersData$Value <- replaceInfWithNA(pkParametersData$Value)

  tableID <- defaultFileNames$resultID("pk_parameters", structureSet$simulationSet$simulationSetName)
  pkParametersResults[[tableID]] <- saveTaskResults(
    id = tableID,
    table = pkParametersData,
    tableCaption = captions$plotPKParameters$mean(
      structureSet$simulationSet$simulationSetName,
      structureSet$simulationSetDescriptor
    ),
    includeTable = TRUE
  )

  return(pkParametersResults)
}

#' @title getMeanPKAnalysesFromOutput
#' @description Get PK analyses from an `Output` object
#' @param data A data.frame of PK Analyses
#' @param output An `Output` object defining `pkParameters`
#' @param molWeight Molecular weight for converting into PK Parameter `displayUnit`
#' @return A data.frame with `Path`, `Parameter`, `Value` and `Unit` to display in final report
#' @import ospsuite
#' @importFrom ospsuite.utils %||%
#' @keywords internal
getMeanPKAnalysesFromOutput <- function(data, output, molWeight = NULL) {
  pkAnalysesFromOutput <- NULL
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
#' @param settings list of settings for the output table/plot
#' @param workflowType workflowType Type of population workflow.
#' Use enum `PopulationWorkflowTypes` to get list of workflow types.
#' @param xParameters list of parameters to be plotted along x axis
#' @param yParameters list of parameters to be plotted along y axis
#' @return list of plots and tables with summary of PK parameters
#' @import ospsuite
#' @import tlf
#' @import ggplot2
#' @importFrom ospsuite.utils %||%
#' @keywords internal
plotPopulationPKParameters <- function(structureSets,
                                       settings = NULL,
                                       workflowType = PopulationWorkflowTypes$parallelComparison,
                                       xParameters = getDefaultPkParametersXParameters(workflowType),
                                       yParameters = NULL) {
  validateIsIncluded(workflowType, PopulationWorkflowTypes)
  validateIsOfType(structureSets, "list")
  validateIsOfType(c(structureSets), "SimulationStructure")
  validateIsString(c(xParameters), nullAllowed = TRUE)
  validateIsOfType(c(yParameters), "Output", nullAllowed = TRUE)
  validateSameOutputsBetweenSets(
    c(lapply(structureSets, function(set) {
      set$simulationSet
    }))
  )

  # Use first structure set as reference
  yParameters <- yParameters %||% structureSets[[1]]$simulationSet$outputs
  # Get first simulation, in case mol weight is needed
  simulation <- loadSimulationWithUpdatedPaths(structureSets[[1]]$simulationSet, loadFromCache = TRUE)
  simulationSetDescriptor <- structureSets[[1]]$simulationSetDescriptor

  pkRatioTableAcrossPopulations <- NULL
  pkParametersResults <- list()

  pkParametersMapping <- tlf::BoxWhiskerDataMapping$new(
    x = "simulationSetName",
    y = "Value"
  )

  # Get and format PK data
  pkParametersAcrossPopulations <- getPKParametersAcrossPopulations(structureSets)
  pkParametersDataAcrossPopulations <- pkParametersAcrossPopulations$data
  pkParametersMetaDataAcrossPopulations <- pkParametersAcrossPopulations$metaData
  simulationSetNames <- unique(as.character(pkParametersDataAcrossPopulations$simulationSetName))

  # Warn if some PK parameters were not exported by the simulation
  checkIsIncluded(xParameters, names(pkParametersDataAcrossPopulations), nullAllowed = TRUE, groupName = "PK parameters variable names across simulation sets")
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
    for (pkParameter in output$pkParameters) {
      resultID <- defaultFileNames$resultID(length(pkParametersResults) + 1, "pk_parameters", pkParameter$pkParameter)

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
      pkParameterData <- removeMissingValues(pkParameterData, "Value")
      if (nrow(pkParameterData) == 0) {
        logError(messages$warningPKParameterNotEnoughData(pkParameter$pkParameter, output$path))
        next
      }

      # Define default plot configuration with rotate x-ticklabels
      boxplotConfiguration <- tlf::BoxWhiskerPlotConfiguration$new(
        data = pkParameterData,
        metaData = pkParameterMetaData,
        dataMapping = pkParametersMapping
      )
      boxplotConfiguration$labels$xlabel$text <- NULL
      boxplotConfiguration$xAxis$font$angle <- 45
      boxplotConfiguration <- settings$plotConfigurations[["boxplotPkParameters"]] %||%
        boxplotConfiguration

      boxplotPkParameter <- tlf::plotBoxWhisker(
        data = pkParameterData,
        metaData = pkParameterMetaData,
        dataMapping = pkParametersMapping,
        plotConfiguration = boxplotConfiguration
      )

      parameterCaption <- pkParameterMetaData$Value$dimension

      # Report tables summarizing the distributions
      pkParameterTable <- getPKParameterMeasure(
        data = pkParameterData,
        dataMapping = pkParametersMapping
      )
      # A different table needs to be created here
      # because of ratio comparison of the table values
      savedPKParameterTable <- formatPKParameterHeader(
        pkParameterTable,
        simulationSetDescriptor
      )

      # Save results
      resultID <- defaultFileNames$resultID(length(pkParametersResults) + 1, "pk_parameters", pkParameter$pkParameter)
      pkParametersResults[[resultID]] <- saveTaskResults(
        id = resultID,
        plot = pkParameterBoxplot,
        plotCaption = captions$plotPKParameters$boxplot(
          parameterCaption,
          output$displayName,
          simulationSetNames,
          simulationSetDescriptor
        ),
        table = savedPKParameterTable,
        tableCaption = captions$plotPKParameters$summaryTable(
          parameterCaption,
          output$displayName,
          simulationSetNames,
          simulationSetDescriptor,
          pkParameterMetaData$Value$unit
        ),
        includeTable = TRUE
      )
      # Plot in log scale
      if (!hasPositiveValues(pkParameterData$Value)) {
        logError(messages$warningLogScaleNoPositiveData(paste0(pkParameter$pkParameter, " for ", output$path)))
      }
      if (hasPositiveValues(pkParameterData$Value)) {
        resultID <- defaultFileNames$resultID(length(pkParametersResults) + 1, "pk_parameters", pkParameter$pkParameter, "log")

        positiveValues <- pkParameterData$Value > 0
        boxRange <- autoAxesLimits(pkParameterData$Value[positiveValues], scale = "log")
        boxBreaks <- autoAxesTicksFromLimits(boxRange)

        pkParametersResults[[resultID]] <- saveTaskResults(
          id = resultID,
          plot = tlf::setYAxis(
            plotObject = boxplotPkParameter,
            scale = tlf::Scaling$log,
            limits = boxRange,
            ticks = boxBreaks
          ),
          plotCaption = captions$plotPKParameters$boxplot(
            parameterCaption,
            output$displayName,
            simulationSetNames,
            simulationSetDescriptor,
            plotScale = "logarithmic"
          )
        )
      }

      # Range plots on PK parameters vs xParameters
      for (demographyParameter in setdiff(xParameters, pkParameter$pkParameter)) {
        # xParameters that are characters are not plotted
        if (pkParametersMetaDataAcrossPopulations[[demographyParameter]]$class %in% "character") {
          next
        }
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

            xParameterCaption <- vpcMetaData$x$dimension
            yParameterCaption <- vpcMetaData$median$dimension

            # Save comparison vpc results
            resultID <- defaultFileNames$resultID(length(pkParametersResults) + 1, "pk_parameters", demographyParameter, pkParameter$pkParameter)
            pkParametersResults[[resultID]] <- saveTaskResults(
              id = resultID,
              plot = comparisonVpcPlot,
              plotCaption = captions$plotPKParameters$rangePlot(
                xParameterCaption,
                yParameterCaption,
                simulationSetName,
                simulationSetDescriptor,
                referenceSetName = referenceSimulationSetName
              )
            )

            resultID <- defaultFileNames$resultID(length(pkParametersResults) + 1, "pk_parameters", demographyParameter, pkParameter$pkParameter, "log")
            pkParametersResults[[resultID]] <- saveTaskResults(
              id = resultID,
              plot = tlf::setYAxis(
                plotObject = comparisonVpcPlot,
                scale = tlf::Scaling$log,
                limits = boxRange,
                ticks = boxBreaks
              ),
              plotCaption = captions$plotPKParameters$rangePlot(
                xParameterCaption,
                yParameterCaption,
                simulationSetName,
                simulationSetDescriptor,
                referenceSetName = referenceSimulationSetName,
                plotScale = "logarithmic"
              )
            )
          }
        }

        # Regular range plots not associated to workflow type
        for (simulationSetName in simulationSetNames) {
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
          xParameterCaption <- vpcMetaData$x$dimension
          yParameterCaption <- vpcMetaData$median$dimension

          resultID <- defaultFileNames$resultID(length(pkParametersResults) + 1, "pk_parameters", demographyParameter, pkParameter$pkParameter)
          pkParametersResults[[resultID]] <- saveTaskResults(
            id = resultID,
            plot = vpcPlot,
            plotCaption = captions$plotPKParameters$rangePlot(
              xParameterCaption,
              yParameterCaption,
              simulationSetName,
              simulationSetDescriptor
            )
          )

          resultID <- defaultFileNames$resultID(length(pkParametersResults) + 1, "pk_parameters", demographyParameter, pkParameter$pkParameter, "log")
          pkParametersResults[[resultID]] <- saveTaskResults(
            id = resultID,
            plot = tlf::setYAxis(
              plotObject = vpcPlot,
              scale = tlf::Scaling$log,
              limits = boxRange,
              ticks = boxBreaks
            ),
            plotCaption = captions$plotPKParameters$rangePlot(
              xParameterCaption,
              yParameterCaption,
              simulationSetName,
              simulationSetDescriptor,
              plotScale = "logarithmic"
            )
          )
        }
      }

      # For Ratio Comparison create boxplots of boxplot hinges ratios
      # TODO: issue #536 change method for calculation of PK Ratio
      if (!isIncluded(workflowType, PopulationWorkflowTypes$ratioComparison)) {
        next
      }
      # Get the tables and compute the ratios using reference population name
      pkRatiosTable <- getPKRatiosTable(pkParameterTable, referenceSimulationSetName)

      pkRatiosData <- pkRatiosTable
      pkRatiosData[, c("ymin", "lower", "middle", "upper", "ymax")] <- pkRatiosTable[, c(3:7)]
      pkRatiosTable <- formatPKParameterHeader(
        pkRatiosTable,
        simulationSetDescriptor
      )

        ratioPlotConfiguration <- tlf::BoxWhiskerPlotConfiguration$new(
          ylabel = paste0(
            pkParameterMetaData$Value$dimension,
            " [fraction of ", referenceSimulationSetName, "]"
          )
        )
        ratioPlotConfiguration$xAxis$font$angle <- 45
        ratioPlotConfiguration <- settings$plotConfigurations[["boxplotPkRatios"]] %||%
          ratioPlotConfiguration

        boxplotPkRatios <- ratioBoxplot(
          data = pkRatiosData,
          plotConfiguration = ratioPlotConfiguration
        )

        parameterCaption <- pkParameterMetaData$Value$dimension

        # Save PK Ratio results
        resultID <- defaultFileNames$resultID(length(pkParametersResults) + 1, "pk_ratios", pkParameter$pkParameter)
        pkParametersResults[[resultID]] <- saveTaskResults(
          id = resultID,
          plot = boxplotPkRatios,
          plotCaption = captions$plotPKParameters$ratioPlot(
            parameterCaption,
            output$displayName,
            setdiff(simulationSetNames, referenceSimulationSetName),
            simulationSetDescriptor,
            referenceSetName = referenceSimulationSetName
          ),
          table = pkRatiosTable,
          tableCaption = captions$plotPKParameters$ratioTable(
            parameterCaption,
            output$displayName,
            setdiff(simulationSetNames, referenceSimulationSetName),
            simulationSetDescriptor,
            referenceSetName = referenceSimulationSetName
          ),
          includeTable = TRUE
        )

        ratioRange <- autoAxesLimits(c(pkRatiosData$ymin, pkRatiosData$ymax), scale = "log")
        ratioBreaks <- autoAxesTicksFromLimits(ratioRange)

        resultID <- defaultFileNames$resultID(length(pkParametersResults) + 1, "pk_ratios", pkParameter$pkParameter, "log")
        pkParametersResults[[resultID]] <- saveTaskResults(
          id = resultID,
          plot = tlf::setYAxis(
            plotObject = boxplotPkRatios,
            scale = tlf::Scaling$log,
            limits = ratioRange,
            ticks = ratioBreaks
          ),
          plotCaption = captions$plotPKParameters$ratioPlot(
            parameterCaption,
            output$displayName,
            setdiff(simulationSetNames, referenceSimulationSetName),
            simulationSetDescriptor,
            referenceSetName = referenceSimulationSetName,
            plotScale = "logarithmic"
          )
        )
    }
  }

  return(pkParametersResults)
}

#' @title ratioBoxplot
#' @description Plot box-whiskers of ratios as is
#' @param data data.frame of the ratios
#' @param plotConfiguration PlotConfiguration R6 class object
#' @return ggplot object
#' @export
#' @import tlf
#' @import ggplot2
ratioBoxplot <- function(data,
                         plotConfiguration = NULL) {
  # TODO: create new molecule plot for this
  ratioPlot <- tlf::initializePlot(plotConfiguration)
  aestheticValues <- tlf:::.getAestheticValuesFromConfiguration(
    n = 1,
    position = 0,
    plotConfigurationProperty = ratioPlot$plotConfiguration$ribbons,
    propertyNames = c("size", "alpha", "fill")
  )

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
      fill = aestheticValues$fill,
      alpha = aestheticValues$alpha,
      size = aestheticValues$size
    )
  ratioPlot <- tlf:::.updateAxes(ratioPlot)
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
#' @importFrom ospsuite.utils %||%
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
    pkParametersTable <- ospsuite::pkAnalysesToDataFrame(pkAnalyses)
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
  if (isOfLength(pkParametersTableAcrossPopulations, 0)) {
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

#' @title getDefaultPKParametersXParameters
#' @description Get names of default parameters in x axis of pk parameters plots.
#' @param workflowType Name of workflow type.
#' Use enum `PopulationWorkflowTypes` to get a list of available workflow types.
#' @return names of default parameters
#' @export
#' @examples
#'
#' getDefaultPKParametersXParameters(PopulationWorkflowTypes$pediatric)
#'
getDefaultPKParametersXParameters <- function(workflowType) {
  validateIsIncluded(workflowType, PopulationWorkflowTypes)
  if (workflowType %in% PopulationWorkflowTypes$pediatric) {
    return(DemographyDefaultParameters)
  }
  return(NULL)
}

#' @rdname getDefaultPKParametersXParameters
#' @export
getDefaultPkParametersXParameters <- getDefaultPKParametersXParameters

#' @title getPopulationPKAnalysesFromOutput
#' @description Get the values of PK parameters specified by an `Output` object from a data.frame
#' @param data data.frame of the PK Analyses across Population Simulation sets
#' @param metaData metaData (dimension and unit) of the PK Analyses across Population Simulation sets
#' @param output An `Output ` object
#' @param pkParameter `pkParameter` from `Output ` object
#' @param molWeight Molecular weight of compound (if unit conversion needed)
#' @return list of data.frame and its metaData including the values of PK parameters specified by `pkParameter` and `Output` objects
#' @importFrom ospsuite.utils %||%
#' @keywords internal
getPopulationPKAnalysesFromOutput <- function(data, metaData, output, pkParameter, molWeight = NULL) {
  validateIsIncluded(output$path, unique(data$QuantityPath))
  outputData <- data[data$QuantityPath %in% output$path, ]

  displayName <- pkParameter$displayName
  displayUnit <- pkParameter$displayUnit

  # Caution: now using group instead of pkParameter and Parameter
  validateIsIncluded(pkParameter$group, unique(outputData$group))
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

#' @title getXParametersForPKParametersPlot
#' @param workflow `PopulationWorkflow` R6 class object
#' @return list of x-parameters used for PK parameters range plots
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Get the list of parameters in x-axis for range plots
#' getXParametersForPKParametersPlot(workflow = myWorkflow)
#' }
#'
getXParametersForPKParametersPlot <- function(workflow) {
  validateIsOfType(workflow, "PopulationWorkflow")
  return(workflow$plotPKParameters$xParameters)
}

#' @title getYParametersForPKParametersPlot
#' @param workflow `PopulationWorkflow` R6 class object
#' @return list of y-parameters used for PK parameters range plots and boxplots
#' @export
#' @import ospsuite.utils
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Get the list of parameters in y-axis for range plots and boxplots
#' getYParametersForPKParametersPlot(workflow = myWorkflow)
#' }
#'
getYParametersForPKParametersPlot <- function(workflow) {
  validateIsOfType(workflow, "PopulationWorkflow")
  yParameters <- workflow$plotPKParameters$yParameters %||% workflow$simulationStructures[[1]]$simulationSet$outputs

  return(yParameters)
}

#' @title setXParametersForPKParametersPlot
#' @description Set x-parameters for range plots of PK parameters plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of parameters to be used as x-parameters
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Set parameters in x-axis for range plots
#' setXParametersForPKParametersPlot(
#'   workflow = myWorkflow,
#'   parameters = StandardPath
#' )
#' }
#'
setXParametersForPKParametersPlot <- function(workflow, parameters) {
  validateIsOfType(workflow, "PopulationWorkflow")
  validateIsString(c(parameters))

  workflow$plotPKParameters$xParameters <- parameters

  logDebug(paste0(
    "X-parameters: '", paste0(c(parameters), collapse = "', '"),
    "' set for PK parameters plot."
  ))
  return(invisible())
}

#' @title addXParametersForPKParametersPlot
#' @description Append x-parameters for range plots of PK parameters plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of parameters to be used as x-parameters
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Add parameters in x-axis for range plots
#' addXParametersForPKParametersPlot(
#'   workflow = myWorkflow,
#'   parameters = StandardPath$GestationalAge
#' )
#' }
#'
addXParametersForPKParametersPlot <- function(workflow, parameters) {
  updatedParameters <- c(getXParametersForPKParametersPlot(workflow), parameters)
  setXParametersForPkParametersPlot(workflow, updatedParameters)
}

#' @title setYParametersForPKParametersPlot
#' @description Set y-parameters for boxplots and range plots of PK parameters plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of R6 class `Output` objects to be used as y-parameters
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Set parameters in y-axis for range plots and boxplots
#' setYParametersForPKParametersPlot(
#'   workflow = myWorkflow,
#'   parameters = Output$new(path, pkParameters)
#' )
#' }
#'
setYParametersForPKParametersPlot <- function(workflow, parameters) {
  validateIsOfType(workflow, "PopulationWorkflow")
  validateIsOfType(c(parameters), "Output")

  workflow$plotPKParameters$yParameters <- parameters

  for (output in c(parameters)) {
    logDebug(paste0(
      "Y-parameters: '", paste0(c(output$pkParameters), collapse = "', '"),
      "' for '", output$path, "' set for PK parameters plot."
    ))
  }
  return(invisible())
}

#' @title addYParametersForPKParametersPlot
#' @description Append y-parameters for range plots of PK parameters plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of parameters to be used as y-parameters
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Add parameters in y-axis for range plots and boxplots
#' addYParametersForPKParametersPlot(
#'   workflow = myWorkflow,
#'   parameters = Output$new(path, pkParameters)
#' )
#' }
#'
addYParametersForPKParametersPlot <- function(workflow, parameters) {
  updatedParameters <- c(getYParametersForPkParametersPlot(workflow), parameters)
  setYParametersForPkParametersPlot(workflow, updatedParameters)
}

#' @rdname getXParametersForPKParametersPlot
#' @export
getXParametersForPkParametersPlot <- getXParametersForPKParametersPlot

#' @rdname getYParametersForPKParametersPlot
#' @export
getYParametersForPkParametersPlot <- getYParametersForPKParametersPlot

#' @rdname setXParametersForPKParametersPlot
#' @export
setXParametersForPkParametersPlot <- setXParametersForPKParametersPlot

#' @rdname setYParametersForPKParametersPlot
#' @export
setYParametersForPkParametersPlot <- setYParametersForPKParametersPlot

#' @rdname addXParametersForPKParametersPlot
#' @export
addXParametersForPkParametersPlot <- addXParametersForPKParametersPlot

#' @rdname addYParametersForPKParametersPlot
#' @export
addYParametersForPkParametersPlot <- addYParametersForPKParametersPlot
