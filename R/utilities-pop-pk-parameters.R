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
#' @import ospsuite.utils
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
    y = "Value",
    fill = "simulationSetName"
  )
  # Get and format PK data
  pkParametersAcrossPopulations <- getPKParametersAcrossPopulations(structureSets)
  pkParametersDataAcrossPopulations <- pkParametersAcrossPopulations$data
  pkParametersMetaDataAcrossPopulations <- pkParametersAcrossPopulations$metaData
  simulationSetNames <- unique(as.character(pkParametersDataAcrossPopulations$simulationSetName))
  referenceSimulationSetName <- getReferencePopulationName(structureSets)


  # Warn if some PK parameters were not exported by the simulation
  checkIsIncluded(xParameters, names(pkParametersDataAcrossPopulations), nullAllowed = TRUE, groupName = "PK parameters variable names across simulation sets")
  xParameters <- intersect(xParameters, names(pkParametersDataAcrossPopulations))

  # Enforce factors for population names with order as input by the user
  pkParametersDataAcrossPopulations$simulationSetName <- factor(
    pkParametersDataAcrossPopulations$simulationSetName,
    levels = unique(pkParametersDataAcrossPopulations$simulationSetName)
  )

  #---- Regular summary tables and boxplots ----
  # PK Parameters are calculated per output
  # Results are calculated within this nested loop
  for (output in yParameters) {
    # Report heading for Output path (in case output use same paths, render a unique id)
    resultID <- defaultFileNames$resultID(
      length(pkParametersResults) + 1,
      "pk-parameters",
      removeForbiddenLetters(output$path)
    )
    pkParametersResults[[resultID]] <- saveTaskResults(
      id = resultID,
      textChunk = captions$plotPKParameters$outputSection(output$displayName, resultID),
      includeTextChunk = TRUE
    )
    # Keep and format only relevant data per parameter
    molWeight <- simulation$molWeightFor(output$path)
    # Maps colors to simulation sets
    # Every simulation set except reference will use fill value from output
    # If there is a reference population, its reference (grey) fill will be used
    colorGrouping <- getColorGroupForPKParameterPlot(
      output = output,
      referenceSetName = referenceSimulationSetName,
      simulationSetNames = simulationSetNames
    )
    for (pkParameter in output$pkParameters) {
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
      if (isEmpty(pkParameterData)) {
        logError(messages$warningPKParameterNotEnoughData(pkParameter$pkParameter, output$path))
        next
      }

      # Report heading for PK Parameter
      resultID <- defaultFileNames$resultID(
        length(pkParametersResults) + 1,
        "pk-parameters",
        removeForbiddenLetters(output$path),
        removeForbiddenLetters(pkParameter$pkParameter)
      )
      pkParametersResults[[resultID]] <- saveTaskResults(
        id = resultID,
        textChunk = captions$plotPKParameters$parameterSection(pkParameter$displayName, resultID),
        includeTextChunk = TRUE
      )

      # Define default plot configuration with rotate x-tick labels
      boxplotConfiguration <- getBoxWhiskerPlotConfiguration(
        plotScale = "lin",
        colorGrouping = colorGrouping,
        data = pkParameterData,
        metaData = pkParameterMetaData,
        dataMapping = pkParametersMapping,
        plotConfiguration = settings$plotConfigurations[["boxplotPKParameters"]]
      )
      pkParameterBoxplot <- tlf::plotBoxWhisker(
        data = pkParameterData,
        metaData = pkParameterMetaData,
        dataMapping = pkParametersMapping,
        plotConfiguration = boxplotConfiguration
      )
      parameterCaption <- pkParameterMetaData$Value$dimension

      # Check there is at least positive data for log plots
      if (!hasPositiveValues(pkParameterData$Value)) {
        logError(messages$warningLogScaleNoPositiveData(paste0(pkParameter$pkParameter, " for ", output$path)))
      } else {
        boxplotConfigurationLog <- getBoxWhiskerPlotConfiguration(
          plotScale = "log",
          colorGrouping = colorGrouping,
          data = pkParameterData[pkParameterData$Value > 0, ],
          metaData = pkParameterMetaData,
          dataMapping = pkParametersMapping,
          plotConfiguration = settings$plotConfigurations[["boxplotPKParametersLog"]]
        )
        pkParameterBoxplotLog <- tlf::plotBoxWhisker(
          data = pkParameterData[pkParameterData$Value > 0, ],
          metaData = pkParameterMetaData,
          dataMapping = pkParametersMapping,
          plotConfiguration = boxplotConfigurationLog
        )
        resultID <- defaultFileNames$resultID(length(pkParametersResults) + 1, "pk_parameters", pkParameter$pkParameter, "log")
        pkParametersResults[[resultID]] <- saveTaskResults(
          id = resultID,
          plot = pkParameterBoxplotLog,
          plotCaption = captions$plotPKParameters$boxplot(
            parameterCaption,
            output$displayName,
            simulationSetNames,
            simulationSetDescriptor,
            plotScale = "logarithmic"
          )
        )
      }

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

      #---- Range plots -----
      # Checks x-dependency of y parameter
      # Usually performed in pediatric workflows
      for (demographyParameter in setdiff(xParameters, pkParameter$pkParameter)) {
        # xParameters that are characters are not plotted
        if (pkParametersMetaDataAcrossPopulations[[demographyParameter]]$class %in% "character") {
          next
        }
        vpcMetaData <- list(
          "x" = pkParameterMetaData[[demographyParameter]],
          "median" = pkParameterMetaData$Value
        )
        # Include reference population if defined
        if (!isEmpty(referenceSimulationSetName)) {
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
            comparisonVpcPlot <- updateVpcPlotColor(
              plotObject = comparisonVpcPlot,
              output = output,
              referenceSimulationSetName = referenceSimulationSetName
            )
            xParameterCaption <- vpcMetaData$x$dimension
            yParameterCaption <- vpcMetaData$median$dimension

            # Check if log scaled vpc plot has positive data
            if (hasPositiveValues(comparisonData$ymin)) {
              vpcDataForLimits <- c(
                comparisonData$ymin, comparisonData$median, comparisonData$ymax,
                referenceData$ymin, referenceData$median, referenceData$ymax
              )
              vpcLimits <- autoAxesLimits(vpcDataForLimits[vpcDataForLimits > 0], scale = tlf::Scaling$log)

              comparisonVpcPlotLog <- tlf::setYAxis(
                plotObject = comparisonVpcPlot,
                scale = tlf::Scaling$log,
                axisLimits = vpcLimits,
                ticks = autoAxesTicksFromLimits(vpcLimits)
              )
              resultID <- defaultFileNames$resultID(length(pkParametersResults) + 1, "pk_parameters", demographyParameter, pkParameter$pkParameter, "log")
              pkParametersResults[[resultID]] <- saveTaskResults(
                id = resultID,
                plot = comparisonVpcPlotLog,
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
          vpcPlot <- updateVpcPlotColor(
            plotObject = vpcPlot,
            output = output
          )
          xParameterCaption <- vpcMetaData$x$dimension
          yParameterCaption <- vpcMetaData$median$dimension

          # Check if log scaled boxplot was created before plotting logs of range plots
          if (hasPositiveValues(vpcData$ymin)) {
            vpcDataForLimits <- c(vpcData$ymin, vpcData$median, vpcData$ymax)
            vpcLimits <- autoAxesLimits(vpcDataForLimits[vpcDataForLimits > 0], scale = tlf::Scaling$log)

            vpcPlotLog <- tlf::setYAxis(
              plotObject = vpcPlot,
              scale = tlf::Scaling$log,
              axisLimits = vpcLimits,
              ticks = autoAxesTicksFromLimits(vpcLimits)
            )
            resultID <- defaultFileNames$resultID(length(pkParametersResults) + 1, "pk_parameters", demographyParameter, pkParameter$pkParameter, "log")
            pkParametersResults[[resultID]] <- saveTaskResults(
              id = resultID,
              plot = vpcPlotLog,
              plotCaption = captions$plotPKParameters$rangePlot(
                xParameterCaption,
                yParameterCaption,
                simulationSetName,
                simulationSetDescriptor,
                plotScale = "logarithmic"
              )
            )
          }

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
        }
      }

      #---- Ratio comparisons -----
      # Checks ratios of statistics and parameters
      # Table is output for every workflow except parallel no reference
      if (isEmpty(referenceSimulationSetName)) {
        next
      }
      # Output table of relative change in the respective statistics
      # From issue #536, no plot is required as artifact here
      pkParameterVsRefData <- pkParameterTableAsRelativeChange(pkParameterTable, referenceSimulationSetName)
      pkParameterVsRefTable <- formatPKParameterHeader(pkParameterVsRefData, simulationSetDescriptor)
      names(pkParameterVsRefData)[1:7] <- c("simulationSetName", "N", "ymin", "lower", "middle", "upper", "ymax")

      ratioPlotConfiguration <- getBoxWhiskerPlotConfiguration(
        plotScale = "lin",
        colorGrouping = colorGrouping,
        # Ensure auto-scaling of boxplot is appropriate
        data = data.frame(
          simulationSetName = rep(pkParameterVsRefData$simulationSetName, 2),
          Value = c(pkParameterVsRefData$ymin, pkParameterVsRefData$ymax)
        ),
        # Create appropriate ylabel
        metaData = list(Value = list(
          dimension = pkParameterMetaData$Value$dimension,
          unit = paste0("fraction of ", referenceSimulationSetName)
        )),
        dataMapping = pkParametersMapping,
        plotConfiguration = settings$plotConfigurations[["boxplotPKRatios"]]
      )

      boxplotPKRatios <- ratioBoxplot(
        data = pkParameterVsRefData,
        plotConfiguration = ratioPlotConfiguration
      )
      parameterCaption <- pkParameterMetaData$Value$dimension

      # Check if log scaled boxplot was created before plotting logs of PK Ratio
      if (hasPositiveValues(pkParameterVsRefData$ymin)) {
        ratioRange <- autoAxesLimits(c(pkParameterVsRefData$ymin, pkParameterVsRefData$median, pkParameterVsRefData$ymax), scale = "log")
        ratioBreaks <- autoAxesTicksFromLimits(ratioRange)
        boxplotPKRatiosLog <- tlf::setYAxis(
          plotObject = boxplotPKRatios,
          scale = tlf::Scaling$log,
          axisLimits = ratioRange,
          ticks = ratioBreaks
        )
        resultID <- defaultFileNames$resultID(length(pkParametersResults) + 1, "pk_parameters", pkParameter$pkParameter, "log")
        pkParametersResults[[resultID]] <- saveTaskResults(
          id = resultID,
          plot = boxplotPKRatiosLog,
          plotCaption = captions$plotPKParameters$relativeChangePlot(
            parameterCaption,
            output$displayName,
            setdiff(simulationSetNames, referenceSimulationSetName),
            simulationSetDescriptor,
            referenceSetName = referenceSimulationSetName,
            plotScale = "logarithmic"
          )
        )
      }

      # Save Ratio of summary stats results
      resultID <- defaultFileNames$resultID(length(pkParametersResults) + 1, "pk_parameters", pkParameter$pkParameter)
      pkParametersResults[[resultID]] <- saveTaskResults(
        id = resultID,
        plot = boxplotPKRatios,
        plotCaption = captions$plotPKParameters$relativeChangePlot(
          parameterCaption,
          output$displayName,
          setdiff(simulationSetNames, referenceSimulationSetName),
          simulationSetDescriptor,
          referenceSetName = referenceSimulationSetName
        ),
        table = pkParameterVsRefTable,
        tableCaption = captions$plotPKParameters$relativeChangeTable(
          parameterCaption,
          output$displayName,
          setdiff(simulationSetNames, referenceSimulationSetName),
          simulationSetDescriptor,
          referenceSetName = referenceSimulationSetName
        ),
        includeTable = TRUE
      )

      # For ratio comparison workflows, compare PK parameter ratios
      # Method will depend on the populations as indicated in issue #536
      if (!isIncluded(workflowType, PopulationWorkflowTypes$ratioComparison)) {
        next
      }
      # To fix issue #1086, now ratio summary statistics are calculated from the calculatePKParameters task
      # and directly available from the structureSet objects

      pkRatiosTable <- getPKParameterRatioTable(
        pkParameter = pkParameter$pkParameter,
        outputPath = output$path,
        structureSets = structureSets
      )
      pkRatiosData <- pkRatiosTable
      names(pkRatiosData)[1:7] <- c("simulationSetName", "N", "ymin", "lower", "middle", "upper", "ymax")
      pkRatiosTable <- formatPKParameterHeader(pkRatiosTable, simulationSetDescriptor)

      ratioPlotConfiguration <- getBoxWhiskerPlotConfiguration(
        plotScale = "lin",
        colorGrouping = colorGrouping,
        # Ensure auto-scaling of boxplot is appropriate
        data = data.frame(
          simulationSetName = rep(pkRatiosData$simulationSetName, 2),
          Value = c(pkRatiosData$ymin, pkRatiosData$ymax)
        ),
        # Create appropriate ylabel
        metaData = list(Value = list(
          dimension = pkParameterMetaData$Value$dimension,
          unit = paste0("fraction of ", referenceSimulationSetName)
        )),
        dataMapping = pkParametersMapping,
        plotConfiguration = settings$plotConfigurations[["boxplotPKRatios"]]
      )

      boxplotPKRatios <- ratioBoxplot(
        data = pkRatiosData,
        plotConfiguration = ratioPlotConfiguration
      )
      parameterCaption <- pkParameterMetaData$Value$dimension

      # Check if log scaled boxplot was created before plotting logs of PK Ratio
      if (hasPositiveValues(pkRatiosData$ymin)) {
        ratioRange <- autoAxesLimits(c(pkRatiosData$ymin, pkRatiosData$median, pkRatiosData$ymax), scale = "log")
        ratioBreaks <- autoAxesTicksFromLimits(ratioRange)
        boxplotPKRatiosLog <- tlf::setYAxis(
          plotObject = boxplotPKRatios,
          scale = tlf::Scaling$log,
          axisLimits = ratioRange,
          ticks = ratioBreaks
        )
        resultID <- defaultFileNames$resultID(length(pkParametersResults) + 1, "pk_ratios", pkParameter$pkParameter, "log")
        pkParametersResults[[resultID]] <- saveTaskResults(
          id = resultID,
          plot = boxplotPKRatiosLog,
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

      # Save PK Ratio results
      resultID <- defaultFileNames$resultID(length(pkParametersResults) + 1, "pk_ratios", pkParameter$pkParameter)
      pkParametersResults[[resultID]] <- saveTaskResults(
        id = resultID,
        plot = boxplotPKRatios,
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
    }
  }
  return(pkParametersResults)
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
#' @import ospsuite.utils
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
    populationTable <- getPopulationPKData(population, simulation)

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
  metaData <- getPopulationPKMetaData(population, simulation, structureSet$parameterDisplayPaths)

  return(list(
    data = pkParametersTableAcrossPopulations,
    metaData = metaData
  ))
}

#' @title getPKParameterMeasure
#' @description Get table summarizing the PK Parameter distribution by population
#' @param data A data.frame of PK Parameter values across Population Simulation sets
#' @param dataMapping A `BoxWhiskerDataMapping` object
#' @return A data.frame summarizing the PK Parameter distribution by population
#' @import tlf
#' @keywords internal
getPKParameterMeasure <- function(data, dataMapping) {
  # Report tables summarizing the distributions
  pkParameterTable <- tlf::getBoxWhiskerMeasure(
    data = data,
    dataMapping = dataMapping
  )
  # Row names are added as factor to data.frames by default
  # This line ensures that the order of the rows is kept for the tables and plots
  pkParameterTableRows <- factor(
    row.names(pkParameterTable),
    levels = row.names(pkParameterTable)
  )
  pkParameterTable <- cbind(
    Population = pkParameterTableRows,
    pkParameterTable
  )
  return(pkParameterTable)
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
