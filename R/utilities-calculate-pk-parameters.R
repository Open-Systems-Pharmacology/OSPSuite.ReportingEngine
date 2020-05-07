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
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)
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
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)
  pkAnalyses <- ospsuite::importPKAnalysesFromCSV(
    structureSet$pkAnalysisResultsFileNames,
    simulation
  )

  pkParametersTable <- ospsuite::pkAnalysesAsDataFrame(pkAnalyses)
  # pkParametersTable <- pkParametersTable[, c("QuantityPath", "Parameters", "Value", "Unit")]

  return(list(
    plots = NULL,
    tables = list(pkAnalysis = pkParametersTable)
  ))
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
                                       xParameters = NULL,
                                       yParameters = NULL) {
  validateIsIncluded(workflowType, PopulationWorkflowTypes)
  validateIsOfType(structureSets, "list")
  validateIsOfType(c(structureSets), "SimulationStructure")
  validateIsString(xParameters, nullAllowed = TRUE)
  validateIsString(yParameters, nullAllowed = TRUE)

  pkRatioTableAcrossPopulations <- NULL
  pkParametersPlots <- NULL
  pkParametersTables <- NULL

  pkParametersMapping <- tlf::BoxWhiskerDataMapping$new(
    x = "simulationSetName",
    y = "Value"
  )

  pkParametersTableAcrossPopulations <- getPkParametersTableAcrossPopulations(structureSets)
  
  if (workflowType %in% c(PopulationWorkflowTypes$ratioComparison, PopulationWorkflowTypes$pediatric)) {
    referencePopulationName <- getReferencePopulationName(structureSets)
  }

  # White list of selected yParameters
  pkParameterNames <- pkParametersTableAcrossPopulations$Parameter
  yParameters <- yParameters %||% levels(factor(pkParameterNames))

  # Enforce factors for population names
  pkParametersTableAcrossPopulations$simulationSetName <- factor(pkParametersTableAcrossPopulations$simulationSetName)

  # Standard boxplots
  for (parameter in yParameters) {
    pkParameterData <- pkParametersTableAcrossPopulations[pkParameterNames %in% parameter, ]
    # remove NA value to prevent crash in computation of percentiles
    pkParameterData <- pkParameterData[!is.na(pkParameterData$Value), ]

    pkParameterMetaData <- list("Value" = list(
      dimension = parameter,
      unit = pkParameterData$Unit[1]
    ))

    # TO DO: standardize this approach for names of plots to export
    parameterLabel <- lastPathElement(parameter)

    boxplotPkParameters <- tlf::plotBoxWhisker(
      data = pkParameterData,
      metaData = pkParameterMetaData,
      dataMapping = pkParametersMapping,
      plotConfiguration = settings$plotConfigurations[["boxplotPkParameters"]]
    ) + ggplot2::labs(title = NULL, subtitle = NULL) + ggplot2::xlab(NULL)

    pkParametersPlots[[parameterLabel]] <- boxplotPkParameters
    pkParametersPlots[[paste0(parameterLabel, "-log")]] <- boxplotPkParameters +
      ggplot2::scale_y_continuous(trans = "log10")

    pkParametersTable <- tlf::getBoxWhiskerMeasure(
      data = pkParameterData,
      dataMapping = pkParametersMapping
    )
    pkParametersTableRows <- row.names(pkParametersTable)
    pkParametersTable <- cbind(
      Population = pkParametersTableRows,
      pkParametersTable
    )

    pkParametersTables[[parameterLabel]] <- pkParametersTable

    # Range plots on PK parameters vs xParameters
    for (demographyParameter in xParameters) {
      aggregatedData <- getDemographyAggregatedData(data = pkParameterData,
                                                    xParameterName = demographyParameter,
                                                    yParameterName = "Value",
                                                    xParameterBreaks = settings$xParametersBreaks[[demographyParameter]]
      )

      populationNames <- levels(factor(aggregatedData$Population))

      # For pediatric workflow, range plots compare reference population to the other populations
      if (workflowType %in% c(PopulationWorkflowTypes$pediatric)) {
        # Get the table for reference population
        pkParametersTable <- pkParametersTables[[parameterLabel]]
        referenceData <- data.frame(
          x = c(-Inf, Inf),
          "Population" = paste("Simulated", AggregationConfiguration$names$middle, AggregationConfiguration$names$range, "for", referencePopulationName)
        )
        referenceData[, c("ymin", "median", "ymax")] <- pkParametersTable[referencePopulationName, c(2, 4, 6)]

        # TO DO: integrate unit in the process
        xParameterName <- lastPathElement(demographyParameter)

        vpcMetaData <- list(
          "x" = list(
            dimension = xParameterName,
            unit = ""
          ),
          "median" = pkParameterMetaData$Value
        )

        referenceVpcPlot <- vpcParameterPlot(
          data = referenceData,
          metaData = vpcMetaData,
          plotConfiguration = settings$plotConfigurations[["comparisonVpcPlot"]]
        )

        for (populationName in populationNames[!populationNames %in% referencePopulationName]) {
          comparisonData <- aggregatedData[aggregatedData$Population %in% populationName, ]
          comparisonData$Population <- paste("Simulated", AggregationConfiguration$names$middle, AggregationConfiguration$names$range, "for", comparisonData$Population)
          comparisonVpcPlot <- vpcParameterPlot(
            data = comparisonData,
            metaData = vpcMetaData,
            plotObject = referenceVpcPlot
          )

          pkParametersPlots[[paste0(populationName, "-vs-ref-", parameterLabel, "-vs-", xParameterName)]] <- comparisonVpcPlot
          pkParametersPlots[[paste0(populationName, "-vs-ref-", parameterLabel, "-vs-", xParameterName, "-log")]] <- comparisonVpcPlot +
            ggplot2::scale_y_continuous(trans = "log10")
        }
      }

      # Regular range plots not associated to workflow type
      for (populationName in populationNames) {
        vpcData <- aggregatedData[aggregatedData$Population %in% populationName, ]
        vpcData$Population <- paste("Simulated", AggregationConfiguration$names$middle, AggregationConfiguration$names$range, "for", vpcData$Population)
        vpcPlot <- vpcParameterPlot(
          data = vpcData,
          metaData = vpcMetaData,
          plotConfiguration = settings$plotConfigurations[["vpcParameterPlot"]]
        )
        pkParametersPlots[[paste0(populationName, "-", parameterLabel, "-vs-", xParameterName)]] <- vpcPlot
        pkParametersPlots[[paste0(populationName, "-", parameterLabel, "-vs-", xParameterName, "-log")]] <- vpcPlot +
          ggplot2::scale_y_continuous(trans = "log10")
      }
    }
  }

  # For Ratio Comparison create boxplots of boxplot hinges ratios
  if (workflowType %in% PopulationWorkflowTypes$ratioComparison) {
    for (parameter in yParameters) {
      parameterLabel <- lastPathElement(parameter)

      pkParametersTable <- pkParametersTables[[parameterLabel]]

      # Get the tables and compute the ratios using reference population name
      pkRatiosTable <- getPkRatiosTable(pkParametersTable, referencePopulationName)

      pkRatiosData <- pkRatiosTable
      pkRatiosData[, c("ymin", "lower", "middle", "upper", "ymax")] <- pkRatiosTable[, c(3:7)]

      boxplotPkRatios <- ratioBoxplot(
        data = pkRatiosData,
        plotConfiguration = settings$plotConfigurations[["boxplotPkRatios"]]
      )
      boxplotPkRatios <- boxplotPkRatios + ggplot2::ylab(paste0(
        parameter,
        " [fraction of ",
        referencePopulationName, "]"
      ))

      pkParametersPlots[[paste0(parameterLabel, "-ratio")]] <- boxplotPkRatios
      pkParametersPlots[[paste0(parameterLabel, "-ratio-log")]] <- boxplotPkRatios +
        ggplot2::scale_y_continuous(trans = "log10")

      pkParametersTables[[paste0(parameterLabel, "-ratio")]] <- pkRatiosTable
    }
  }

  return(list(
    plots = pkParametersPlots,
    tables = pkParametersTables
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
  ratioPlot <- ratioPlot + ggplot2::labs(title = NULL, subtitle = NULL) + ggplot2::xlab(NULL)
  return(ratioPlot)
}

#' @title vpcParameterPlot
#' @description Plot vpc like plot of yParameter along xParameter
#' @param data data.frame
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
  vpcPlot <- vpcPlot +
    ggplot2::labs(title = NULL, subtitle = NULL)

  return(vpcPlot)
}

getPkParametersTableAcrossPopulations <- function(structureSets) {
  pkParametersTableAcrossPopulations <- NULL
  for (structureSet in structureSets)
  {
    simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)
    population <- ospsuite::loadPopulation(structureSet$simulationSet$populationFile)

    pkAnalyses <- ospsuite::importPKAnalysesFromCSV(
      structureSet$pkAnalysisResultsFileNames,
      simulation
    )

    pkParametersTable <- ospsuite::pkAnalysesAsDataFrame(pkAnalyses)
    populationTable <- ospsuite::populationAsDataFrame(population)

    # Use merge instead of cbind as there is a same variable IndividualId
    fullPkParametersTable <- merge.data.frame(
      pkParametersTable,
      populationTable
    )
    fullPkParametersTable <- cbind.data.frame(
      simulationSetName = structureSet$simulationSet$simulationSetName,
      fullPkParametersTable
    )
    pkParametersTableAcrossPopulations <- rbind.data.frame(
      pkParametersTableAcrossPopulations,
      fullPkParametersTable
    )
  }

  return(pkParametersTableAcrossPopulations)
}

getPkRatiosTable <- function(pkParametersTable,
                             referencePopulationName) {
  populationNames <- pkParametersTable$Population

  pkRatiosTable <- pkParametersTable[populationNames != referencePopulationName, ]
  referencePkParametersTable <- pkParametersTable[rep(referencePopulationName, length(pkRatiosTable$Population)), ]

  pkRatiosTable[, seq(3, ncol(pkRatiosTable))] <- pkRatiosTable[, seq(3, ncol(pkRatiosTable))] / referencePkParametersTable[, seq(3, ncol(pkRatiosTable))]

  return(pkRatiosTable)
}
