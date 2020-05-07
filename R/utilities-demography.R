#' @title plotDemographyParameters
#' @description Plot demography parameters box plots and tables
#' @param structureSets `SimulationStructure` R6 class object
#' @param logFolder folder where the logs are saved
#' @param settings list of settings for the output table/plot
#' @param workflowType workflowType Type of population workflow.
#' Use enum `PopulationWorkflowTypes` to get list of workflow types.
#' @param xParameters list of parameters to be plotted along x axis
#' @param yParameters list of parameters to be plotted along y axis
#' @return list of plots and tables with summary of demography parameters
#' @export
#' @import ospsuite
#' @import tlf
#' @import ggplot2
plotDemographyParameters <- function(structureSets,
                                     logFolder = getwd(),
                                     settings = NULL,
                                     workflowType = PopulationWorkflowTypes$parallelComparison,
                                     xParameters = getDefaultXParametersForWorkflowType(workflowType),
                                     yParameters = NULL) {
  validateIsIncluded(workflowType, PopulationWorkflowTypes)
  validateIsOfType(structureSets, "list")
  validateIsOfType(c(structureSets), "SimulationStructure")
  validateIsString(xParameters, nullAllowed = TRUE)
  validateIsString(yParameters, nullAllowed = TRUE)

  demographyPlot <- list()

  yParameters <- yParameters %||% getDefaultYParametersForWorkflowType(workflowType)

  demographyAcrossPopulations <- getDemographyAcrossPopulations(structureSets)

  if (workflowType %in% PopulationWorkflowTypes$pediatric) {
    referencePopulationName <- getReferencePopulationName(structureSets)
  }

  if (is.null(xParameters)) {
    # Pediatric: comparison histogram
    if (workflowType %in% c(PopulationWorkflowTypes$pediatric)) {
      for (parameterName in yParameters) {
        parameterLabel <- lastPathElement(parameterName)

        histogramMapping <- tlf::HistogramDataMapping$new(
          x = parameterName,
          fill = "simulationSetName"
        )

        demographyHistogram <- plotDemographyHistogram(
          data = demographyAcrossPopulations,
          bins = settings$bins %||% 11,
          dataMapping = histogramMapping,
          plotConfiguration = settings$plotConfigurations[["histogram"]]
        )
        demographyPlot[[parameterLabel]] <- demographyHistogram
      }
    }
    # Parallel and Ratio: histograms per population
    if (workflowType %in% c(PopulationWorkflowTypes$parallelComparison, PopulationWorkflowTypes$ratioComparison)) {
      for (parameterName in yParameters) {
        parameterLabel <- lastPathElement(parameterName)
        histogramMapping <- tlf::HistogramDataMapping$new(
          x = parameterName,
          fill = "simulationSetName"
        )
        for (populationName in levels(factor(demographyAcrossPopulations$simulationSetName))) {
          demographyData <- demographyAcrossPopulations[demographyAcrossPopulations$simulationSetName %in% populationName, ]

          demographyHistogram <- plotDemographyHistogram(
            data = demographyData,
            bins = settings$bins %||% 11,
            dataMapping = histogramMapping,
            plotConfiguration = settings$plotConfigurations[["histogram"]]
          )

          demographyPlot[[paste0(parameterLabel, "-", populationName)]] <- demographyHistogram
        }
      }
    }
    return(list(plots = demographyPlot))
  }

  for (demographyParameter in xParameters) {
    xParameterLabel <- lastPathElement(demographyParameter)
    for (parameterName in yParameters) {
      yParameterLabel <- lastPathElement(parameterName)

      vpcMetaData <- list(
        "x" = list(
          dimension = xParameterLabel,
          unit = ""
        ),
        "median" = list(
          dimension = yParameterLabel,
          unit = ""
        )
      )

      aggregatedData <- getDemographyAggregatedData(
        data = demographyAcrossPopulations,
        xParameterName = demographyParameter,
        yParameterName = parameterName,
        xParameterBreaks = settings$xParametersBreaks[[demographyParameter]]
      )
      populationNames <- levels(factor(aggregatedData$Population))

      # For pediatric workflow, range plots compare reference population to the other populations
      if (workflowType %in% c(PopulationWorkflowTypes$pediatric)) {
        referenceData <- demographyAcrossPopulations[demographyAcrossPopulations$simulationSetName %in% referencePopulationName, ]

        aggregatedReferenceData <- data.frame(
          x = c(-Inf, Inf),
          ymin = rep(AggregationConfiguration$functions$ymin(referenceData[, parameterName]), 2),
          median = rep(AggregationConfiguration$functions$middle(referenceData[, parameterName]), 2),
          ymax = rep(AggregationConfiguration$functions$ymax(referenceData[, parameterName]), 2),
          "Population" = paste("Simulated ", AggregationConfiguration$names$middle, AggregationConfiguration$names$range, "for", referencePopulationName)
        )

        referenceVpcPlot <- vpcParameterPlot(
          data = aggregatedReferenceData,
          metaData = vpcMetaData,
          plotConfiguration = settings$plotConfigurations[["comparisonVpcPlot"]]
        )

        for (populationName in populationNames[!populationNames %in% referencePopulationName]) {
          comparisonData <- aggregatedData[aggregatedData$Population %in% populationName, ]
          comparisonData$Population <- paste("Simulated ", AggregationConfiguration$names$middle, AggregationConfiguration$names$range, "for", comparisonData$Population)

          comparisonVpcPlot <- vpcParameterPlot(
            data = comparisonData,
            metaData = vpcMetaData,
            plotObject = referenceVpcPlot
          )

          demographyPlot[[paste0(populationName, "-vs-ref-", yParameterLabel, "-vs-", xParameterLabel)]] <- comparisonVpcPlot
          demographyPlot[[paste0(populationName, "-vs-ref-", yParameterLabel, "-vs-", xParameterLabel, "-log")]] <- comparisonVpcPlot +
            ggplot2::scale_y_continuous(trans = "log10")
        }
      }

      for (populationName in populationNames) {
        vpcData <- aggregatedData[aggregatedData$Population %in% populationName, ]
        vpcData$Population <- paste("Simulated ", AggregationConfiguration$names$middle, AggregationConfiguration$names$range, "for", vpcData$Population)

        vpcPlot <- vpcParameterPlot(
          data = vpcData,
          metaData = vpcMetaData,
          plotConfiguration = settings$plotConfigurations[["vpcParameterPlot"]]
        )
        demographyPlot[[paste0(populationName, "-", yParameterLabel, "-vs-", xParameterLabel)]] <- vpcPlot
        demographyPlot[[paste0(populationName, "-", yParameterLabel, "-vs-", xParameterLabel, "-log")]] <- vpcPlot +
          ggplot2::scale_y_continuous(trans = "log10")
      }
    }
  }

  return(list(plots = demographyPlot))
}

getDemographyAcrossPopulations <- function(structureSets) {
  demographyAcrossPopulations <- NULL
  for (structureSet in structureSets)
  {
    population <- ospsuite::loadPopulation(structureSet$simulationSet$populationFile)
    populationTable <- ospsuite::populationAsDataFrame(population)

    fullDemographyTable <- cbind.data.frame(
      simulationSetName = structureSet$simulationSet$simulationSetName,
      populationTable
    )
    demographyAcrossPopulations <- rbind.data.frame(
      demographyAcrossPopulations,
      fullDemographyTable
    )
  }

  return(demographyAcrossPopulations)
}

getDefaultXParametersForWorkflowType <- function(workflowType) {
  validateIsIncluded(workflowType, PopulationWorkflowTypes)
  if (workflowType %in% PopulationWorkflowTypes$pediatric) {
    return(ospsuite::StandardPath$Age)
  }
  return(NULL)
}

# TO DO:
# Default does not include gender nor BSA. BSA is not output by population class
getDefaultYParametersForWorkflowType <- function(workflowType) {
  validateIsIncluded(workflowType, PopulationWorkflowTypes)

  defaultStandardPaths <- ospsuite::StandardPath
  if (workflowType %in% PopulationWorkflowTypes$pediatric) {
    defaultStandardPaths <- defaultStandardPaths[c("Height", "Weight", "BMI", "GestationalAge", "OntogenyFactorAlbumin", "OntogenyFactorAlbuminAGP")]
    return(defaultStandardPaths)
  }
  defaultStandardPaths <- defaultStandardPaths[c("Age", "Height", "Weight", "BMI")]
  return(defaultStandardPaths)
}

getDemographyAggregatedData <- function(data,
                                        xParameterName,
                                        yParameterName,
                                        xParameterBreaks = NULL) {
  xParameterBreaks <- xParameterBreaks %||% 10
  xParameterBins <- cut(data[, xParameterName], breaks = xParameterBreaks)

  xData <- stats::aggregate(
    x = data[, xParameterName],
    by = list(
      Bins = xParameterBins,
      Population = data[, "simulationSetName"]
    ),
    FUN = AggregationConfiguration$functions$middle
  )

  medianData <- stats::aggregate(
    x = data[, yParameterName],
    by = list(
      Bins = xParameterBins,
      Population = data[, "simulationSetName"]
    ),
    FUN = AggregationConfiguration$functions$middle
  )
  lowPercData <- stats::aggregate(
    x = data[, yParameterName],
    by = list(
      Bins = xParameterBins,
      Population = data[, "simulationSetName"]
    ),
    FUN = AggregationConfiguration$functions$ymin
  )
  highPercData <- stats::aggregate(
    x = data[, yParameterName],
    by = list(
      Bins = xParameterBins,
      Population = data[, "simulationSetName"]
    ),
    FUN = AggregationConfiguration$functions$ymax
  )

  aggregatedData <- cbind.data.frame(xData,
    median = medianData$x,
    ymin = lowPercData$x,
    ymax = highPercData$x
  )

  return(aggregatedData)
}

getReferencePopulationName <- function(structureSets) {
  allSimulationReferences <- sapply(structureSets, function(structureSet) {
    structureSet$simulationSet$referencePopulation
  })
  validateIsOfLength(allSimulationReferences[allSimulationReferences], 1)
  referencePopulationName <- structureSets[[which(allSimulationReferences)]]$simulationSet$simulationSetName
  return(referencePopulationName)
}

#' @title plotDemographyHistogram
#' @description Plot histograms of ratios as is
#' @param data data.frame of the ratios
#' @param plotConfiguration PlotConfiguration R6 class object
#' @return ggplot object
#' @export
#' @import ospsuite
#' @import tlf
#' @import ggplot2
plotDemographyHistogram <- function(data,
                                    bins = NULL,
                                    dataMapping = NULL,
                                    plotConfiguration = NULL) {
  dataMapping <- dataMapping %||% tlf::HistogramDataMapping$new(x = ospsuite::StandardPath$Age)

  plotConfiguration <- PlotConfiguration$new(
    data = data,
    dataMapping = dataMapping
  )

  demographyPlot <- tlf::initializePlot(plotConfiguration)

  demographyPlot <- demographyPlot +
    ggplot2::geom_histogram(
      data = data,
      mapping = ggplot2::aes_string(
        x = paste0("`", dataMapping$x, "`"),
        fill = paste0("`", dataMapping$groupMapping$fill$label, "`"),
      ),
      color = "black",
      alpha = 0.8,
      position = ggplot2::position_dodge2(preserve = "single"),
      bins = bins
    )
  demographyPlot <- demographyPlot + ggplot2::labs(title = NULL, subtitle = NULL) +
    ggplot2::xlab(lastPathElement(dataMapping$x)) + ggplot2::ylab("Number of individuals") +
    ggplot2::guides(fill = guide_legend(title = NULL))
  return(demographyPlot)
}
