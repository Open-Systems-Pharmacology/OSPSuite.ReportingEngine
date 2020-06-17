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
                                     xParameters = getDefaultDemographyXParameters(workflowType),
                                     yParameters = NULL) {
  validateIsIncluded(workflowType, PopulationWorkflowTypes)
  validateIsOfType(structureSets, "list")
  validateIsOfType(c(structureSets), "SimulationStructure")
  validateIsString(c(xParameters), nullAllowed = TRUE)
  validateIsString(c(yParameters), nullAllowed = TRUE)

  demographyPlots <- list()
  demographyCaptions <- list()
  captionSimulationNames <- paste0(as.character(sapply(structureSets, function(set) {
    set$simulationSet$simulationSetName
  })), collapse = ", ")

  # User defined yParameters will be used as is.
  # Otherwise, the default is to pick from the DemographyDefaultParameters excluding xParameters
  # This will prevent plots such as age vs age
  yParameters <- yParameters %||% setdiff(DemographyDefaultParameters, xParameters)

  demographyAcrossPopulations <- getDemographyAcrossPopulations(structureSets)
  demographyData <- demographyAcrossPopulations$data
  demographyMetaData <- demographyAcrossPopulations$metaData

  if (workflowType %in% PopulationWorkflowTypes$pediatric) {
    referencePopulationName <- getReferencePopulationName(structureSets)
  }

  if (is.null(xParameters)) {
    # Pediatric: comparison histogram
    if (workflowType %in% c(PopulationWorkflowTypes$pediatric)) {
      for (parameterName in yParameters) {
        parameterLabel <- demographyMetaData[[parameterName]]$dimension

        histogramMapping <- tlf::HistogramDataMapping$new(
          x = parameterName,
          fill = "simulationSetName"
        )

        demographyHistogram <- plotDemographyHistogram(
          data = demographyData,
          metaData = demographyMetaData,
          dataMapping = histogramMapping,
          plotConfiguration = settings$plotConfigurations[["histogram"]],
          bins = settings$bins %||% 11
        )
        demographyPlots[[parameterLabel]] <- demographyHistogram
        demographyCaptions[[parameterLabel]] <- paste0("Distribution of ", demographyMetaData[[parameterName]]$dimension, " for ", captionSimulationNames)
      }
    }
    # Parallel and Ratio: histograms per population
    if (workflowType %in% c(PopulationWorkflowTypes$parallelComparison, PopulationWorkflowTypes$ratioComparison)) {
      for (parameterName in yParameters) {
        parameterLabel <- demographyMetaData[[parameterName]]$dimension
        histogramMapping <- tlf::HistogramDataMapping$new(
          x = parameterName,
          fill = "simulationSetName"
        )
        for (populationName in levels(factor(demographyData$simulationSetName))) {
          demographyDataByPopulation <- demographyData[demographyData$simulationSetName %in% populationName, ]

          demographyHistogram <- plotDemographyHistogram(
            data = demographyDataByPopulation,
            metaData = demographyMetaData,
            dataMapping = histogramMapping,
            plotConfiguration = settings$plotConfigurations[["histogram"]],
            bins = settings$bins %||% 11
          )

          demographyPlots[[paste0(parameterLabel, "-", populationName)]] <- demographyHistogram
          demographyCaptions[[paste0(parameterLabel, "-", populationName)]] <- paste0("Distribution of ", demographyMetaData[[parameterName]]$dimension, " for ", populationName)
        }
      }
    }
    return(list(plots = demographyPlots))
  }

  for (demographyParameter in xParameters) {
    xParameterLabel <- demographyMetaData[[demographyParameter]]$dimension
    for (parameterName in yParameters) {
      yParameterLabel <- demographyMetaData[[parameterName]]$dimension

      vpcMetaData <- list(
        "x" = demographyMetaData[[demographyParameter]],
        "median" = demographyMetaData[[parameterName]]
      )

      aggregatedData <- getDemographyAggregatedData(
        data = demographyData,
        xParameterName = demographyParameter,
        yParameterName = parameterName,
        xParameterBreaks = settings$xParametersBreaks[[demographyParameter]]
      )
      populationNames <- levels(factor(aggregatedData$Population))

      # For pediatric workflow, range plots compare reference population to the other populations
      if (workflowType %in% c(PopulationWorkflowTypes$pediatric)) {
        referenceData <- demographyData[demographyData$simulationSetName %in% referencePopulationName, ]

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

          demographyPlots[[paste0(populationName, "-vs-ref-", yParameterLabel, "-vs-", xParameterLabel)]] <- comparisonVpcPlot
          demographyPlots[[paste0(populationName, "-vs-ref-", yParameterLabel, "-vs-", xParameterLabel, "-log")]] <- tlf::setYAxis(plotObject = comparisonVpcPlot, scale = tlf::Scaling$log10)

          demographyCaptions[[paste0(populationName, "-vs-ref-", yParameterLabel, "-vs-", xParameterLabel)]] <- paste0(vpcMetaData$x$dimension, "-dependence of ", vpcMetaData$median$dimension, " for ", populationName, " in comparison to ", referencePopulationName, ". Profiles are plotted in a linear scale.")
          demographyCaptions[[paste0(populationName, "-vs-ref-", yParameterLabel, "-vs-", xParameterLabel, "-log")]] <- paste0(vpcMetaData$x$dimension, "-dependence of ", vpcMetaData$median$dimension, " for ", populationName, " in comparison to ", referencePopulationName, ". Profiles are plotted in a logarithmic scale.")
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
        demographyPlots[[paste0(populationName, "-", yParameterLabel, "-vs-", xParameterLabel)]] <- vpcPlot
        demographyPlots[[paste0(populationName, "-", yParameterLabel, "-vs-", xParameterLabel, "-log")]] <- tlf::setYAxis(plotObject = vpcPlot, scale = tlf::Scaling$log10)

        demographyCaptions[[paste0(populationName, "-", yParameterLabel, "-vs-", xParameterLabel)]] <- paste0(vpcMetaData$x$dimension, "-dependence of ", vpcMetaData$median$dimension, " for ", populationName, ". Profiles are plotted in a linear scale.")
        demographyCaptions[[paste0(populationName, "-", yParameterLabel, "-vs-", xParameterLabel, "-log")]] <- paste0(vpcMetaData$x$dimension, "-dependence of ", vpcMetaData$median$dimension, " for ", populationName, ". Profiles are plotted in a logarithmic scale.")
      }
    }
  }

  return(list(
    plots = demographyPlots,
    captions = demographyCaptions
  ))
}

getDemographyAcrossPopulations <- function(structureSets) {
  demographyAcrossPopulations <- NULL
  for (structureSet in structureSets)
  {
    population <- loadWorkflowPopulation(structureSet$simulationSet)
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
  simulation <- ospsuite::loadSimulation(structureSet$simulationSet$simulationFile)
  allParameters <- ospsuite::getAllParametersMatching(population$allParameterPaths, simulation)
  metaData <- lapply(allParameters, function(parameter) {
    list(
      dimension = parameter$name,
      unit = parameter$displayUnit
    )
  })
  names(metaData) <- sapply(allParameters, function(parameter) {
    parameter$path
  })

  # TO DO: Issue #185, covariate from population can be categorical/factors
  demographyAcrossPopulations$Gender <- as.numeric(demographyAcrossPopulations$Gender)
  metaData[["Gender"]] <- list(
    dimension = "Gender",
    unit = ""
  )

  return(list(
    data = demographyAcrossPopulations,
    metaData = metaData
  ))
}

DemographyDefaultParameters <- c(ospsuite::StandardPath[c("Age", "Height", "Weight", "BMI")], list(Gender = "Gender"))

#' @title getDefaultDemographyXParameters
#' @description Get names of default demography parameters in x axis of demography plots.
#' @param workflowType Name of workflow type.
#' Use enum `PopulationWorkflowTypes` to get a list of available workflow types.
#' @return names of default demography parameters
getDefaultDemographyXParameters <- function(workflowType) {
  validateIsIncluded(workflowType, PopulationWorkflowTypes)
  if (workflowType %in% PopulationWorkflowTypes$pediatric) {
    return(ospsuite::StandardPath$Age)
  }
  return(NULL)
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
                                    metaData,
                                    dataMapping = NULL,
                                    plotConfiguration = NULL,
                                    bins = NULL) {
  dataMapping <- dataMapping %||% tlf::HistogramDataMapping$new(x = ospsuite::StandardPath$Age)

  plotConfiguration <- PlotConfiguration$new(
    data = data,
    metaData = metaData,
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
  demographyPlot <- demographyPlot + 
    ggplot2::ylab("Number of individuals") +
    ggplot2::guides(fill = guide_legend(title = NULL))
  demographyPlot <- tlf::setLegendPosition(plotObject = demographyPlot, position = reDefaultLegendPosition)
  return(demographyPlot)
}

#' @title getXParametersForDemogrpahyPlot
#' @param workflow `PopulationWorkflow` R6 class object
#' @return list of x parameters used for demography range plots
#' @export
getXParametersForDemogrpahyPlot <- function(workflow) {
  validateIsOfType(workflow, "PopulationWorkflow")
  return(workflow$plotDemography$xParameters)
}

#' @title getYParametersForDemogrpahyPlot
#' @param workflow `PopulationWorkflow` R6 class object
#' @return list of y parameters used for demography histogram and range plots
#' @export
getYParametersForDemogrpahyPlot <- function(workflow) {
  validateIsOfType(workflow, "PopulationWorkflow")
  xParameters <- getXParametersForDemogrpahyPlot(workflow)
  yParameters <- workflow$plotDemography$yParameters %||% setdiff(DemographyDefaultParameters, xParameters)

  return(yParameters)
}

#' @title setXParametersForDemogrpahyPlot
#' @description Set x parameters for range plots of demography plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of demography parameters to be used as x-parameters
#' @export
setXParametersForDemogrpahyPlot <- function(workflow, parameters) {
  validateIsOfType(workflow, "PopulationWorkflow")
  validateIsString(c(parameters), nullAllowed = TRUE)

  workflow$plotDemography$xParameters <- parameters

  logWorkflow(
    message = paste0(
      "X-parameters: '",
      paste0(c(parameters), collapse = "', '"),
      "' set for demography plot."
    ),
    pathFolder = workflow$workflowFolder,
    logTypes = LogTypes$Debug
  )
  return(invisible())
}

#' @title setYParametersForDemogrpahyPlot
#' @description Set y-parameters for histograms and range plots of demography plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of demography parameters to be used as y-parameters
#' @export
setYParametersForDemogrpahyPlot <- function(workflow, parameters) {
  validateIsOfType(workflow, "PopulationWorkflow")
  validateIsString(c(parameters))

  workflow$plotDemography$yParameters <- parameters

  logWorkflow(
    message = paste0(
      "Y-parameters: '",
      paste0(c(parameters), collapse = "', '"),
      "' set for demography plot."
    ),
    pathFolder = workflow$workflowFolder,
    logTypes = LogTypes$Debug
  )
  return(invisible())
}
