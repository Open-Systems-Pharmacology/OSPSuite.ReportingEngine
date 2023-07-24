#' @title plotDemographyParameters
#' @description Plot demography parameters box plots and tables
#' @param structureSets `SimulationStructure` R6 class object
#' @param settings list of settings for the output table/plot
#' @param workflowType workflowType Type of population workflow.
#' Use enum `PopulationWorkflowTypes` to get list of workflow types.
#' @param xParameters list of parameters to be plotted along x axis
#' @param yParameters list of parameters to be plotted along y axis
#' @return list of plots and tables with summary of demography parameters
#' @import ospsuite
#' @import tlf
#' @import ggplot2
#' @import ospsuite.utils
#' @keywords internal
plotDemographyParameters <- function(structureSets,
                                     settings = NULL,
                                     workflowType = PopulationWorkflowTypes$parallelComparison,
                                     xParameters = getDefaultDemographyXParameters(workflowType),
                                     yParameters = NULL) {
  validateIsIncluded(workflowType, PopulationWorkflowTypes)
  validateIsOfType(structureSets, "list")
  validateIsOfType(c(structureSets), "SimulationStructure")
  validateIsString(c(xParameters), nullAllowed = TRUE)
  validateIsString(c(yParameters), nullAllowed = TRUE)

  demographyResults <- list()
  simulationSetDescriptor <- structureSets[[1]]$simulationSetDescriptor

  yParameters <- yParameters %||% DemographyDefaultParameters

  demographyAcrossPopulations <- getDemographyAcrossPopulations(
    structureSets = structureSets,
    demographyPaths = unique(c(xParameters, yParameters))
  )
  demographyData <- demographyAcrossPopulations$data
  simulationSetNames <- unique(as.character(demographyData$simulationSetName))
  demographyMetaData <- demographyAcrossPopulations$metaData

  observedDemographyData <- getObservedDemographyAcrossPopulations(
    structureSets = structureSets,
    demographyPaths = unique(c(xParameters, yParameters)),
    metaData = demographyMetaData
  )

  if (isIncluded(workflowType, PopulationWorkflowTypes$pediatric)) {
    referenceSimulationSetName <- getReferencePopulationName(structureSets)
  }
  # If no demography variable defined in xParameters, plot histograms
  if (isEmpty(xParameters)) {
    demographyResults <- switch(workflowType,
      # Pediatric: comparison histogram
      "pediatric" = getComparisonHistogramResults(
        simulationSetNames = simulationSetNames,
        demographyPaths = yParameters,
        data = demographyData,
        metaData = demographyMetaData,
        observedData = observedDemographyData,
        settings = settings,
        simulationSetDescriptor = simulationSetDescriptor
      ),
      # Parallel and Ratio: histograms per population
      getHistogramResults(
        simulationSetNames = simulationSetNames,
        demographyPaths = yParameters,
        data = demographyData,
        metaData = demographyMetaData,
        observedData = observedDemographyData,
        settings = settings,
        simulationSetDescriptor = simulationSetDescriptor
      )
    )
  }

  for (demographyParameter in xParameters) {
    # Categorical variables won't be plotted
    if (demographyMetaData[[demographyParameter]]$class %in% "character") {
      next
    }
    xParameterCaption <- demographyMetaData[[demographyParameter]]$dimension
    sectionId <- defaultFileNames$resultID(length(demographyResults) + 1, "demography", demographyParameter)

    # Create level title for x parameter name
    demographyResults[[sectionId]] <- saveTaskResults(
      id = sectionId,
      textChunk = captions$demography$xParameterSection(sectionId, xParameterCaption),
      includeTextChunk = TRUE
    )
    # This aims at preventing plots such as age vs age
    for (parameterName in setdiff(yParameters, demographyParameter)) {
      # Categorical variables won't be plotted
      if (demographyMetaData[[parameterName]]$class %in% "character") {
        next
      }
      vpcMetaData <- list(
        "x" = demographyMetaData[[demographyParameter]],
        "median" = demographyMetaData[[parameterName]]
      )
      yParameterCaption <- vpcMetaData$median$dimension
      sectionId <- defaultFileNames$resultID(length(demographyResults) + 1, "demography", demographyParameter, parameterName)

      # Create sub level title for y parameter name
      demographyResults[[sectionId]] <- saveTaskResults(
        id = sectionId,
        textChunk = captions$demography$yParameterSection(sectionId, yParameterCaption),
        includeTextChunk = TRUE
      )

      # For pediatric workflow, range plots compare reference population to the other populations
      if (workflowType %in% c(PopulationWorkflowTypes$pediatric)) {
        referenceData <- demographyData[demographyData$simulationSetName %in% referenceSimulationSetName, ]

        aggregatedReferenceData <- data.frame(
          x = c(-Inf, Inf),
          ymin = rep(AggregationConfiguration$functions$ymin(referenceData[, parameterName]), 2),
          median = rep(AggregationConfiguration$functions$middle(referenceData[, parameterName]), 2),
          ymax = rep(AggregationConfiguration$functions$ymax(referenceData[, parameterName]), 2),
          "Population" = paste("Simulated", AggregationConfiguration$names$middle, "and", AggregationConfiguration$names$range, "of", referenceSimulationSetName)
        )

        referenceVpcPlot <- vpcParameterPlot(
          data = aggregatedReferenceData,
          metaData = vpcMetaData,
          plotConfiguration = settings$plotConfigurations[["comparisonVpcPlot"]]
        )

        # Range plot comparisons with reference
        for (simulationSetName in simulationSetNames[!simulationSetNames %in% referenceSimulationSetName]) {
          sectionId <- defaultFileNames$resultID(length(demographyResults) + 1, "demography", demographyParameter, parameterName)
          # Create sub level title for population
          demographyResults[[sectionId]] <- saveTaskResults(
            id = sectionId,
            textChunk = captions$demography$populationSection(sectionId, simulationSetName, simulationSetDescriptor),
            includeTextChunk = TRUE
          )

          comparisonData <- demographyData[demographyData$simulationSetName %in% simulationSetName, ]
          comparisonData <- getDemographyAggregatedData(
            data = comparisonData,
            xParameterName = demographyParameter,
            yParameterName = parameterName,
            bins = settings$bins,
            stairstep = settings$stairstep
          )
          comparisonData$Population <- paste("Simulated", AggregationConfiguration$names$middle, "and", AggregationConfiguration$names$range)

          comparisonVpcPlot <- vpcParameterPlot(
            data = comparisonData,
            metaData = vpcMetaData,
            plotObject = referenceVpcPlot
          )

          # Save comparison vpc plots
          resultID <- defaultFileNames$resultID(length(demographyResults) + 1, "demography", demographyParameter, parameterName)
          demographyResults[[resultID]] <- saveTaskResults(
            id = resultID,
            plot = comparisonVpcPlot,
            plotCaption = captions$demography$rangePlot(
              xParameterCaption,
              yParameterCaption,
              simulationSetName,
              simulationSetDescriptor,
              referenceSetName = referenceSimulationSetName
            )
          )

          vpcLogLimits <- autoAxesLimits(c(comparisonData$ymin, comparisonData$median, comparisonData$ymax), scale = "log")
          vpcLogTicks <- autoAxesTicksFromLimits(vpcLogLimits)

          resultID <- defaultFileNames$resultID(length(demographyResults) + 1, "demography", demographyParameter, parameterName, "log")
          demographyResults[[resultID]] <- saveTaskResults(
            id = resultID,
            plot = tlf::setYAxis(
              plotObject = comparisonVpcPlot,
              scale = tlf::Scaling$log,
              axisLimits = vpcLogLimits,
              ticks = vpcLogTicks
            ),
            plotCaption = captions$demography$rangePlot(
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

      # Simple range plots
      for (simulationSetName in simulationSetNames) {
        sectionId <- defaultFileNames$resultID(length(demographyResults) + 1, "demography", demographyParameter, parameterName)
        # Create sub level title for population
        demographyResults[[sectionId]] <- saveTaskResults(
          id = sectionId,
          textChunk = captions$demography$populationSection(sectionId, simulationSetName, simulationSetDescriptor),
          includeTextChunk = TRUE
        )

        vpcData <- demographyData[demographyData$simulationSetName %in% simulationSetName, ]
        vpcData <- getDemographyAggregatedData(
          data = vpcData,
          xParameterName = demographyParameter,
          yParameterName = parameterName,
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

        # Save comparison vpc plots
        resultID <- defaultFileNames$resultID(length(demographyResults) + 1, "demography", demographyParameter, parameterName)
        demographyResults[[resultID]] <- saveTaskResults(
          id = resultID,
          plot = vpcPlot,
          plotCaption = captions$demography$rangePlot(
            xParameterCaption,
            yParameterCaption,
            simulationSetName,
            simulationSetDescriptor
          )
        )

        vpcLogLimits <- autoAxesLimits(c(vpcData$ymin, vpcData$median, vpcData$ymax), scale = "log")
        vpcLogTicks <- autoAxesTicksFromLimits(vpcLogLimits)

        resultID <- defaultFileNames$resultID(length(demographyResults) + 1, "demography", demographyParameter, parameterName, "log")
        demographyResults[[resultID]] <- saveTaskResults(
          id = resultID,
          plot = tlf::setYAxis(
            plotObject = vpcPlot,
            scale = tlf::Scaling$log,
            axisLimits = vpcLogLimits,
            ticks = vpcLogTicks
          ),
          plotCaption = captions$demography$rangePlot(
            xParameterCaption,
            yParameterCaption,
            simulationSetName,
            simulationSetDescriptor,
            plotScale = "logarithmic"
          )
        )
      }
    }
  }

  return(demographyResults)
}

#' @title getDemographyAcrossPopulations
#' @description Get Demography data across populations of simulation sets
#' @param structureSets List of `SimulationStructure` objects
#' @param demographyPaths Paths of demography variables to display
#' @return A list of `data` and its `metaData`
#' @import ospsuite
#' @keywords internal
getDemographyAcrossPopulations <- function(structureSets, demographyPaths) {
  demographyAcrossPopulations <- NULL
  dataColumnNames <- c("simulationSetName", as.character(demographyPaths))
  for (structureSet in structureSets) {
    population <- loadWorkflowPopulation(structureSet$simulationSet)
    simulation <- ospsuite::loadSimulation(structureSet$simulationSet$simulationFile, loadFromCache = TRUE)
    populationTable <- getPopulationPKData(population, simulation)
    checkIsIncluded(
      demographyPaths,
      names(populationTable),
      groupName = paste0(
        "demography variables of simulation set '",
        structureSet$simulationSet$simulationSetName,
        "'"
      )
    )
    # Initialize data.frame of only relevant demography data
    demographyData <- as.data.frame(
      sapply(
      dataColumnNames,
      function(x) {
        rep(NA, population$count)
      },
      simplify = FALSE
    ),
    check.names = FALSE
    )
    demographyData$simulationSetName <- structureSet$simulationSet$simulationSetName
    for (demographyPath in demographyPaths) {
      if (!isIncluded(demographyPath, names(populationTable))) {
        next
      }
      demographyData[[demographyPath]] <- populationTable[[demographyPath]]
    }
    demographyAcrossPopulations <- rbind.data.frame(
      demographyAcrossPopulations,
      demographyData
    )
  }
  # Use last simulationSet to get display name and unit as metaData
  metaData <- getPopulationPKMetaData(population, simulation, structureSet$parameterDisplayPaths)

  return(list(
    data = demographyAcrossPopulations,
    metaData = metaData
  ))
}

#' @title getObservedDemographyAcrossPopulations
#' @description Get Observed Demography data across populations of simulation sets
#' @param structureSets List of `SimulationStructure` objects
#' @param demographyPaths Paths of demography variables to display
#' @param metaData List of display names and units of demography variables
#' @return A data.frame
#' @import ospsuite
#' @keywords internal
getObservedDemographyAcrossPopulations <- function(structureSets, demographyPaths, metaData) {
  demographyDataAcrossPopulations <- NULL
  for (structureSet in structureSets) {
    demographyData <- getObservedDemographyFromSimulationSet(
      structureSet = structureSet,
      demographyPaths = demographyPaths,
      metaData = metaData
    )
    demographyDataAcrossPopulations <- rbind.data.frame(
      demographyDataAcrossPopulations,
      demographyData
    )
  }

  return(demographyDataAcrossPopulations)
}

#' @title DemographyDefaultParameters
#' @description Demography Default Parameters
#' @keywords internal
DemographyDefaultParameters <- c(ospsuite::StandardPath[c("Age", "Height", "Weight", "BMI")], list(Gender = "Gender"))

#' @title getDefaultDemographyXParameters
#' @description Get names of default demography parameters in x axis of demography plots.
#' @param workflowType Name of workflow type.
#' Use enum `PopulationWorkflowTypes` to get a list of available workflow types.
#' @return names of default demography parameters
#' @export
#' @examples
#'
#' getDefaultDemographyXParameters(PopulationWorkflowTypes$pediatric)
#'
getDefaultDemographyXParameters <- function(workflowType) {
  validateIsIncluded(workflowType, PopulationWorkflowTypes)
  if (workflowType %in% PopulationWorkflowTypes$pediatric) {
    return(ospsuite::StandardPath$Age)
  }
  return(NULL)
}

#' @title getDemographyAggregatedData
#' @param data A data.frame
#' @param xParameterName Name of parameter in `data` used for aggregation in x axis of plot
#' @param yParameterName Name of parameter in `data` aggregated in y axis of plot
#' @param bins Either a numeric vector defining bin edges
#' or a numeric value defining the number of bins.
#' @param stairstep A logical value defining if aggregation uses continuous or stairstep plot
#' @return A data.frame of aggregated data
#' @import ospsuite.utils
#' @keywords internal
getDemographyAggregatedData <- function(data,
                                        xParameterName,
                                        yParameterName,
                                        bins = NULL,
                                        stairstep = TRUE) {
  stairstep <- stairstep %||% TRUE
  xParameterBreaks <- bins %||% AggregationConfiguration$bins
  # binningOnQuantiles use data distribution to improve the binning
  if (isOfLength(bins, 1) & AggregationConfiguration$binUsingQuantiles) {
    xParameterBreaks <- unique(unname(quantile(x = data[, xParameterName], probs = seq(0, 1, length.out = xParameterBreaks))))
  }
  xParameterBins <- cut(data[, xParameterName], breaks = xParameterBreaks)

  # simulationSetName was removed from "by" input because
  # it is a factor class that messes up the aggregation now that
  # simulationSetName filtering is performed before aggregation
  xData <- stats::aggregate(
    x = data[, xParameterName],
    by = list(Bins = xParameterBins),
    FUN = AggregationConfiguration$functions$middle,
    drop = FALSE
  )

  medianData <- stats::aggregate(
    x = data[, yParameterName],
    by = list(Bins = xParameterBins),
    FUN = AggregationConfiguration$functions$middle,
    drop = FALSE
  )
  lowPercData <- stats::aggregate(
    x = data[, yParameterName],
    by = list(Bins = xParameterBins),
    FUN = AggregationConfiguration$functions$ymin,
    drop = FALSE
  )
  highPercData <- stats::aggregate(
    x = data[, yParameterName],
    by = list(Bins = xParameterBins),
    FUN = AggregationConfiguration$functions$ymax,
    drop = FALSE
  )

  aggregatedData <- cbind.data.frame(xData,
    median = medianData$x,
    ymin = lowPercData$x,
    ymax = highPercData$x
  )

  if (stairstep) {
    # Method in documentation of cut to get the bin edges
    labs <- levels(xParameterBins)
    xminValues <- as.numeric(sub("\\((.+),.*", "\\1", labs))
    xmaxValues <- as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", labs))

    xData <- rbind.data.frame(xData, xData)
    xData$x <- sort(c(xminValues, xmaxValues))

    aggregatedData <- cbind.data.frame(xData,
      median = rep(medianData$x, each = 2),
      ymin = rep(lowPercData$x, each = 2),
      ymax = rep(highPercData$x, each = 2)
    )
  }

  return(aggregatedData)
}


#' @title getHistogramResults
#' @description Get Histogram results for Parallel and Ratio Comparison workflows
#' @param demographyPaths Names of demography variables to be displayed
#' @param simulationSetNames Names of simulation sets
#' @param data A data.frame of simulated demography values across the simulationSets
#' @param metaData A list of meta data indicating the display properties of the data
#' @param observedData A data.frame of observed demography values across the simulationSets
#' @param settings A list of plot settings
#' @param simulationSetDescriptor Character describing the population sets within the report
#' @param demographyResults A list of `TaskResult` objects
#' @return A list of `TaskResult` objects
#' @keywords internal
getHistogramResults <- function(demographyPaths,
                                simulationSetNames,
                                data,
                                metaData,
                                observedData,
                                settings = NULL,
                                simulationSetDescriptor = "",
                                demographyResults = list()) {
  for (demographyPath in demographyPaths) {
    sectionId <- defaultFileNames$resultID(length(demographyResults) + 1, "demography", demographyPath)
    # Display name was stored in dimension of metaData
    # to leverage smart plot configuration caption as dimension [unit]
    parameterCaption <- metaData[[demographyPath]]$dimension
    # Create sub level title for each demography path
    demographyResults[[sectionId]] <- saveTaskResults(
      id = sectionId,
      textChunk = captions$demography$parameterSection(sectionId, parameterCaption),
      includeTextChunk = TRUE
    )
    histogramMapping <- tlf::HistogramDataMapping$new(
      x = demographyPath,
      fill = "Legend"
    )
    for (simulationSetName in simulationSetNames) {
      sectionId <- defaultFileNames$resultID(length(demographyResults) + 1, "demography", demographyPath)
      # Within each path, create sub level title for each simulation set
      demographyResults[[sectionId]] <- saveTaskResults(
        id = sectionId,
        textChunk = captions$demography$populationSection(sectionId, simulationSetName, simulationSetDescriptor, level = 3),
        includeTextChunk = TRUE
      )

      demographyData <- data[data$simulationSetName %in% simulationSetName, c("simulationSetName", demographyPath)]
      demographyData$Legend <- captions$demography$histogramLegend(demographyData)
      # Check for observed data and include if possible
      selectedRows <- observedData$simulationSetName %in% simulationSetName
      dataSource <- ""
      if (sum(selectedRows) > 0) {
        observedDemographyData <- observedData[selectedRows, c("simulationSetName", demographyPath)]
        observedDemographyData$Legend <- captions$demography$histogramLegend(observedDemographyData, observed = TRUE)
        demographyData <- rbind.data.frame(demographyData, observedDemographyData)
        demographyData$Legend <- factor(demographyData$Legend, levels = c(demographyData$Legend[1], observedDemographyData$Legend[1]))
        dataSource <- head(observedData$dataSource, 1)
      }

      demographyHistogram <- plotDemographyHistogram(
        data = demographyData,
        metaData = metaData,
        dataMapping = histogramMapping,
        plotConfiguration = settings$plotConfigurations[["histogram"]] %||%
          tlf::HistogramPlotConfiguration$new(
            ylabel = "Number of individuals [%]",
            data = demographyData,
            metaData = metaData,
            dataMapping = histogramMapping
          ),
        bins = settings$bins %||% AggregationConfiguration$bins,
        dodge = settings$dodge %||% TRUE
      )

      # Save results
      resultID <- defaultFileNames$resultID(length(demographyResults) + 1, "demography", demographyPath)
      demographyResults[[resultID]] <- saveTaskResults(
        id = resultID,
        plot = demographyHistogram,
        plotCaption = captions$demography$histogram(
          parameterCaption,
          simulationSetName,
          simulationSetDescriptor,
          dataSource = dataSource
        )
      )
    }
  }
  return(demographyResults)
}

#' @title getComparisonHistogramResults
#' @description Get Comparison Histogram results for Pediatric workflows
#' @param demographyPaths Names of demography variables to be displayed
#' @param simulationSetNames Names of simulation sets
#' @param data A data.frame of simulated demography values across the simulationSets
#' @param metaData A list of meta data indicating the display properties of the data
#' @param observedData A data.frame of observed demography values across the simulationSets
#' @param settings A list of plot settings
#' @param simulationSetDescriptor Character describing the population sets within the report
#' @param demographyResults A list of `TaskResult` objects
#' @return A list of `TaskResult` objects
#' @keywords internal
getComparisonHistogramResults <- function(demographyPaths,
                                          simulationSetNames,
                                          data,
                                          metaData,
                                          observedData,
                                          settings = NULL,
                                          simulationSetDescriptor = "",
                                          demographyResults = list()) {
  for (demographyPath in demographyPaths) {
    sectionId <- defaultFileNames$resultID(length(demographyResults) + 1, "demography", demographyPath)
    # Display name was stored in dimension of metaData
    # to leverage smart plot configuration caption as dimension [unit]
    parameterCaption <- metaData[[demographyPath]]$dimension
    # Create sub level title for each demography path
    demographyResults[[sectionId]] <- saveTaskResults(
      id = sectionId,
      textChunk = captions$demography$parameterSection(sectionId, parameterCaption),
      includeTextChunk = TRUE
    )
    histogramMapping <- tlf::HistogramDataMapping$new(x = demographyPath, fill = "Legend")

    data$Legend <- stats::ave(
      data$simulationSetName,
      # This second line is needed to compute size within each set
      data$simulationSetName,
      FUN = function(setName) {
        paste0("Simulated ", setName, " (n=", length(setName), ")")
      }
    )
    demographyHistogram <- plotDemographyHistogram(
      data = data,
      metaData = metaData,
      dataMapping = histogramMapping,
      plotConfiguration = settings$plotConfigurations[["histogram"]] %||%
        tlf::HistogramPlotConfiguration$new(
          ylabel = "Number of individuals [%]",
          data = data,
          metaData = metaData,
          dataMapping = histogramMapping
        ),
      bins = settings$bins %||% AggregationConfiguration$bins,
      dodge = settings$dodge %||% TRUE
    )
    # Save results
    resultID <- defaultFileNames$resultID(length(demographyResults) + 1, "demography", demographyPath)
    demographyResults[[resultID]] <- saveTaskResults(
      id = resultID,
      plot = demographyHistogram,
      plotCaption = captions$demography$histogram(
        parameterCaption,
        simulationSetNames,
        simulationSetDescriptor
      )
    )
    # Plot observed only if available
    if (sum(!is.na(observedData[, demographyPath])) == 0) {
      next
    }
    observedData$Legend <- stats::ave(
      observedData$simulationSetName,
      # This second line is needed to compute size within each set
      observedData$simulationSetName,
      FUN = function(setName) {
        paste0("Observed ", setName, " (n=", length(setName), ")")
      }
    )
    demographyHistogram <- plotDemographyHistogram(
      data = observedData,
      metaData = metaData,
      dataMapping = histogramMapping,
      plotConfiguration = settings$plotConfigurations[["histogram"]] %||%
        tlf::HistogramPlotConfiguration$new(
          ylabel = "Number of individuals [%]",
          data = observedData,
          metaData = metaData,
          dataMapping = histogramMapping
        ),
      bins = settings$bins %||% AggregationConfiguration$bins,
      dodge = settings$dodge %||% TRUE
    )
    # Save results
    resultID <- defaultFileNames$resultID(length(demographyResults) + 1, "demography", demographyPath)
    demographyResults[[resultID]] <- saveTaskResults(
      id = resultID,
      plot = demographyHistogram,
      plotCaption = captions$demography$histogram(
        parameterCaption,
        simulationSetNames,
        simulationSetDescriptor,
        dataSource = paste(unique(observedData$dataSource), collapse = "")
      )
    )
  }
  return(demographyResults)
}


#' @title plotDemographyHistogram
#' @description Plot histograms for demography parameters
#' @param data data.frame
#' @param metaData list of metaData about `data`
#' @param dataMapping `HistogramDataMapping` class object
#' @param plotConfiguration `PlotConfiguration` class object
#' @param bins Number of bins for continuous demography parameters
#' @param dodge For continuous demography parameters,
#' Logical defining if histogram bars should dodge for continuous parameters
#' @return ggplot object
#' @export
#' @import tlf
#' @import ggplot2
plotDemographyHistogram <- function(data,
                                    metaData,
                                    dataMapping = NULL,
                                    plotConfiguration = NULL,
                                    bins = AggregationConfiguration$bins,
                                    dodge = TRUE) {
  mapLabels <- tlf:::.getAesStringMapping(dataMapping)
  # Calculate the inner class count to normalize the final histogram
  data$classCount <- as.numeric(stats::ave(
    data[[mapLabels$x]],
    data[[mapLabels$fill]],
    FUN = function(x) {
      sum(!is.na(x))
    }
  ))

  aesProperties <- tlf:::.getAestheticValuesFromConfiguration(
    n = length(unique(data[[mapLabels$fill]])),
    plotConfigurationProperty = plotConfiguration$ribbons,
    propertyNames = c("fill", "color", "alpha")
  )
  demographyPlot <- tlf::initializePlot(plotConfiguration)
  # If character covariate such as Gender, use geom_bar
  if (isIncluded(metaData[[dataMapping$x]]$class, "character")) {
    demographyPlot <- demographyPlot +
      ggplot2::geom_bar(
        data = data,
        mapping = ggplot2::aes(
          x = .data[[mapLabels$x]],
          # Scaling to population size
          weight = 100 / .data[["classCount"]],
          fill = .data[[mapLabels$fill]]
        ),
        color = aesProperties$color[1],
        alpha = aesProperties$alpha[1],
        position = ggplot2::position_dodge2(preserve = "single")
      ) +
      ggplot2::scale_fill_manual(values = aesProperties$fill) +
      ggplot2::labs(fill = NULL)

    return(demographyPlot)
  }

  barPosition <- ggplot2::position_nudge()
  if (dodge) {
    barPosition <- ggplot2::position_dodge2(preserve = "single")
  }
  demographyPlot <- demographyPlot +
    ggplot2::geom_histogram(
      data = data,
      mapping = ggplot2::aes(
        x = .data[[mapLabels$x]],
        weight = 100 / .data[["classCount"]],
        fill = .data[[mapLabels$fill]]
      ),
      bins = bins,
      color = aesProperties$color[1],
      # Set bars more transparent if dodge is false, because they could mask each other
      alpha = aesProperties$alpha[1] * ifelse(dodge, 1, 0.8),
      position = barPosition
    ) +
    ggplot2::scale_fill_manual(values = aesProperties$fill) +
    ggplot2::labs(fill = NULL)

  return(demographyPlot)
}
