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
  # If no demography variable defined in xParameters
  # Plot histograms
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
    return(demographyResults)
  }

  # If demography variable(s) defined in xParameters
  # Range plots or boxplots depending on the parameter class (issue #1088)
  for (demographyParameter in xParameters) {
    parameterClass <- demographyMetaData[[demographyParameter]]$class
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
      # Categorical covariates only plotted when on xParameter as boxplot (issue #1088)
      if (demographyMetaData[[parameterName]]$class %in% "character") {
        next
      }
      demographyDataMapping <- switch(parameterClass,
        "character" = tlf::BoxWhiskerDataMapping$new(
          x = demographyParameter,
          y = parameterName,
          fill = "Legend"
        ),
        tlf::TimeProfileDataMapping$new(
          x = "x",
          y = "median",
          ymin = "ymin",
          ymax = "ymax",
          color = "Legend",
          fill = "Legend"
        )
      )
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

      # Plot regular range plots potentially comparing simulated and observed
      for (simulationSetName in simulationSetNames) {
        sectionId <- defaultFileNames$resultID(length(demographyResults) + 1, "demography", demographyParameter, parameterName)
        # Create sub level title for population
        demographyResults[[sectionId]] <- saveTaskResults(
          id = sectionId,
          textChunk = captions$demography$populationSection(sectionId, simulationSetName, simulationSetDescriptor),
          includeTextChunk = TRUE
        )
        # Build dataset with Visual Predictive check format
        selectedRows <- demographyData$simulationSetName %in% simulationSetName
        demographyData$Legend <- captions$demography$rangePlotLegend(
          simulationSetName = simulationSetName,
          n = sum(selectedRows),
          parameterClass = parameterClass
        )

        # Depending on parameterClass, produce range plot or boxplot
        if (!isIncluded(parameterClass, "character")) {
          vpcData <- getDemographyAggregatedData(
            data = demographyData[selectedRows, ],
            xParameterName = demographyParameter,
            yParameterName = parameterName,
            groupName = "Legend",
            bins = settings$bins,
            stairstep = settings$stairstep
          )
        }
        # comparisonData is the name of the final data provided to the plot function
        # If parallel or ratio comparison workflow without data, it will be used as is
        # Otherwise, vpcData will be combined with obs/reference data to get new comparisonData
        comparisonData <- switch(parameterClass,
          "character" = demographyData[selectedRows, ],
          vpcData
        )

        # Include observed data into regular plot if available
        selectedObsRows <- observedDemographyData$simulationSetName %in% simulationSetName
        dataSource <- ""
        if (sum(!is.na(observedDemographyData[selectedObsRows, parameterName])) > 0) {
          dataSource <- head(observedDemographyData[selectedObsRows, "dataSource"], 1)
          # Build dataset with Visual Predictive check format
          selectedColumns <- c(demographyParameter, parameterName, "Legend", "simulationSetName")
          observedDemographyData$Legend <- switch(parameterClass,
            "character" = captions$demography$boxPlotLegend(simulationSetName, n = sum(selectedObsRows), dataType = "Observed"),
            captions$demography$rangePlotLegend(simulationSetName, n = sum(selectedObsRows), dataType = "Observed")
          )
          comparisonData <- switch(parameterClass,
            "character" = rbind.data.frame(
              demographyData[selectedRows, selectedColumns],
              observedDemographyData[selectedObsRows, selectedColumns]
            ),
            getDemographyAggregatedData(
              data = rbind.data.frame(
                demographyData[selectedRows, selectedColumns],
                observedDemographyData[selectedObsRows, selectedColumns]
              ),
              xParameterName = demographyParameter,
              yParameterName = parameterName,
              groupName = "Legend",
              bins = settings$bins,
              stairstep = settings$stairstep
            )
          )
          # Keep always reference as second entry for same coloring
          comparisonData$Legend <- factor(
            comparisonData$Legend,
            levels = c(
              captions$demography$rangePlotLegend(
                simulationSetName = simulationSetName,
                n = sum(selectedRows),
                parameterClass = parameterClass
              ),
              captions$demography$rangePlotLegend(
                simulationSetName = simulationSetName,
                n = sum(selectedRows),
                parameterClass = parameterClass,
                dataType = "Observed"
              )
            )
          )
        }

        demographyPlot <- plotDemographyRange(
          data = comparisonData,
          metaData = switch(parameterClass,
            "character" = demographyMetaData,
            vpcMetaData
          ),
          dataMapping = demographyDataMapping,
          plotConfiguration = settings$plotConfigurations[[switch(parameterClass,
            "character" = "boxplot",
            "vpcParameterPlot"
          )]],
          parameterClass = parameterClass
        )

        # Save demography range plots
        resultID <- defaultFileNames$resultID(length(demographyResults) + 1, "demography", demographyParameter, parameterName)
        demographyResults[[resultID]] <- saveTaskResults(
          id = resultID,
          plot = demographyPlot,
          plotCaption = captions$demography$rangePlot(
            xParameterName = xParameterCaption,
            yParameterName = yParameterCaption,
            simulationSetName = simulationSetName,
            descriptor = simulationSetDescriptor,
            parameterClass = parameterClass,
            dataSource = dataSource
          )
        )

        # Range or boxplots in log scale
        logLimits <- autoAxesLimits(
          switch(parameterClass,
            "character" = comparisonData[, parameterName],
            c(comparisonData$ymin, comparisonData$median, comparisonData$ymax)
          ),
          scale = "log"
        )
        logTicks <- autoAxesTicksFromLimits(logLimits)

        resultID <- defaultFileNames$resultID(length(demographyResults) + 1, "demography", demographyParameter, parameterName, "log")
        demographyResults[[resultID]] <- saveTaskResults(
          id = resultID,
          plot = tlf::setYAxis(
            plotObject = demographyPlot,
            scale = tlf::Scaling$log,
            axisLimits = logLimits,
            ticks = logTicks
          ),
          plotCaption = captions$demography$rangePlot(
            xParameterName = xParameterCaption,
            yParameterName = yParameterCaption,
            simulationSetName = simulationSetName,
            descriptor = simulationSetDescriptor,
            plotScale = "logarithmic",
            parameterClass = parameterClass,
            dataSource = dataSource
          )
        )
        # If workflow is pediatric, include plots comparing to reference population
        if (!isIncluded(workflowType, PopulationWorkflowTypes$pediatric)) {
          next
        }
        if (isIncluded(simulationSetName, referenceSimulationSetName)) {
          next
        }
        selectedRefRows <- demographyData$simulationSetName %in% referenceSimulationSetName
        referenceData <- switch(parameterClass,
          "character" = demographyData[selectedRefRows, ],
          getDemographyAggregatedData(
            data = demographyData[selectedRefRows, ],
            xParameterName = demographyParameter,
            yParameterName = parameterName,
            bins = settings$bins,
            stairstep = settings$stairstep
          )
        )

        # If reference is preferred plotted as its global range
        if (settings$referenceGlobalRange) {
          referenceData <- switch(parameterClass,
            "character" = demographyData[selectedRefRows, ],
            getDemographyAggregatedData(
              data = demographyData[selectedRefRows, ],
              xParameterName = demographyParameter,
              yParameterName = parameterName,
              bins = c(-Inf, Inf),
              stairstep = TRUE
            )
          )
        }
        # Label for legend
        referenceData$Legend <- captions$demography$rangePlotLegend(
          referenceSimulationSetName,
          n = sum(selectedRefRows),
          parameterClass = parameterClass
        )
        comparisonData <- switch(parameterClass,
          "character" = rbind.data.frame(demographyData[selectedRows, ], referenceData),
          rbind.data.frame(vpcData, referenceData)
        )
        # Keep always reference as second entry for same coloring
        comparisonData$Legend <- factor(
          comparisonData$Legend,
          levels = c(
            captions$demography$rangePlotLegend(
              simulationSetName = simulationSetName,
              n = sum(selectedRows),
              parameterClass = parameterClass
            ),
            captions$demography$rangePlotLegend(
              simulationSetName = referenceSimulationSetName,
              n = sum(selectedRefRows),
              parameterClass = parameterClass
            )
          )
        )

        demographyPlot <- plotDemographyRange(
          data = comparisonData,
          metaData = switch(parameterClass,
            "character" = demographyMetaData,
            vpcMetaData
          ),
          dataMapping = demographyDataMapping,
          plotConfiguration = settings$plotConfigurations[[switch(parameterClass,
            "character" = "comparisonBoxplot",
            "comparisonVpcPlot"
          )]],
          parameterClass = parameterClass
        )

        # Save comparison range plots
        resultID <- defaultFileNames$resultID(length(demographyResults) + 1, "demography", demographyParameter, parameterName)
        demographyResults[[resultID]] <- saveTaskResults(
          id = resultID,
          plot = demographyPlot,
          plotCaption = captions$demography$rangePlot(
            xParameterName = xParameterCaption,
            yParameterName = yParameterCaption,
            simulationSetName = simulationSetName,
            descriptor = simulationSetDescriptor,
            referenceSetName = referenceSimulationSetName,
            parameterClass = parameterClass
          )
        )

        # Comparison range plots in log scale
        logLimits <- autoAxesLimits(
          switch(parameterClass,
            "character" = comparisonData[, parameterName],
            c(comparisonData$ymin, comparisonData$median, comparisonData$ymax)
          ),
          scale = "log"
        )
        logTicks <- autoAxesTicksFromLimits(logLimits)

        resultID <- defaultFileNames$resultID(length(demographyResults) + 1, "demography", demographyParameter, parameterName, "log")
        demographyResults[[resultID]] <- saveTaskResults(
          id = resultID,
          plot = tlf::setYAxis(
            plotObject = demographyPlot,
            scale = tlf::Scaling$log,
            axisLimits = logLimits,
            ticks = logTicks
          ),
          plotCaption = captions$demography$rangePlot(
            xParameterName = xParameterCaption,
            yParameterName = yParameterCaption,
            simulationSetName = simulationSetName,
            descriptor = simulationSetDescriptor,
            plotScale = "logarithmic",
            referenceSetName = referenceSimulationSetName,
            parameterClass = parameterClass
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
#' @param groupName Name of parameter in `data` aggregated for grouping
#' @param bins Either a numeric vector defining bin edges
#' or a numeric value defining the number of bins.
#' @param stairstep A logical value defining if aggregation uses continuous or stairstep plot
#' @return A data.frame of aggregated data
#' @import ospsuite.utils
#' @import dplyr
#' @keywords internal
getDemographyAggregatedData <- function(data,
                                        xParameterName,
                                        yParameterName,
                                        groupName = NULL,
                                        bins = NULL,
                                        stairstep = TRUE) {
  stairstep <- stairstep %||% TRUE
  xParameterBreaks <- bins %||% AggregationConfiguration$bins
  # If bins has 1 value -> number of bins, otherwise values of edges
  if (isOfLength(xParameterBreaks, 1)) {
    # Default aggregation option binUsingQuantiles
    # leverages distribution to get equal number of data points within bins
    if (AggregationConfiguration$binUsingQuantiles) {
      xParameterBreaks <- unique(unname(
        quantile(
          x = data[, xParameterName],
          probs = seq(0, 1, length.out = xParameterBreaks)
        )
      ))
    } else {
      # Split x ranges of equal length
      xParameterBreaks <- unique(seq(
        min(data[, xParameterName], na.rm = TRUE),
        max(data[, xParameterName], na.rm = TRUE),
        length.out = xParameterBreaks
      ))
    }
  }
  # Create "bins" variable corresponding to factor class grouping time points
  # Leverage dplyr package now that ospsuite requires it
  # Also, note that include.lowest is used in cut to include the lowest data point within the bin
  data <- data %>%
    mutate(bins = cut(.data[[xParameterName]], breaks = xParameterBreaks, include.lowest = TRUE))

  # Create "bins" variable corresponding to factor class grouping time points
  aggregatedData <- data %>% group_by(bins)
  aggregatedXData <- aggregatedData %>%
    summarise(
      # If range plot is not stairstep,
      # Get the same use median for x which may not be centered within its bin,
      # however this summary may better represent where data actually is located
      x = AggregationConfiguration$functions$middle(.data[[xParameterName]])
    )
  # If defined use groupName to split aggregation between groups
  if (!is.null(groupName)) {
    eval(parse(text = paste0("aggregatedData <- data %>% group_by(bins,", groupName, ")")))
  }
  # Remove unwanted message: `summarise()` has grouped output by ...
  suppressMessages({
    aggregatedData <- aggregatedData %>%
      summarise(
        median = AggregationConfiguration$functions$middle(.data[[yParameterName]]),
        ymin = AggregationConfiguration$functions$ymin(.data[[yParameterName]]),
        ymax = AggregationConfiguration$functions$ymax(.data[[yParameterName]])
      )
  })
  aggregatedData <- full_join(aggregatedXData, aggregatedData, by = "bins", multiple = "all")
  # In case, enforce ordering of the x values,
  # aiming at preventing lines going back and forth to the x values
  aggregatedData <- as.data.frame(aggregatedData[order(aggregatedData$bins), ])

  if (stairstep) {
    # Use bin level number to associate the correct edge bins
    aggregatedDataMin <- aggregatedData %>% mutate(x = xParameterBreaks[as.numeric(bins)])
    aggregatedDataMax <- aggregatedData %>% mutate(x = xParameterBreaks[as.numeric(bins) + 1])
    aggregatedData <- bind_rows(aggregatedDataMin, aggregatedDataMax)
    aggregatedData <- aggregatedData[order(aggregatedData$bins), ]
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
#' @param demographyResults A list of `TaskResults` objects
#' @return A list of `TaskResults` objects
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
            ylabel = reEnv$demographyHistogramLabel,
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
#' @param demographyResults A list of `TaskResults` objects
#' @return A list of `TaskResults` objects
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
          ylabel = reEnv$demographyHistogramLabel,
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
          ylabel = reEnv$demographyHistogramLabel,
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
        paste("observed", parameterCaption),
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

#' @title plotDemographyRange
#' @description
#' Plot demography range plot using visual predictive check style if data is numeric,
#' or using box whisker plot if data is categorical
#' @param data data.frame
#' @param metaData list of metaData about `data`
#' @param dataMapping A `TimeProfileDataMapping` or `BoxWhiskerDataMapping` object
#' @param plotConfiguration `PlotConfiguration`  object
#' @param parameterClass Class of the parameter, either "numeric" or "character"
#' @return ggplot object
#' @export
#' @import tlf
#' @import ggplot2
#' @import ospsuite.utils
plotDemographyRange <- function(data,
                                metaData = NULL,
                                dataMapping = NULL,
                                plotConfiguration = NULL,
                                parameterClass = "numeric") {
  mapLabels <- tlf:::.getAesStringMapping(dataMapping)
  # TODO: once range plots included in tlf, switch to range plots
  vpcPlotConfiguration <- plotConfiguration %||% switch(parameterClass,
    "character" = tlf::BoxWhiskerPlotConfiguration$new(
      data = data,
      metaData = metaData,
      dataMapping = dataMapping
    ),
    tlf::TimeProfilePlotConfiguration$new(
      data = data,
      metaData = metaData,
      dataMapping = dataMapping
    )
  )

  ribbonProperties <- tlf:::.getAestheticValuesFromConfiguration(
    n = length(unique(data[[mapLabels$fill]])),
    plotConfigurationProperty = vpcPlotConfiguration$ribbons,
    propertyNames = c("fill", "color", "alpha")
  )
  lineProperties <- tlf:::.getAestheticValuesFromConfiguration(
    n = length(unique(data[[mapLabels$color]])),
    plotConfigurationProperty = vpcPlotConfiguration$lines,
    propertyNames = c("color", "linetype", "size")
  )
  # 2nd value get reference coloring
  ribbonProperties$fill[2] <- reEnv$referenceFill
  lineProperties$color[2] <- reEnv$referenceColor

  if (isIncluded(parameterClass, "character")) {
    vpcPlotConfiguration$ribbons$fill <- ribbonProperties$fill
    return(tlf::plotBoxWhisker(
      data = data,
      metaData = metaData,
      dataMapping = dataMapping,
      plotConfiguration = vpcPlotConfiguration
    ))
  }
  vpcPlot <- tlf::initializePlot(vpcPlotConfiguration) +
    ggplot2::geom_ribbon(
      data = data,
      mapping = ggplot2::aes(
        x = .data[[mapLabels$x]],
        ymin = .data[[mapLabels$ymin]],
        ymax = .data[[mapLabels$ymax]],
        color = .data[[mapLabels$color]],
        fill = .data[[mapLabels$fill]]
      ),
      color = ribbonProperties$color[1],
      alpha = ribbonProperties$alpha[1],
      linetype = tlf::Linetypes$blank
    ) +
    ggplot2::geom_line(
      data = data,
      mapping = ggplot2::aes(
        x = .data[[mapLabels$x]],
        y = .data[[mapLabels$y]],
        color = .data[[mapLabels$color]]
      ),
      linetype = lineProperties$linetype[1],
      size = lineProperties$size[1]
    ) +
    ggplot2::scale_fill_manual(values = ribbonProperties$fill) +
    ggplot2::scale_color_manual(values = lineProperties$color) +
    labs(color = NULL, fill = NULL)
  return(vpcPlot)
}
