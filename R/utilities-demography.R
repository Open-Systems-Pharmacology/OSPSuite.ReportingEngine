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
#' @importFrom ospsuite.utils %||%
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

  demographyAcrossPopulations <- getDemographyAcrossPopulations(structureSets)
  demographyData <- demographyAcrossPopulations$data
  demographyMetaData <- demographyAcrossPopulations$metaData
  simulationSetNames <- unique(as.character(demographyData$simulationSetName))

  checkIsIncluded(xParameters, names(demographyData), nullAllowed = TRUE, groupName = "demography variable names across simulation sets")
  checkIsIncluded(yParameters, names(demographyData), nullAllowed = TRUE, groupName = "demography variable names across simulation sets")
  xParameters <- intersect(xParameters, names(demographyData))
  yParameters <- intersect(yParameters, names(demographyData))

  if (workflowType %in% PopulationWorkflowTypes$pediatric) {
    referenceSimulationSetName <- getReferencePopulationName(structureSets)
  }

  if (isOfLength(xParameters, 0)) {
    # Pediatric: comparison histogram
    if (workflowType %in% c(PopulationWorkflowTypes$pediatric)) {
      for (parameterName in yParameters) {
        resultID <- defaultFileNames$resultID(length(demographyResults) + 1, "demography", parameterName)
        parameterCaption <- demographyMetaData[[parameterName]]$dimension

        histogramMapping <- tlf::HistogramDataMapping$new(
          x = parameterName,
          fill = "simulationSetName"
        )
        demographyHistogram <- plotDemographyHistogram(
          data = demographyData,
          metaData = demographyMetaData,
          dataMapping = histogramMapping,
          plotConfiguration = settings$plotConfigurations[["histogram"]],
          bins = settings$bins %||% AggregationConfiguration$bins
        )

        # Save results
        demographyResults[[resultID]] <- saveTaskResults(
          id = resultID,
          plot = demographyHistogram,
          plotCaption = captions$demography$histogram(
            parameterCaption,
            simulationSetNames,
            simulationSetDescriptor
          )
        )
      }
    }
    # Parallel and Ratio: histograms per population
    if (workflowType %in% c(PopulationWorkflowTypes$parallelComparison, PopulationWorkflowTypes$ratioComparison)) {
      for (parameterName in yParameters) {
        parameterCaption <- demographyMetaData[[parameterName]]$dimension

        histogramMapping <- tlf::HistogramDataMapping$new(
          x = parameterName,
          fill = "simulationSetName"
        )
        for (simulationSetName in simulationSetNames) {
          resultID <- defaultFileNames$resultID(length(demographyResults) + 1, "demography", parameterName)
          demographyDataByPopulation <- demographyData[demographyData$simulationSetName %in% simulationSetName, ]

          demographyHistogram <- plotDemographyHistogram(
            data = demographyDataByPopulation,
            metaData = demographyMetaData,
            dataMapping = histogramMapping,
            plotConfiguration = settings$plotConfigurations[["histogram"]],
            bins = settings$bins %||% AggregationConfiguration$bins
          )

          # Save results
          demographyResults[[resultID]] <- saveTaskResults(
            id = resultID,
            plot = demographyHistogram,
            plotCaption = captions$demography$histogram(
              parameterCaption,
              simulationSetName,
              simulationSetDescriptor
            )
          )
        }
      }
    }
    return(demographyResults)
  }

  for (demographyParameter in xParameters) {
    # Categorical variables won't be plotted
    if (demographyMetaData[[demographyParameter]]$class %in% "character") {
      next
    }
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

          xParameterCaption <- vpcMetaData$x$dimension
          yParameterCaption <- vpcMetaData$median$dimension

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
              limits = vpcLogLimits,
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
            limits = vpcLogLimits,
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

getDemographyAcrossPopulations <- function(structureSets) {
  demographyAcrossPopulations <- NULL
  for (structureSet in structureSets)
  {
    population <- loadWorkflowPopulation(structureSet$simulationSet)
    simulation <- ospsuite::loadSimulation(structureSet$simulationSet$simulationFile, loadFromCache = TRUE)
    populationTable <- getPopulationAsDataFrame(population, simulation)

    fullDemographyTable <- cbind.data.frame(
      simulationSetName = structureSet$simulationSet$simulationSetName,
      populationTable
    )
    # Prevent crash when merging populations with different parameters by filling unexisting with NA
    newNamesDemographyAcrossPopulations <- setdiff(names(fullDemographyTable), names(demographyAcrossPopulations))
    newNamesDemographyTable <- setdiff(names(demographyAcrossPopulations), names(fullDemographyTable))
    if (!is.null(demographyAcrossPopulations)) {
      demographyAcrossPopulations[, newNamesDemographyAcrossPopulations] <- NA
    }
    fullDemographyTable[, newNamesDemographyTable] <- NA

    demographyAcrossPopulations <- rbind.data.frame(
      demographyAcrossPopulations,
      fullDemographyTable
    )
  }

  metaData <- getPopulationMetaData(population, simulation, structureSet$parameterDisplayPaths)

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
#' @param yParameterName Name of parameter in `data` aggreated in y axis of plot
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

getReferencePopulationName <- function(structureSets) {
  allSimulationReferences <- sapply(structureSets, function(structureSet) {
    structureSet$simulationSet$referencePopulation
  })
  validateIsOfLength(allSimulationReferences[allSimulationReferences], 1)
  referencePopulationName <- structureSets[[which(allSimulationReferences)]]$simulationSet$simulationSetName
  return(referencePopulationName)
}

#' @title plotDemographyHistogram
#' @description Plot histograms for demography parameters
#' @param data data.frame
#' @param metaData list of metaData about `data`
#' @param dataMapping `HistogramDataMapping` class object
#' @param plotConfiguration `PlotConfiguration` class object
#' @param bins number of bins for continuous parameters
#' @return ggplot object
#' @export
#' @import ospsuite
#' @import tlf
#' @import ggplot2
#' @importFrom ospsuite.utils %||%
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

  if (metaData[[dataMapping$x]]$class %in% "character") {
    data[, dataMapping$x] <- factor(data[, dataMapping$x])
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
        stat = "count"
      )
  } else {
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
  }
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
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Get the list of parameters in x-axis for range plots
#' getXParametersForDemogrpahyPlot(workflow = myWorkflow)
#' }
#'
getXParametersForDemogrpahyPlot <- function(workflow) {
  validateIsOfType(workflow, "PopulationWorkflow")
  return(workflow$plotDemography$xParameters)
}

#' @title getYParametersForDemogrpahyPlot
#' @param workflow `PopulationWorkflow` R6 class object
#' @return list of y parameters used for demography histogram and range plots
#' @import ospsuite.utils
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Get the list of parameters in x-axis for range plots
#' getYParametersForDemogrpahyPlot(workflow = myWorkflow)
#' }
#'
getYParametersForDemogrpahyPlot <- function(workflow) {
  validateIsOfType(workflow, "PopulationWorkflow")
  return(workflow$plotDemography$yParameters %||% DemographyDefaultParameters)
}

#' @title setXParametersForDemogrpahyPlot
#' @description Set x parameters for range plots of demography plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of demography parameters to be used as x-parameters
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Set parameters in x-axis for range plots
#' setXParametersForDemogrpahyPlot(
#'   workflow = myWorkflow,
#'   parameters = StandardPath
#' )
#' }
#'
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

#' @title addXParametersForDemogrpahyPlot
#' @description Append x parameters for range plots of demography plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of demography parameters to be used as x-parameters
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Get the list of parameters in x-axis for range plots
#' addXParametersForDemogrpahyPlot(
#'   workflow = myWorkflow,
#'   parameters = StandardPath$GestationalAge
#' )
#' }
#'
addXParametersForDemogrpahyPlot <- function(workflow, parameters) {
  updatedParameters <- c(getXParametersForDemogrpahyPlot(workflow), parameters)
  setXParametersForDemogrpahyPlot(workflow, updatedParameters)
}

#' @title setYParametersForDemogrpahyPlot
#' @description Set y-parameters for histograms and range plots of demography plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of demography parameters to be used as y-parameters
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Set parameters in y-axis for range plots and histograms
#' setYParametersForDemogrpahyPlot(
#'   workflow = myWorkflow,
#'   parameters = StandardPath
#' )
#' }
#'
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

#' @title addYParametersForDemogrpahyPlot
#' @description Append y parameters for range plots of demography plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of demography parameters to be used as x-parameters
#' @export
#' @family workflow helpers
#' @examples \dontrun{
#'
#' # A workflow object needs to be created first
#' myWorkflow <- PopulationWorkflow$new(worflowType, workflowFolder, simulationSets)
#'
#' # Add parameters in y-axis for range plots and histograms
#' addYParametersForDemogrpahyPlot(
#'   workflow = myWorkflow,
#'   parameters = StandardPath$GestationalAge
#' )
#' }
#'
addYParametersForDemogrpahyPlot <- function(workflow, parameters) {
  updatedParameters <- c(getYParametersForDemogrpahyPlot(workflow), parameters)
  setYParametersForDemogrpahyPlot(workflow, updatedParameters)
}

getPopulationAsDataFrame <- function(population, simulation) {
  populationTable <- ospsuite::populationToDataFrame(population)
  allParameters <- ospsuite::getAllParametersMatching(population$allParameterPaths, simulation)

  for (parameter in allParameters) {
    populationTable[, parameter$path] <- ospsuite::toDisplayUnit(parameter, populationTable[, parameter$path])
  }
  return(populationTable)
}

getPopulationMetaData <- function(population, simulation, parameterDisplayPaths) {
  metaData <- list()
  allParameters <- ospsuite::getAllParametersMatching(population$allParameterPaths, simulation)

  for (covariate in population$allCovariateNames) {
    metaData[[covariate]] <- list(
      dimension = covariate,
      unit = "",
      class = class(population$getCovariateValues(covariate))
    )
  }
  for (parameter in allParameters) {
    metaData[[parameter$path]] <- list(
      dimension = getSimulationParameterDisplayPaths(parameter$path, simulation, parameterDisplayPaths),
      unit = parameter$displayUnit,
      class = class(population$getParameterValues(parameter$path))
    )
  }
  return(metaData)
}
