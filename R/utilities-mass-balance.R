#' @title plotMeanMassBalance
#' @description Plot mass balance diagnostics time profiles, cumulative time profiles,
#' normalized time profiles and cumulative normalized time profiles
#' @param structureSet `SimulationStructure` R6 class object
#' @param logFolder folder where the logs are saved
#' @param settings Options to be defined
#' (e.g. how to group the plot results, or which molecule to exclude)
#' @return list of `ggplot` objects
#' @import tlf
#' @import ospsuite
#' @import utils
#' @keywords internal
plotMeanMassBalance <- function(structureSet,
                                logFolder = getwd(),
                                settings = NULL) {
  re.tStoreFileMetadata(access = "read", filePath = structureSet$simulationSet$simulationFile)
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet, loadFromCache = TRUE)

  # Get drug mass to perform the drugmass normalized plot
  applications <- ospsuite::getContainer("Applications", simulation)
  appliedMoleculePaths <- ospsuite::getAllMoleculePathsIn(applications)

  appliedMolecules <- ospsuite::getAllMoleculesMatching(appliedMoleculePaths, simulation)
  drugMass <- sum(sapply(appliedMolecules, function(molecule) {
    molecule$value
  }))

  # Get all the relevant compounds
  organism <- ospsuite::getContainer("Organism", simulation)
  allCompoundNames <- c(simulation$allFloatingMoleculeNames(), simulation$allStationaryMoleculeNames())
  relevantCompoundNames <- allCompoundNames[!sapply(allCompoundNames, function(compoundName) {
    compoundName %in% simulation$allEndogenousStationaryMoleculeNames()
  })]

  # User defined coumpound selection
  selectedCompoundNames <- settings$selectedCompoundNames %||% relevantCompoundNames
  ospsuite.utils::validateIsIncluded(selectedCompoundNames, relevantCompoundNames)

  # Get all the molecule paths (with dimension=amount) of the selected/relevant coumpounds
  molecules <- ospsuite::getAllMoleculesMatching(paste0("**|", selectedCompoundNames), organism)

  # Clear concentration output in case any concentrations are still included
  ospsuite::clearOutputs(simulation)
  for (molecule in molecules) {
    addOutputs(quantitiesOrPaths = molecule, simulation = simulation)
  }

  simulationResults <- ospsuite::runSimulation(simulation)
  simulationResultsOutput <- ospsuite::getOutputValues(
    simulationResults = simulationResults,
    quantitiesOrPaths = simulationResults$allQuantityPaths
  )

  # Create a data.frame with full string path and separates the last 3 elements of paths
  # to perform filtering and grouping of for the final plot
  pathsArray <- NULL
  for (path in simulationResults$allQuantityPaths) {
    endOfPath <- utils::tail(ospsuite::toPathArray(path), 3)
    pathsArray <- rbind.data.frame(
      pathsArray,
      data.frame(
        "parentCompartmentName" = endOfPath[1],
        "subCompartmentName" = endOfPath[2],
        "compoundName" = endOfPath[3],
        "path" = path,
        stringsAsFactors = FALSE
      )
    )
  }

  # In the Matlab version, there are multiple groupings performed at that point:
  # 1) Saliva is excluded from the mass balance calculation
  # 2) Lumen is a parent compartment that regroups many sub-compartments but Feces
  # 3) All the rest of the sub-compartments are used normally

  # 1) Exclude Saliva
  salivaExclusionCondition <- !pathsArray[, "parentCompartmentName"] %in% "Saliva"
  pathsArray <- pathsArray[salivaExclusionCondition, ]

  # 2) Regroup the lumen sub-compartment but Feces
  lumenGroupingCondition <- pathsArray[, "parentCompartmentName"] %in% "Lumen" & !pathsArray[, "subCompartmentName"] %in% "Feces"
  pathsArray[lumenGroupingCondition, "subCompartmentName"] <- "Lumen"

  # Factors could not be used for steps 1 and 2 but are needed in the next step
  pathsArray <- as.data.frame(lapply(pathsArray, function(pathsArrayColumn) {
    as.factor(pathsArrayColumn)
  }))

  # Aggregate Data by compound and compartment
  simulationResultsOutputByGroup <- NULL

  for (compoundLevel in levels(pathsArray[, "compoundName"])) {
    for (compartmentLevel in levels(pathsArray[, "subCompartmentName"])) {
      pathFilter <- pathsArray[, "compoundName"] %in% compoundLevel & pathsArray[, "subCompartmentName"] %in% compartmentLevel
      # as.character() ensures that levels filtered out won't still be added to the aggregation
      pathNamesFiltered <- as.character(pathsArray[pathFilter, "path"])

      # cbind(..., 0) prevents rowSums to crash if only one column is filtered
      aggregatedData <- rowSums(cbind.data.frame(
        simulationResultsOutput$data[, pathNamesFiltered, drop = FALSE],
        data.frame(dummyVariable = 0)
      ))

      if (max(aggregatedData) > 0) {
        simulationResultsOutputByGroup <- rbind.data.frame(
          simulationResultsOutputByGroup,
          cbind.data.frame(
            "Time" = toUnit("Time", simulationResultsOutput$data[, "Time"], structureSet$simulationSet$timeUnit),
            "Amount" = aggregatedData,
            "NormalizedAmount" = aggregatedData / drugMass,
            "Legend" = paste0(compoundLevel, " - ", compartmentLevel)
          )
        )
      }
    }
  }

  # TO DO: Get meta data from input or settings
  metaDataOutputByGroup <- list(
    "Time" = list(
      dimension = "Time",
      unit = structureSet$simulationSet$timeUnit
    ),
    "Amount" = list(
      dimension = appliedMolecules[[1]]$dimension,
      unit = appliedMolecules[[1]]$unit
    ),
    "NormalizedAmount" = list(
      dimension = appliedMolecules[[1]]$dimension,
      unit = "fraction of drugmass"
    )
  )

  massBalancePlots <- list()
  massBalanceCaptions <- list()
  massBalancePlots[["timeProfile"]] <- plotMassBalanceTimeProfile(
    data = simulationResultsOutputByGroup,
    metaData = metaDataOutputByGroup,
    dataMapping = tlf::XYGDataMapping$new(
      x = "Time",
      y = "Amount",
      color = "Legend"
    ),
    plotConfiguration = settings$plotConfigurations[["timeProfile"]]
  )

  massBalancePlots[["cumulativeTimeProfile"]] <- plotMassBalanceCumulativeTimeProfile(
    data = simulationResultsOutputByGroup,
    metaData = metaDataOutputByGroup,
    dataMapping = tlf::XYGDataMapping$new(
      x = "Time",
      y = "Amount",
      fill = "Legend"
    ),
    plotConfiguration = settings$plotConfigurations[["cumulativeTimeProfile"]]
  )

  massBalancePlots[["normalizedTimeProfile"]] <- plotMassBalanceTimeProfile(
    data = simulationResultsOutputByGroup,
    metaData = metaDataOutputByGroup,
    dataMapping = tlf::XYGDataMapping$new(
      x = "Time",
      y = "NormalizedAmount",
      color = "Legend"
    ),
    plotConfiguration = settings$plotConfigurations[["normalizedTimeProfile"]]
  )

  massBalancePlots[["normalizedCumulativeTimeProfile"]] <- plotMassBalanceCumulativeTimeProfile(
    data = simulationResultsOutputByGroup,
    metaData = metaDataOutputByGroup,
    dataMapping = tlf::XYGDataMapping$new(
      x = "Time",
      y = "NormalizedAmount",
      fill = "Legend"
    ),
    plotConfiguration = settings$plotConfigurations[["normalizedCumulativeTimeProfile"]]
  )

  massBalancePlots[["pieChart"]] <- plotMassBalancePieChart(
    data = simulationResultsOutputByGroup,
    metaData = metaDataOutputByGroup,
    dataMapping = tlf::XYGDataMapping$new(
      x = "Time",
      y = "NormalizedAmount",
      fill = "Legend"
    ),
    plotConfiguration = settings$plotConfigurations[["pieChart"]]
  )

  timeCaption <- formatNumerics(
    max(simulationResultsOutputByGroup$Time),
    digits = settings$digits,
    scientific = settings$scientific
  )

  massBalanceCaptions <- list(
    timeProfile = captions$massBalance$timeProfile(),
    cumulativeTimeProfile = captions$massBalance$cumulativeTimeProfile(),
    normalizedTimeProfile = captions$massBalance$normalizedTimeProfile(),
    normalizedCumulativeTimeProfile = captions$massBalance$normalizedCumulativeTimeProfile(),
    pieChart = captions$massBalance$pieChart(timeCaption, metaDataOutputByGroup$Time$unit)
  )

  return(list(
    plots = massBalancePlots,
    tables = list(timeProfiles = simulationResultsOutputByGroup),
    captions = massBalanceCaptions
  ))
}


#' @title plotMassBalanceTimeProfile
#' @description Plot mass balance time profile
#' @param data data.frame
#' @param metaData meta data on `data`
#' @param dataMapping `XYGDataMapping` R6 class object from `tlf` library
#' @param plotConfiguration `PlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
#' @import ggplot2
#' @import utils
plotMassBalanceTimeProfile <- function(data,
                                       metaData = NULL,
                                       dataMapping = NULL,
                                       plotConfiguration = NULL) {
  timeVsAmountDataMapping <- dataMapping %||% tlf::XYGDataMapping$new(
    x = "Time",
    y = "Amount",
    color = "Legend"
  )

  plotConfiguration <- plotConfiguration %||% tlf::PlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = timeVsAmountDataMapping
  )

  timeVsAmountPlot <- tlf::initializePlot(plotConfiguration)

  timeVsAmountPlot <- timeVsAmountPlot + ggplot2::geom_line(
    data = data,
    mapping = ggplot2::aes_string(
      x = timeVsAmountDataMapping$x,
      y = timeVsAmountDataMapping$y,
      color = timeVsAmountDataMapping$groupMapping$color$label
    )
  ) + ggplot2::theme(legend.title = ggplot2::element_blank())
  return(timeVsAmountPlot)
}

#' @title plotMassBalanceCumulativeTimeProfile
#' @description Plot mass balance time profile
#' @param data data.frame
#' @param metaData meta data on `data`
#' @param dataMapping `XYGDataMapping` R6 class object from `tlf` library
#' @param plotConfiguration `PlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of time profile for mean model workflow
#' @export
#' @import tlf
#' @import ggplot2
#' @import utils
plotMassBalanceCumulativeTimeProfile <- function(data,
                                                 metaData = NULL,
                                                 dataMapping = NULL,
                                                 plotConfiguration = NULL) {
  timeVsAmountDataMapping <- dataMapping %||% tlf::XYGDataMapping$new(
    x = "Time",
    y = "Amount",
    fill = "Legend"
  )

  plotConfiguration <- plotConfiguration %||% tlf::PlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = timeVsAmountDataMapping
  )

  timeVsAmountPlot <- tlf::initializePlot(plotConfiguration)

  timeVsAmountPlot <- timeVsAmountPlot + ggplot2::geom_area(
    data = data,
    mapping = ggplot2::aes_string(
      x = timeVsAmountDataMapping$x,
      y = timeVsAmountDataMapping$y,
      fill = timeVsAmountDataMapping$groupMapping$fill$label
    ),
    position = ggplot2::position_stack(),
    alpha = 0.8 # TO DO: Define this value as a setting from the plot configuration
  ) + ggplot2::theme(legend.title = ggplot2::element_blank())
  return(timeVsAmountPlot)
}

#' @title plotMassBalancePieChart
#' @description Plot mass balance PieChart
#' @param data data.frame
#' @param metaData meta data on `data`
#' @param dataMapping `XYGDataMapping` R6 class object from `tlf` library
#' @param plotConfiguration `PlotConfiguration` R6 class object from `tlf` library
#' @return ggplot object of piechart for mean model workflow
#' @export
#' @import tlf
#' @import ggplot2
#' @import utils
plotMassBalancePieChart <- function(data,
                                    metaData = NULL,
                                    dataMapping = NULL,
                                    plotConfiguration = NULL) {
  pieChartDataMapping <- dataMapping %||% tlf::XYGDataMapping$new(
    x = "Time",
    y = "NormalizedAmount",
    fill = "Legend"
  )

  timeFilter <- data[, pieChartDataMapping$x] == max(data[, pieChartDataMapping$x])
  pieChartData <- data[timeFilter, ]
  # Legend captions need to includes normalized amount as percent
  pieChartData$Legend <- paste(
    pieChartData$Legend,
    " (", round(100 * pieChartData[, pieChartDataMapping$y], digits = 1), "%)"
  )
  # Ensure that the colors and legend match previous mass balance plots by re-ordering Legend
  pieChartData$Legend <- reorder(pieChartData$Legend, as.numeric(factor(data[timeFilter, "Legend"])))

  # Caution:
  # Watermark relies on ggplot2::annotation_custom which is not compatible with ggplot2::coord_polar
  # As a consequence, the polar plot needs to be saved as a grob first, ie as a ggplot grid image
  # Then, the grob is added as a layer of a tlf empty plot which can include a Watermark
  pieChartCorePlot <- ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = pieChartData,
      mapping = ggplot2::aes_string(
        x = pieChartDataMapping$x,
        y = pieChartDataMapping$y,
        fill = pieChartDataMapping$groupMapping$fill$label
      ),
      width = 1,
      stat = "identity",
      alpha = 0.8
    ) +
    coord_polar("y", start = 0) +
    ggplot2::theme_void() +
    xlab("") +
    ylab("") +
    ggplot2::theme(legend.title = ggplot2::element_blank())

  pieChartGrob <- ggplot2::ggplotGrob(pieChartCorePlot)

  pieChartPlot <- tlf::initializePlot(plotConfiguration)
  pieChartPlot <- pieChartPlot + ggplot2::annotation_custom(pieChartGrob)

  return(pieChartPlot)
}
