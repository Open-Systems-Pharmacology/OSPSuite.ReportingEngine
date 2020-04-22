#' @title plotMeanMassBalance
#' @description Plot mass balance diagnostics time profiles, cumulative time profiles,
#' normalized time profiles and cumulative normalized time profiles
#' @param structureSet `SimulationStructure` R6 class object
#' @param logFolder folder where the logs are saved
#' @param plotConfigurations List of `PlotConfiguration` R6 class objects
#' @param selectedCompoundNames Options to be defined
#' (e.g. how to group the plot results, or which molecule to exclude)
#' @return list of `ggplot` objects
#' @export
#' @import tlf
#' @import ospsuite
#' @import utils
plotMeanMassBalance <- function(structureSet,
                                logFolder = getwd(),
                                plotConfigurations = NULL,
                                selectedCompoundNames = NULL) {
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)

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
  selectedCompoundNames <- selectedCompoundNames %||% relevantCompoundNames
  validateIsIncluded(selectedCompoundNames, relevantCompoundNames)

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
  massBalancePlots[["timeProfile"]] <- plotMassBalanceTimeProfile(
    data = simulationResultsOutputByGroup,
    metaData = metaDataOutputByGroup,
    dataMapping = tlf::XYGDataMapping$new(
      x = "Time",
      y = "Amount",
      color = "Legend"
    ),
    plotConfiguration = plotConfigurations[["timeProfile"]]
  )

  massBalancePlots[["cumulativeTimeProfile"]] <- plotMassBalanceCumulativeTimeProfile(
    data = simulationResultsOutputByGroup,
    metaData = metaDataOutputByGroup,
    dataMapping = tlf::XYGDataMapping$new(
      x = "Time",
      y = "Amount",
      fill = "Legend"
    ),
    plotConfiguration = plotConfigurations[["cumulativeTimeProfile"]]
  )

  massBalancePlots[["normalizedTimeProfile"]] <- plotMassBalanceTimeProfile(
    data = simulationResultsOutputByGroup,
    metaData = metaDataOutputByGroup,
    dataMapping = tlf::XYGDataMapping$new(
      x = "Time",
      y = "NormalizedAmount",
      color = "Legend"
    ),
    plotConfiguration = plotConfigurations[["normalizedTimeProfile"]]
  )

  massBalancePlots[["normalizedCumulativeTimeProfile"]] <- plotMassBalanceCumulativeTimeProfile(
    data = simulationResultsOutputByGroup,
    metaData = metaDataOutputByGroup,
    dataMapping = tlf::XYGDataMapping$new(
      x = "Time",
      y = "NormalizedAmount",
      fill = "Legend"
    ),
    plotConfiguration = plotConfigurations[["normalizedCumulativeTimeProfile"]]
  )

  massBalancePlots[["pieChart"]] <- plotMassBalancePieChart(
    data = simulationResultsOutputByGroup,
    metaData = metaDataOutputByGroup,
    dataMapping = tlf::XYGDataMapping$new(
      x = "Time",
      y = "NormalizedAmount",
      fill = "Legend"
    ),
    plotConfiguration = plotConfigurations[["pieChart"]]
  )

  return(list(
    plots = massBalancePlots,
    tables = simulationResultsOutputByGroup
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

  # TO DO: use the new version of tlf to get this plot
  timeVsAmountPlot <- ggplot2::ggplot()
  timeVsAmountPlot <- plotConfiguration$setPlotBackground(timeVsAmountPlot)
  timeVsAmountPlot <- plotConfiguration$setPlotLabels(timeVsAmountPlot)

  timeVsAmountPlot <- timeVsAmountPlot + ggplot2::geom_line(
    data = data,
    mapping = ggplot2::aes_string(
      x = timeVsAmountDataMapping$x,
      y = timeVsAmountDataMapping$y,
      color = timeVsAmountDataMapping$groupMapping$color$label
    )
  )
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

  # TO DO: use the new version of tlf to get this plot
  timeVsAmountPlot <- ggplot2::ggplot()
  timeVsAmountPlot <- plotConfiguration$setPlotBackground(timeVsAmountPlot)
  timeVsAmountPlot <- plotConfiguration$setPlotLabels(timeVsAmountPlot)

  timeVsAmountPlot <- timeVsAmountPlot + ggplot2::geom_area(
    data = data,
    mapping = ggplot2::aes_string(
      x = timeVsAmountDataMapping$x,
      y = timeVsAmountDataMapping$y,
      fill = timeVsAmountDataMapping$groupMapping$fill$label
    ),
    position = ggplot2::position_stack(),
    alpha = 0.8 # TO DO: Define this value as a setting from the plot configuration
  )
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
    y = "Amount",
    fill = "Legend"
  )

  timeFilter <- data[, pieChartDataMapping$x] == max(data[, pieChartDataMapping$x])
  pieChartData <- data[timeFilter, ]

  # TO DO: remove Testing Data Watermark when plot is complete
  # plotConfiguration <- plotConfiguration %||% tlf::PlotConfiguration$new(
  #  data = data,
  #  metaData = metaData,
  #  dataMapping = pieChartDataMapping,
  #  watermark = "Testing !"
  # )
  # TO DO: watermark can't be used for polar plots...
  # Need to check if there are other ways

  # TO DO: use the new version of tlf to get this plot
  pieChartPlot <- ggplot2::ggplot()
  # pieChartPlot <- plotConfiguration$setPlotBackground(pieChartPlot)

  pieChartPlot <- pieChartPlot +
    ggplot2::geom_bar(
      data = pieChartData,
      mapping = ggplot2::aes_string(
        x = pieChartDataMapping$x,
        y = pieChartDataMapping$y,
        fill = pieChartDataMapping$groupMapping$fill$label
      ),
      width = 1,
      stat = "identity",
      alpha = 0.8 # TO DO: Define this value as a setting from the plot configuration)
    ) + coord_polar("y", start = 0) + xlab("") + ylab("") +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    )

  return(pieChartPlot)
}
