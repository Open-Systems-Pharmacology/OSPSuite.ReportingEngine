#' @title plotMeanMassBalance
#' @description Plot mass balance diagnostics time profiles, cumulative time profiles,
#' normalized time profiles and cumulative normalized time profiles
#' @param structureSet `SimulationStructure` R6 class object
#' @param plotConfigurations List of `PlotConfiguration` R6 class objects
#' @param selectedCompoundNames Options to be defined
#' (e.g. how to group the plot results, or which molecule to exclude)
#' @return list of `ggplot` objects
#' @export
#' @import tlf
#' @import ospsuite
#' @import utils
plotMeanMassBalance <- function(structureSet,
                                plotConfigurations = NULL,
                                selectedCompoundNames = NULL) {
  simulation <- ospsuite::loadSimulation(structureSet$simulationSet$simulationFile)
  molecules <- ospsuite::getAllMoleculesMatching(paths = "**", container = simulation)
  moleculesPaths <- sapply(molecules, function(x) {
    x$path
  })
  # TO DO: Check if Application need to be part of the paths, or need to be included as the drugmass reference
  # applicationMolecules <- ospsuite::getAllMoleculesMatching(paths = "Applications|**", container = simulation)
  # applicationMoleculesPaths <- sapply(newMolecules, function(x){x$path})

  # Get all the compartment and coumpound names
  # The plot will aggregate on them
  allCompoundNames <- NULL
  allCompartmentNames <- NULL
  for (moleculePath in moleculesPaths) {
    moleculeArray <- utils::tail(ospsuite::toPathArray(moleculePath), 2)
    allCompoundNames <- cbind(allCompoundNames, moleculeArray[2])
    allCompartmentNames <- cbind(allCompartmentNames, moleculeArray[1])
  }
  allCompoundNames <- levels(as.factor(allCompoundNames))
  allCompartmentNames <- levels(as.factor(allCompartmentNames))

  # TO DO: add user defined inclusion/exclusion of compound for the plot
  selectedCompoundNames <- selectedCompoundNames %||% allCompoundNames
  validateIsIncluded(selectedCompoundNames, allCompoundNames)

  # Clear concentration output and add only molecules paths to the simulation output
  ospsuite::clearOutputs(simulation)
  for (molecule in molecules) {
    addOutputs(quantitiesOrPaths = molecule, simulation = simulation)
  }

  simulationResults <- ospsuite::runSimulation(simulation)
  simulationResultsOutputTLF <- getOutputValuesTLF(
    simulationResults = simulationResults,
    quantitiesOrPaths = simulationResults$allQuantityPaths
  )

  # TO DO: Check that the drugMass method is correct
  # Do we need to include Applications, how ?
  drugAmount <- sum(sapply(molecules, function(x) {
    x$value
  }))

  # Pre-filter data to get only paths which have >0 amount all along the simulation
  # The goal is to simplify the following loops that may take long
  pathFilter <- sapply(simulationResultsOutputTLF$data, max)
  pathNames <- names(simulationResultsOutputTLF$data)
  pathNames <- pathNames[pathFilter > 0]

  # TO DO: add user defined compartment grouping ?
  # Or default grouping eg for saliva and salivagland
  groupOfCompartments <- allCompartmentNames
  groupOfCompartments <- groupOfCompartments[!allCompartmentNames %in% c(
    "Caecum", "ColonAscendens", "ColonDescendens", "ColonSigmoid", "ColonTransversum",
    "Duodenum", "LowerIleum", "LowerJejunum", "Rectum", "Stomach", "UpperIleum", "UpperJejunum"
  )]
  # TO DO: check that these compartments are considered as Lumen or something else
  lumenCompartmentNames <- c(
    "Lumen", "Caecum", "ColonAscendens", "ColonDescendens", "ColonSigmoid", "ColonTransversum",
    "Duodenum", "LowerIleum", "LowerJejunum", "Rectum", "Stomach", "UpperIleum", "UpperJejunum"
  )

  amountGroupPaths <- list()
  simulationResultsOutputByGroup <- NULL

  # TO DO: Abdullah aggregation summary method might simplify the following process
  for (compartmentName in groupOfCompartments) {
    # Aggregation on compartment
    compartmentAggregatedPaths <- pathNames[grepl(pattern = compartmentName, x = pathNames, fixed = TRUE)]
    if (compartmentName %in% "Lumen") {
      compartmentAggregatedPaths <- pathNames[grepl(pattern = lumenCompartmentNames, x = pathNames, fixed = TRUE)]
    }
    for (compoundName in selectedCompoundNames) {
      # Aggregation on compound
      aggregatedPaths <- pathNames[grepl(pattern = compoundName, x = compartmentAggregatedPaths, fixed = TRUE)]

      aggregatedData <- rowSums(cbind(simulationResultsOutputTLF$data[, aggregatedPaths], 0))
      # Add to curve only if amount is >0
      if (max(aggregatedData) > 0) {
        # TO DO: Parametrize displayTimeUnit and unit conversion for time
        simulationResultsOutputByGroup <- rbind.data.frame(
          simulationResultsOutputByGroup,
          cbind.data.frame(
            "Time" = simulationResultsOutputTLF$data[, "Time"] / 60,
            "Amount" = aggregatedData,
            "NormalizedAmount" = aggregatedData / drugAmount,
            "Legend" = paste0(compoundName, " - ", compartmentName)
          )
        )
      }
    }
  }

  # TO DO: Get meta data from input or settings
  metaDataOutputByGroup <- list(
    "Time" = list(
      "unit" = "h",
      "dimension" = "Time"
    ),
    "Amount" = list(
      "unit" = "umol",
      "dimension" = "Amount"
    ),
    "NormalizedAmount" = list(
      "unit" = "fraction of drugmass",
      "dimension" = "Amount"
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
    timeProfile = simulationResultsOutputByGroup
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
    ) + coord_polar("y", start = 0) + xlab("") + ylab("")
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

  return(pieChartPlot)
}
