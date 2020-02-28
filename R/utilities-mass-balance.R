#' @title getCompoundsList
#' @description Function to return a list of the compounds in a simulation.
#' @description Input is a simulation ("simulation") loaded from a PKML file using ospsuite::loadSimulation()
#' @description Output is a vector ("uniqueCompounds") of strings of unique compounds
#' @export
getCompoundsList <- function(simulation) {
  moleculePaths <- ospsuite::getAllMoleculesMatching(paths = "**", container = simulation)
  compoundsInPaths <- NULL
  for (env in moleculePaths) {
    str <- ospsuite::toPathArray(env$path)
    compoundsInPaths <- c(compoundsInPaths, tail(str, 1))
  }
  uniqueCompounds <- unique(compoundsInPaths)
  return(uniqueCompounds)
}

#' @title getPathsForMoleculeAmount
#' @description Input is a simulation ("simulation") loaded from a PKML file using ospsuite::loadSimulation() and the name of a compound ("compound") in that simulation
#' @description Output is a list ("pth") of environments pointing to amounts of the compound in the model containers
#' @export
getPathsForMoleculeAmount <- function(simulation, compound) {
  pth <- ospsuite::getAllMoleculesMatching(paths = paste0("**|", compound), container = simulation)
  return(pth)
}

#' @title pathStringsVector
#' @description Input is a list envList of environments that is output by the ospsuite::getAllXXXMatching() functions
#' @description Output is a list of the paths in each environment in the input list
#' @export
envList2PathStringsVector <- function(envList) {
  # envList2PathStringsVector = function(envList,removeSimulationName = FALSE){
  pathStringsVector <- NULL


  for (eL in envList) {
    pathStringsVector <- c(pathStringsVector, eL$path)
  }

  return(pathStringsVector)
}


#' @title plotMeanMassBalance
#' @description Plot mass balance diagnostics time profiles, cumulative time profiles,
#' normalized time profiles and cumulative normalized time profiles
#' @param structureSet `SimulationStructure` R6 class object
#' @param plotConfigurations List of `PlotConfiguration` R6 class objects for each goodness of fit plot
#' @return list of `ggplot` objects
#' @export
#' @import tlf
#' @import ospsuite
#' @import utils
plotMeanMassBalance <- function(structureSet,
                                plotConfigurations = NULL) {
  simulation <- ospsuite::loadSimulation(structureSet$simulationSet$simulationFile)
  allCompounds <- getCompoundsList(simulation)

  # TO DO: check if a list of compounds need to be excluded
  # compoundsOfInterest <- allCompounds[grep(structureSet$simulationSet$pathName, allCompounds)]

  # Paths of interest (ie with Amount) need to be explicitely added to the simulation
  # in order to be output
  ospsuite::clearOutputs(simulation)
  for (compound in allCompounds) {
    pathsOfInterest <- getPathsForMoleculeAmount(simulation = simulation, compound)
    if(!is.null(pathsOfInterest)){
      addOutputs(quantitiesOrPaths = pathsOfInterest, simulation = simulation)
      }
  }

  simulationResults <- ospsuite::runSimulation(simulation)
  simulationResultsOutputTLF <- getOutputValuesTLF(
    simulationResults = simulationResults,
    quantitiesOrPaths = simulationResults$allQuantityPaths
  )


  # TO DO: check how to obtain the list of compartments to group by
  # TO DO: Use an ospsuite grouping method ? e.g. path = **|compartment
  groupOfCompartments <- c("Lumen", "BloodCells", "Interstitial", "Intracellular", "Plasma", "Urine", "Feces")

  pathNames <- names(simulationResultsOutputTLF$data)
  amountGroupPaths <- list()
  simulationResultsOutputByGroup <- NULL

  # TO DO: check method to get drug total amount per molecule
  # drugAmount <- getAmount()
  drugAmount <- 10

  for (compartmentName in groupOfCompartments) {
    # amountGroupPaths is currently saved as list to know which paths are saved in which group
    # because it can be useful for debugging later on
    amountGroupPaths[[compartmentName]] <- pathNames[grepl(pattern = paste0(compartmentName), x = pathNames, fixed = TRUE)]

    # TO DO: Parametrize displayTimeUnit and unit conversion for time
    simulationResultsOutputByGroup <- rbind.data.frame(
      simulationResultsOutputByGroup,
      cbind.data.frame(
        "Time" = simulationResultsOutputTLF$data[, "Time"] / 60,
        "Amount" = rowSums(cbind(simulationResultsOutputTLF$data[, amountGroupPaths[[compartmentName]]], 0)),
        "NormalizedAmount" = rowSums(cbind(simulationResultsOutputTLF$data[, amountGroupPaths[[compartmentName]]], 0)) / drugAmount,
        "Legend" = paste0(structureSet$simulationSet$pathName, "-", compartmentName)
      )
    )
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
#' @param plotCOnfiguration `PlotConfiguration` R6 class object from `tlf` library
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

  # TO DO: remove Testing Data Watermark when plot is complete
  plotConfiguration <- plotConfiguration %||% tlf::PlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = timeVsAmountDataMapping,
    watermark = "Testing !"
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
#' @param plotCOnfiguration `PlotConfiguration` R6 class object from `tlf` library
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

  # TO DO: remove Testing Data Watermark when plot is complete
  plotConfiguration <- plotConfiguration %||% tlf::PlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = timeVsAmountDataMapping,
    watermark = "Testing !"
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
