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
  pkParametersData <- NULL
  simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)
  pkAnalyses <- ospsuite::importPKAnalysesFromCSV(
    structureSet$pkAnalysisResultsFileNames,
    simulation
  )

  pkParametersTable <- ospsuite::pkAnalysesAsDataFrame(pkAnalyses)
  for (output in structureSet$simulationSet$outputs) {
    molWeight <- simulation$molWeightFor(output$path)

    pkParametersData <- rbind.data.frame(
      pkParametersData,
      getMeanPkAnalysesFromOuptut(pkParametersTable, output, molWeight)
    )
  }

  return(list(
    plots = NULL,
    tables = list(pkAnalysis = pkParametersData)
  ))
}

getMeanPkAnalysesFromOuptut <- function(data, output, molWeight = NULL) {
  pkAnalysesFromOuptut <- NULL
  outputData <- data[data$QuantityPath %in% output$path, ]

  for (pkParameter in output$pkParameters) {
    displayName <- pkParameter$displayName
    displayUnit <- pkParameter$displayUnit

    selectedParameter <- outputData$Parameter %in% pkParameter$pkParameter

    pkParameterObject <- ospsuite::pkParameterByName(pkParameter$pkParameter)
    
    # Need to switch back to base unit first if a display unit is provided
    pkParameterValue <- outputData$Value[selectedParameter]
    
    if (!is.null(displayUnit)) {
      pkParameterValueInBaseUnit <- ospsuite::toBaseUnit(
        pkParameterObject$dimension,
        outputData$Value[selectedParameter],
        pkParameterObject$displayUnit,
        molWeight
      )
      pkParameterValue <- ospsuite::toUnit(
        pkParameterObject$dimension,
        pkParameterValueInBaseUnit,
        displayUnit,
        molWeight
      )
    }

    pkAnalysesFromOuptut <- rbind.data.frame(
      pkAnalysesFromOuptut,
      data.frame(
        Path = output$displayName,
        Parameter = displayName %||% outputData$Parameter[selectedParameter],
        Value = pkParameterValue,
        Unit = displayUnit %||% outputData$Unit[selectedParameter]
      )
    )
  }

  return(pkAnalysesFromOuptut)
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
                                       xParameters = getDefaultPkParametersXParameters(workflowType),
                                       yParameters = NULL) {
  validateIsIncluded(workflowType, PopulationWorkflowTypes)
  validateIsOfType(structureSets, "list")
  validateIsOfType(c(structureSets), "SimulationStructure")
  validateIsString(c(xParameters), nullAllowed = TRUE)
  validateIsOfType(c(yParameters), "Output", nullAllowed = TRUE)

  # TO DO: need a validate method to ensure that all outputs are identical
  # validateIsIdentical(sapply(structureSets, function(set){set$simulationSet$outputs})) ?
  yParameters <- yParameters %||% structureSets[[1]]$simulationSet$outputs

  # Get first simulation, in case mol weight is needed
  simulation <- loadSimulationWithUpdatedPaths(structureSets[[1]]$simulationSet)

  pkRatioTableAcrossPopulations <- NULL
  pkParametersPlots <- NULL
  pkParametersTables <- NULL

  pkParametersMapping <- tlf::BoxWhiskerDataMapping$new(
    x = "simulationSetName",
    y = "Value"
  )

  pkParametersAcrossPopulations <- getPkParametersAcrossPopulations(structureSets)
  pkParametersDataAcrossPopulations <- pkParametersAcrossPopulations$data
  pkParametersMetaDataAcrossPopulations <- pkParametersAcrossPopulations$metaData

  # Enforce factors for population names
  pkParametersDataAcrossPopulations$simulationSetName <- factor(pkParametersDataAcrossPopulations$simulationSetName)

  if (workflowType %in% c(PopulationWorkflowTypes$ratioComparison, PopulationWorkflowTypes$pediatric)) {
    referencePopulationName <- getReferencePopulationName(structureSets)
  }

  # Standard boxplots for each pkParameters of each output
  for (output in yParameters) {
    molWeight <- simulation$molWeightFor(output$path)
    pathLabel <- lastPathElement(output$path)
    for (pkParameter in output$pkParameters) {
      yParameterLabel <- lastPathElement(pkParameter$pkParameter)

      pkParameterFromOutput <- getPopulationPkAnalysesFromOuptut(
        pkParametersDataAcrossPopulations,
        pkParametersMetaDataAcrossPopulations,
        output,
        pkParameter,
        molWeight
      )

      pkParameterData <- pkParameterFromOutput$data
      pkParameterMetaData <- pkParameterFromOutput$metaData

      # remove NA value to prevent crash in computation of percentiles
      pkParameterData <- pkParameterData[!is.na(pkParameterData$Value), ]

      boxplotPkParameter <- tlf::plotBoxWhisker(
        data = pkParameterData,
        metaData = pkParameterMetaData,
        dataMapping = pkParametersMapping,
        plotConfiguration = settings$plotConfigurations[["boxplotPkParameters"]]
      ) + ggplot2::labs(title = NULL, subtitle = NULL) + ggplot2::xlab(NULL)



      pkParametersPlots[[paste0(pathLabel, "-", yParameterLabel)]] <- boxplotPkParameter
      pkParametersPlots[[paste0(pathLabel, "-", yParameterLabel, "-log")]] <- boxplotPkParameter +
        ggplot2::scale_y_continuous(trans = "log10")

      pkParameterTable <- tlf::getBoxWhiskerMeasure(
        data = pkParameterData,
        dataMapping = pkParametersMapping
      )

      pkParameterTableRows <- row.names(pkParameterTable)
      pkParameterTable <- cbind(
        Population = pkParameterTableRows,
        pkParameterTable
      )

      pkParametersTables[[paste0(pathLabel, "-", yParameterLabel)]] <- pkParameterTable

      # Range plots on PK parameters vs xParameters
      for (demographyParameter in xParameters) {
        xParameterLabel <- lastPathElement(demographyParameter)
        vpcMetaData <- list(
          "x" = pkParameterMetaData[[demographyParameter]],
          "median" = pkParameterMetaData$Value
        )

        aggregatedData <- getDemographyAggregatedData(
          data = pkParameterData,
          xParameterName = demographyParameter,
          yParameterName = "Value",
          xParameterBreaks = settings$xParametersBreaks[[demographyParameter]]
        )

        populationNames <- levels(factor(aggregatedData$Population))

        # For pediatric workflow, range plots compare reference population to the other populations
        if (workflowType %in% c(PopulationWorkflowTypes$pediatric)) {
          # Get the table for reference population
          referenceData <- data.frame(
            x = c(-Inf, Inf),
            "Population" = paste("Simulated", AggregationConfiguration$names$middle, AggregationConfiguration$names$range, "for", referencePopulationName)
          )
          referenceData[, c("ymin", "median", "ymax")] <- pkParameterTable[referencePopulationName, c(3, 5, 7)]

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

            pkParametersPlots[[paste0(populationName, "-vs-ref-", yParameterLabel, "-vs-", xParameterLabel)]] <- comparisonVpcPlot
            pkParametersPlots[[paste0(populationName, "-vs-ref-", yParameterLabel, "-vs-", xParameterLabel, "-log")]] <- comparisonVpcPlot +
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
          pkParametersPlots[[paste0(populationName, "-", yParameterLabel, "-vs-", xParameterLabel)]] <- vpcPlot
          pkParametersPlots[[paste0(populationName, "-", yParameterLabel, "-vs-", xParameterLabel, "-log")]] <- vpcPlot +
            ggplot2::scale_y_continuous(trans = "log10")
        }
      }

      # For Ratio Comparison create boxplots of boxplot hinges ratios
      if (workflowType %in% PopulationWorkflowTypes$ratioComparison) {
        # Get the tables and compute the ratios using reference population name
        pkRatiosTable <- getPkRatiosTable(pkParameterTable, referencePopulationName)

        pkRatiosData <- pkRatiosTable
        pkRatiosData[, c("ymin", "lower", "middle", "upper", "ymax")] <- pkRatiosTable[, c(3:7)]

        boxplotPkRatios <- ratioBoxplot(
          data = pkRatiosData,
          plotConfiguration = settings$plotConfigurations[["boxplotPkRatios"]]
        ) + ggplot2::ylab(paste0(pkParameterMetaData$Value$dimension, " [fraction of ", referencePopulationName, "]"))

        pkParametersPlots[[paste0(pathLabel, "-", yParameterLabel, "-ratio")]] <- boxplotPkRatios
        pkParametersPlots[[paste0(pathLabel, "-", yParameterLabel, "-ratio-log")]] <- boxplotPkRatios +
          ggplot2::scale_y_continuous(trans = "log10")

        pkParametersTables[[paste0(pathLabel, "-", yParameterLabel, "-ratio")]] <- pkRatiosTable
      }
    }
  }

  return(list(plots = pkParametersPlots, tables = pkParametersTables))
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

getPkParametersAcrossPopulations <- function(structureSets) {
  pkParametersTableAcrossPopulations <- NULL
  for (structureSet in structureSets)
  {
    simulation <- loadSimulationWithUpdatedPaths(structureSet$simulationSet)
    population <- loadWorkflowPopulation(structureSet$simulationSet)

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

  pkParametersTableAcrossPopulations$Gender <- as.numeric(pkParametersTableAcrossPopulations$Gender)
  metaData[["Gender"]] <- list(
    dimension = "Gender",
    unit = ""
  )

  return(list(
    data = pkParametersTableAcrossPopulations,
    metaData = metaData
  ))
}

getPkRatiosTable <- function(pkParametersTable,
                             referencePopulationName) {
  populationNames <- pkParametersTable$Population

  pkRatiosTable <- pkParametersTable[populationNames != referencePopulationName, ]
  referencePkParametersTable <- pkParametersTable[rep(referencePopulationName, length(pkRatiosTable$Population)), ]

  pkRatiosTable[, seq(3, ncol(pkRatiosTable))] <- pkRatiosTable[, seq(3, ncol(pkRatiosTable))] / referencePkParametersTable[, seq(3, ncol(pkRatiosTable))]

  return(pkRatiosTable)
}

#' @title getDefaultPkParametersXParameters
#' @description Get names of default parameters in x axis of pk parameters plots.
#' @param workflowType Name of workflow type.
#' Use enum `PopulationWorkflowTypes` to get a list of available workflow types.
#' @return names of default parameters
getDefaultPkParametersXParameters <- function(workflowType) {
  validateIsIncluded(workflowType, PopulationWorkflowTypes)
  if (workflowType %in% PopulationWorkflowTypes$pediatric) {
    return(DemographyDefaultParameters)
  }
  return(NULL)
}

getPopulationPkAnalysesFromOuptut <- function(data, metaData, output, pkParameter, molWeight = NULL) {
  outputData <- data[data$QuantityPath %in% output$path, ]

  displayName <- pkParameter$displayName
  displayUnit <- pkParameter$displayUnit

  selectedParameter <- outputData$Parameter %in% pkParameter$pkParameter
  pkParameterObject <- ospsuite::pkParameterByName(pkParameter$pkParameter)

  # Need to switch back to base unit first if a display unit is provided
  pkParameterValue <- outputData$Value[selectedParameter]

  if (!is.null(displayUnit)) {
    pkParameterValueInBaseUnit <- ospsuite::toBaseUnit(
      pkParameterObject$dimension,
      outputData$Value[selectedParameter],
      pkParameterObject$displayUnit,
      molWeight
    )
    pkParameterValue <- ospsuite::toUnit(
      pkParameterObject$dimension,
      pkParameterValueInBaseUnit,
      displayUnit,
      molWeight
    )
  }

  pkAnalysesFromOuptut <- outputData[selectedParameter, ]
  pkAnalysesFromOuptut$Value <- pkParameterValue

  pkAnalysesFromOuptutMetaData <- metaData
  parameterLabel <- paste0(displayName %||% pkAnalysesFromOuptut$Parameter[1], " for ", output$displayName)
  pkAnalysesFromOuptutMetaData$Value <- list(
    dimension = parameterLabel,
    unit = displayUnit %||% pkAnalysesFromOuptut$Unit[1]
  )

  return(list(
    data = pkAnalysesFromOuptut,
    metaData = pkAnalysesFromOuptutMetaData
  ))
}

#' @title getXParametersForPkParametersPlot
#' @param workflow `PopulationWorkflow` R6 class object
#' @return list of x-parameters used for PK parameters range plots
#' @export
getXParametersForPkParametersPlot <- function(workflow) {
  validateIsOfType(workflow, "PopulationWorkflow")
  return(workflow$plotPKParameters$xParameters)
}

#' @title getYParametersForPkParametersPlot
#' @param workflow `PopulationWorkflow` R6 class object
#' @return list of y-parameters used for PK parameters range plots and boxplots
#' @export
getYParametersForPkParametersPlot <- function(workflow) {
  validateIsOfType(workflow, "PopulationWorkflow")
  yParameters <- workflow$plotPKParameters$yParameters %||% workflow$simulationStructures[[1]]$simulationSet$outputs

  return(yParameters)
}

#' @title setXParametersForPkParametersPlot
#' @description Set x-parameters for range plots of PK parameters plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of parameters to be used as x-parameters
#' @export
setXParametersForPkParametersPlot <- function(workflow, parameters) {
  validateIsOfType(workflow, "PopulationWorkflow")
  validateIsString(c(parameters))

  workflow$plotPKParameters$xParameters <- parameters

  logWorkflow(
    message = paste0(
      "X-parameters: '",
      paste0(c(parameters), collapse = "', '"),
      "' set for PK parameters plot."
    ),
    pathFolder = workflow$workflowFolder,
    logTypes = LogTypes$Debug
  )
  return(invisible())
}

#' @title setYParametersForPkParametersPlot
#' @description Set y-parameters for boxplots and range plots of PK parameters plot task.
#' The method update directly the input workflow
#' @param workflow `PopulationWorkflow` R6 class object
#' @param parameters list of R6 class `Output` objects to be used as y-parameters
#' @export
setYParametersForPkParametersPlot <- function(workflow, parameters) {
  validateIsOfType(workflow, "PopulationWorkflow")
  validateIsOfType(c(parameters), "Output")

  workflow$plotPKParameters$yParameters <- parameters

  for (output in c(parameters)) {
    logWorkflow(
      message = paste0(
        "Y-parameters: '",
        paste0(c(output$pkParameters), collapse = "', '"),
        "' for '", output$path, "' set for PK parameters plot."
      ),
      pathFolder = workflow$workflowFolder,
      logTypes = LogTypes$Debug
    )
  }
  return(invisible())
}
