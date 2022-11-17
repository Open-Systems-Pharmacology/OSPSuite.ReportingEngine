captions <- list(
  absorption = function(compound) {
    paste0("Absorption of ", compound)
  },
  massBalance = list(
    timeProfile = function() {
      "Amount of drug vs time within the different compartments"
    },
    cumulativeTimeProfile = function() {
      "Cumulated amount of drug vs time within the different compartments"
    },
    normalizedTimeProfile = function() {
      "Amount of drug vs time within the different compartments normalized to applicated drugmass"
    },
    normalizedCumulativeTimeProfile = function() {
      "Cumulated amount of drug vs time within the different compartments normalized to applicated drugmass"
    },
    pieChart = function(time, timeUnit) {
      paste0("Fraction of drug within the different compartments at ", time, timeUnit)
    }
  ),
  demography = list(
    parameterSection = function(sectionId, parameterName) {
      c(anchor(sectionId), "", paste("##", parameterName, "distributions"))
    },
    xParameterSection = function(sectionId, parameterName) {
      c(anchor(sectionId), "", paste("## ", parameterName, "-dependence", sep = ""))
    },
    yParameterSection = function(sectionId, parameterName) {
      c(anchor(sectionId), "", paste("### Dependence of", parameterName))
    },
    populationSection = function(sectionId, simulationSetName, descriptor, level = 4) {
      tagLevel <- paste0(rep("#", level), collapse = "")
      sectionTitle <- paste(tagLevel, "For", reportSimulationSet(simulationSetName, descriptor))
      return(c(anchor(sectionId), "", sectionTitle))
    },
    histogram = function(parameterName, simulationSetName, descriptor) {
      paste0("Distribution of ", parameterName, " for ", reportSimulationSet(simulationSetName, descriptor))
    },
    rangePlot = function(xParameterName, yParameterName, simulationSetName, descriptor, referenceSetName = NULL, plotScale = "linear") {
      referenceSetText <- ifNotNull(referenceSetName, paste0(" in comparison to ", referenceSetName), "")
      return(paste0(
        xParameterName, "-dependence of ", yParameterName, " for ", reportSimulationSet(simulationSetName, descriptor),
        referenceSetText, getPlotScaleCaption("Profiles", plotScale)
      ))
    }
  ),
  plotGoF = list(
    timeProfile = function(simulationSetName, descriptor, dataSource = "", plotScale = "linear") {
      return(paste0(
        "Time profiles for ", reportSimulationSet(simulationSetName, descriptor), 
        dataSource, getPlotScaleCaption("Time profiles", plotScale)
      ))
    },
    obsVsPred = function(simulationSetName, descriptor, dataSource = "", plotScale = "linear", yCaption = NULL, pathName = NULL) {
      return(paste0(
        "Predicted ", getStatisticsCaption(yCaption), "vs observed for ",
        getResidualsAcrossCaption(pathName), reportSimulationSet(simulationSetName, descriptor), 
        dataSource, getPlotScaleCaption("Predictions and observations", plotScale)
      ))
    },
    resVsPred = function(simulationSetName, descriptor, dataSource = "", plotScale = ResidualScales$Linear, yCaption = NULL, pathName = NULL) {
      return(paste0(
        plotScale, " residuals vs predicted ", getStatisticsCaption(yCaption), "values for ", 
        getResidualsAcrossCaption(pathName), reportSimulationSet(simulationSetName, descriptor), 
        dataSource
        ))
    },
    resVsTime = function(simulationSetName, descriptor, dataSource = "", plotScale = ResidualScales$Linear, pathName = NULL) {
      return(paste0(
        plotScale, " residuals vs time values for ", 
        getResidualsAcrossCaption(pathName), reportSimulationSet(simulationSetName, descriptor), 
        dataSource
        ))
    },
    resHisto = function(simulationSetName, descriptor, dataSource = "", plotScale = ResidualScales$Linear, pathName = NULL) {
      return(paste0(
        plotScale, " residuals distribution (stacked) for ", 
        getResidualsAcrossCaption(pathName), reportSimulationSet(simulationSetName, descriptor), 
        dataSource
        ))
    },
    resQQPlot = function(simulationSetName, descriptor, dataSource = "", plotScale = ResidualScales$Linear, pathName = NULL) {
      return(paste0(
        plotScale, " residuals for ", 
        getResidualsAcrossCaption(pathName), reportSimulationSet(simulationSetName, descriptor), 
        " as quantile-quantile plot", dataSource
        ))
    },
    meanLegend = function(simulationSetName, descriptor, pathName) {
      return(paste0("Simulated ", pathName, " (", reportSimulationSet(simulationSetName, descriptor), ")"))
    },
    populationLegend = function(simulationSetName, descriptor, statistics, pathName) {
      paste0(
        "Simulated ", statistics$yCaption, " and ", statistics$rangeCaption,
        " for ", pathName, " (", reportSimulationSet(simulationSetName, descriptor), ")"
      )
    },
    resLegend = function(simulationSetName, descriptor, pathName) {
      return(paste0(pathName, " (", reportSimulationSet(simulationSetName, descriptor), ")"))
    },
    observedLegend = function(simulationSetName, descriptor, pathName) {
      paste0(
        "Observed data for ", pathName, " (", reportSimulationSet(simulationSetName, descriptor), ")"
      )
    },
    lloqLegend = function(simulationSetName, descriptor, pathName) {
      paste0(
        "BLQ data for ", pathName, " (", reportSimulationSet(simulationSetName, descriptor), ")"
      )
    }
  ),
  plotPKParameters = list(
    mean = function(simulationSetName, descriptor) {
      paste0("PK parameters for ", reportSimulationSet(simulationSetName, descriptor))
    },
    boxplot = function(parameterName, pathName, simulationSetName, descriptor, plotScale = "linear") {
      return(paste0(
        parameterName, " of ", pathName, " for ", reportSimulationSet(simulationSetName, descriptor),
        " shown as box-whisker plot, which indicates the 5th, 25th, 50th, 75th, and 95th percentiles in ", plotScale, " scale."
      ))
    },
    summaryTable = function(parameterName, pathName, simulationSetName, descriptor, displayUnit) {
      return(paste0(parameterName, " of ", pathName, " for ", reportSimulationSet(simulationSetName, descriptor), reportUnit(displayUnit)))
    },
    ratioPlot = function(parameterName, pathName, simulationSetName, descriptor, referenceSetName, plotScale = "linear") {
      referenceSetText <- paste0(" in comparison to ", referenceSetName)
      return(paste0(
        parameterName, " of ", pathName, " for ", reportSimulationSet(simulationSetName, descriptor), referenceSetText,
        " shown as box-whisker plot, which indicates ratios of the 5th, 25th, 50th, 75th, and 95th percentiles in ", plotScale, " scale."
      ))
    },
    ratioTable = function(parameterName, pathName, simulationSetName, descriptor, referenceSetName) {
      referenceSetText <- paste0(" in comparison to ", referenceSetName)
      return(paste0(
        "Ratios of ", parameterName, " of ", pathName, " for ", reportSimulationSet(simulationSetName, descriptor), referenceSetText
      ))
    },
    rangePlot = function(xParameterName, yParameterName, simulationSetName, descriptor, referenceSetName = NULL, plotScale = "linear") {
      referenceSetText <- ifNotNull(referenceSetName, paste0(" in comparison to ", referenceSetName), "")
      return(paste0(
        xParameterName, "-dependence of ", yParameterName, " for ", reportSimulationSet(simulationSetName, descriptor),
        referenceSetText, ". Profiles are plotted in a ", plotScale, " scale."
      ))
    }
  ),
  plotSensitivity = list(
    mean = function(parameterName, pathName) {
      paste0("Most sensitive parameters for ", parameterName, " of ", pathName, ".")
    },
    population = function(parameterName, pathName, quantiles, simulationSetName, descriptor) {
      quantileText <- paste0(quantiles, collapse = ", ")
      return(paste0(
        "Most sensitive parameters for ", parameterName, " of ", pathName,
        " for individuals at percentiles ", quantileText, " for ",
        reportSimulationSet(simulationSetName, descriptor), "."
      ))
    }
  )
)

getDataSourceCaption <- function(structureSet) {
  # If no observed data, end the sentence of caption
  if (isEmpty(structureSet$simulationSet$dataSource)) {
    return(". ")
  }
  dataSourceCaption <- structureSet$simulationSet$dataSource$getCaption(structureSet$workflowFolder)
  return(paste(".", dataSourceCaption, "."))
}

getGoodnessOfFitCaptions <- function(structureSet, plotType, plotScale = "linear", settings = NULL) {
  dataSource <- getDataSourceCaption(structureSet)
  simulationSetName <- structureSet$simulationSet$simulationSetName
  setDescriptor <- structureSet$simulationSetDescriptor
  yCaption <- NULL
  if (isOfType(structureSet$simulationSet, "PopulationSimulationSet") &
    isIncluded(plotType, c("obsVsPred", "resVsPred"))) {
    gofStatistics <- settings$getStatistics()
    yCaption <- gofStatistics$yCaption
    plotCaption <- captions$plotGoF[[plotType]](simulationSetName, setDescriptor, dataSource, plotScale, yCaption)
    return(plotCaption)
  }
  plotCaption <- captions$plotGoF[[plotType]](simulationSetName, setDescriptor, dataSource, plotScale)
  return(plotCaption)
}

# Description needs to adapt to number of sets and descriptor
reportSimulationSet <- function(simulationSetNames, descriptor) {
  # For multiple sets, concatenate with ", " and add "s" to descriptor
  if (length(simulationSetNames) > 1) {
    simulationSetNames <- paste0(simulationSetNames, collapse = ", ")
    if (descriptor != "") {
      descriptor <- paste0(descriptor, "s")
    }
  }
  if (descriptor == "") {
    return(simulationSetNames)
  }
  return(paste0(descriptor, " ", simulationSetNames))
}

reportUnit <- function(displayUnit) {
  if (isOfLength(displayUnit, 0)) {
    return("")
  }
  if (displayUnit == "") {
    return("")
  }
  return(paste0(" reported in [", displayUnit, "]"))
}

# For the reported tables, first column is simulation set names
# Consequently, the descriptor should be used
# However, expression "" is not accepted and needs to be replaced by "Population"
translateDescriptor <- function(descriptor) {
  if (descriptor == "") {
    descriptor <- "Population"
  }
  return(descriptor)
}

addDescriptorToTable <- function(data, descriptor) {
  names(data)[1] <- translateDescriptor(descriptor)
  return(data)
}

getTimeRangeCaption <- function(timeRangeName, reference, simulationSetName) {
  if (isIncluded(timeRangeName, ApplicationRanges$total)) {
    return(c(
      anchor(paste0(reference, "-", removeForbiddenLetters(simulationSetName), "-", "total")), "",
      "### For total simulation time range"
    ))
  }
  if (isIncluded(timeRangeName, ApplicationRanges$firstApplication)) {
    return(c(
      anchor(paste0(reference, "-", removeForbiddenLetters(simulationSetName), "-", "first")), "",
      "### For first application range"
    ))
  }
  if (isIncluded(timeRangeName, ApplicationRanges$lastApplication)) {
    return(c(
      anchor(paste0(reference, "-", removeForbiddenLetters(simulationSetName), "-", "last")), "",
      "### For last application range"
    ))
  }
}

getStatisticsCaption <- function(statistics){
  if(isEmpty(statistics)){
    return("")
  }
  return(paste0("(", statistics, ") "))
}

getPlotScaleCaption <- function(plotName, plotScale) {
  return(paste0(". ", plotName, " are plotted in a ", plotScale, " scale."))
}

getResidualsAcrossCaption <- function(pathName) {
  if (isEmpty(pathName)) {
    return("")
  }
  return(paste0(pathName, " across "))
}
