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
    histogram = function(parameterName, simulationSetName, descriptor) {
      paste0("Distribution of ", parameterName, " for ", reportSimulationSet(simulationSetName, descriptor))
    },
    rangePlot = function(xParameterName, yParameterName, simulationSetName, descriptor, referenceSetName = NULL, plotScale = "linear") {
      referenceSetText <- ifNotNull(referenceSetName, paste0(" in comparison to ", referenceSetName), "")
      return(paste0(
        xParameterName, "-dependence of ", yParameterName, " for ", reportSimulationSet(simulationSetName, descriptor),
        referenceSetText, ". Profiles are plotted in a ", plotScale, " scale."
      ))
    }
  ),
  plotGoF = list(
    timeProfile = function(simulationSetName, descriptor, dataSource, plotScale = "linear") {
      dataSourceText <- ifNotNull(dataSource, paste0(". Data source: ", dataSource), "")
      return(paste0(
        "Time profiles for ", reportSimulationSet(simulationSetName, descriptor),
        dataSourceText, ". Time profiles are plotted in a ", plotScale, " scale."
      ))
    },
    obsVsPred = function(simulationSetName, descriptor, dataSource, plotScale = "linear") {
      dataSourceText <- ifNotNull(dataSource, paste0(". Data source: ", dataSource), "")
      return(paste0(
        "Predicted vs observed for ", reportSimulationSet(simulationSetName, descriptor),
        dataSourceText, ". Predictions and observations are plotted in a ", plotScale, " scale."
      ))
    },
    resVsPred = function(simulationSetName, descriptor, dataSource, plotScale = ResidualScales$Linear) {
      dataSourceText <- ifNotNull(dataSource, paste0(". Data source: ", dataSource), "")
      return(paste0(plotScale, " residuals vs predicted values for ", reportSimulationSet(simulationSetName, descriptor), dataSourceText, "."))
    },
    resVsTime = function(simulationSetName, descriptor, dataSource, plotScale = ResidualScales$Linear) {
      dataSourceText <- ifNotNull(dataSource, paste0(". Data source: ", dataSource), "")
      return(paste0(plotScale, " residuals vs time values for ", reportSimulationSet(simulationSetName, descriptor), dataSourceText, "."))
    },
    resHisto = function(simulationSetName, descriptor, dataSource, plotScale = ResidualScales$Linear) {
      dataSourceText <- ifNotNull(dataSource, paste0(". Data source: ", dataSource), "")
      return(paste0(plotScale, " residuals distribution for ", reportSimulationSet(simulationSetName, descriptor), dataSourceText, "."))
    },
    resQQPlot = function(simulationSetName, descriptor, dataSource, plotScale = ResidualScales$Linear) {
      dataSourceText <- ifNotNull(dataSource, paste0(". Data source: ", dataSource), "")
      return(paste0(plotScale, " residuals for ", reportSimulationSet(simulationSetName, descriptor), dataSourceText, "."))
    },
    histogram = function(simulationSetName, descriptor) {
      paste0("Distribution of residuals for ", reportSimulationSet(simulationSetName, descriptor), ".")
    },
    qqPlot = function(simulationSetName, descriptor) {
      paste0("Residuals for ", reportSimulationSet(simulationSetName, descriptor), " as quantile-quantile plot.")
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

getDataSource <- function(structureSet) {
  # If no observed data, return null
  if (isOfLength(structureSet$simulationSet$observedDataFile, 0)) {
    return()
  }
  # Use strplit combined with normalizePath to get a vector of path elements
  # Then, cancel elements until observedDataFile diverge from workflowFolder
  observedDataPathElements <- unlist(strsplit(normalizePath(structureSet$simulationSet$observedDataFile, winslash = "/"), "/"))
  workflowPathElements <- unlist(strsplit(normalizePath(structureSet$workflowFolder, winslash = "/"), "/"))

  observedDataPathSize <- length(observedDataPathElements)
  workflowPathSize <- length(workflowPathElements)
  isCommonElement <- rep(FALSE, observedDataPathSize)

  for (pathElementIndex in 1:observedDataPathSize) {
    if (pathElementIndex > workflowPathSize) {
      next
    }
    isCommonElement[pathElementIndex] <- (observedDataPathElements[pathElementIndex] == workflowPathElements[pathElementIndex])
  }
  dataSource <- paste0(observedDataPathElements[!isCommonElement], collapse = "/")
  return(dataSource)
}

getGoodnessOfFitCaptions <- function(structureSet, plotType, plotScale = "linear") {
  dataSource <- getDataSource(structureSet)
  simulationSetName <- structureSet$simulationSet$simulationSetName
  setDescriptor <- structureSet$simulationSetDescriptor

  captionExpression <- parse(text = paste0("plotCaption <- captions$plotGoF$", plotType, "(simulationSetName, setDescriptor, dataSource, plotScale)"))
  eval(captionExpression)
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

getTimeRangeCaption <- function(timeRangeName) {
  if (isIncluded(timeRangeName, ApplicationRanges$total)) {
    return("### For total simulation time range")
  }
  if (isIncluded(timeRangeName, ApplicationRanges$firstApplication)) {
    return("### For first application range")
  }
  if (isIncluded(timeRangeName, ApplicationRanges$lastApplication)) {
    return("### For last application range")
  }
}
