#' @title calculateResiduals
#' @param simulatedData, vector of simulated data
#' @param observedData, vector of observed data
#' @param residualScale, must be selected from enum ResidualScales
#' @return residuals between simulatedData and observedData
#' @description
#' Calculate residuals between vectors `simulatedData` and `observedData` according the the residual scale specified in `residualScale`
#' @export
calculateResiduals <- function(simulatedData, observedData, residualScale) {
  validateIsOfLength(object = simulatedData, nbElements = length(observedData))
  residualValues <- rep(NA, length(observedData))
  if (isIncluded(residualScale, ResidualScales$Logarithmic)) {
    residualValues <- log(observedData) - log(simulatedData)
  }
  if (isIncluded(residualScale, ResidualScales$Linear)) {
    residualValues <- (observedData - simulatedData)
  }
}

# TODO: add these functions to tlf

#' @title geomean
#' @description
#' Calculate the geometric mean
#' @param x values
#' @param na.rm logical defining removal of `NA` values
#' @return Geometric mean
#' @export
geomean <- function(x, na.rm = TRUE) {
  logX <- log(x[x>0])
  exp(mean(logX, na.rm = na.rm))
}

#' @title geomean*sd
#' @description
#' Calculate the geometric mean * geometric SD
#' @param x values
#' @param na.rm logical defining removal of `NA` values
#' @return Geometric mean * geometric SD
#' @export
`geomean*sd` <- function(x, na.rm = TRUE) {
  logX <- log(x[x>0])
  exp(mean(logX, na.rm = na.rm)+stats::sd(logX, na.rm = na.rm))
}

#' @title geomean/sd
#' @description
#' Calculate the geometric mean / geometric SD
#' @param x values
#' @param na.rm logical defining removal of `NA` values
#' @return Geometric mean / geometric SD
#' @export
`geomean/sd` <- function(x, na.rm = TRUE) {
  logX <- log(x[x>0])
  exp(mean(logX, na.rm = na.rm)-stats::sd(logX, na.rm = na.rm))
}

#' @title calculateGeometricErrorRange
#' @param values Numeric values of the geometric mean
#' @param errorValues Numeric values of the geometric error
#' @return A named list, with `ymin` and `ymax`, of the range calculated from the geometric mean and errors.
#' @description
#' Calculate the range from the geometric mean and error.
#' @export
calculateGeometricErrorRange <- function(values, errorValues) {
  return(list(
    ymin = values / errorValues,
    ymax = values * errorValues
  ))
}

#' @title calculateArithmeticErrorRange
#' @param values Numeric values of the arithmetic mean
#' @param errorValues Numeric values of the arithmetic error
#' @return A named list, with `ymin` and `ymax`, of the range calculated from the arithmetic mean and errors.
#' @description
#' Calculate the range from the arithmetic mean and error.
#' @export
calculateArithmeticErrorRange <- function(values, errorValues) {
  return(list(
    ymin = values - errorValues,
    ymax = values + errorValues
  ))
}

#' @title trimFileName
#' @param path character string containing the name of the path or file to trim
#' @param extension character string containing the extension file
#' @param sep character string separating path elements. "/" is default value.
#' @return fileName character string of the trimmed filed name
#' @description
#' Trim path and extension of a file
#' @examples
#' \dontrun{
#' pathName <- "folder/subfolder/testFile.txt"
#' trimFileName(pathName, extension = "txt")
#' }
#' @export
trimFileName <- function(path, extension = NULL, sep = "/") {
  fileName <- sub(
    pattern = paste0("^.*[", sep, "]"),
    replacement = "",
    x = path
  )
  if (!is.null(extension)) {
    fileName <- sub(
      pattern = paste0("[.].*", extension),
      "[.].*$",
      replacement = "",
      x = fileName
    )
  }
  return(fileName)
}

#' @title removeForbiddenLetters
#' @param text character string to be evaluated
#' @param forbiddenLetters characters to be removed if in the \code{text}.
#' Default value of \code{forbiddenLetters} is \code{"[[:punct:]]"}
#' meaning that all pointuation characters are forbidden.
#' @param replacement character replacing the \code{forbiddenLetters}.
#' Default value of \code{forbiddenLetters} is "_".
#' @return \code{text} character string with forbidden letters replaced
#' @description
#' Trim path and extension of a file
#' @examples
#' \dontrun{
#' removeForbiddenLetters(text)
#' }
#' @export
removeForbiddenLetters <- function(text, forbiddenLetters = "[[:punct:][:blank:]]", replacement = "_") {
  gsub(
    pattern = forbiddenLetters,
    replacement = replacement,
    x = text
  )
}

#' @title generateResultFileNames
#' @return A list of filenames to be output by each core
#' @param numberOfCores to be used in parallel computation
#' @param folderName where result files will be saved
#' @param fileName prefix of result file names
#' @param separator used between file name prefix and index
#' @param extension for result file type.  default is CSV
#' @description
#' #Generate a listcontaining names of CSV result files that will be output by each core in parallel computation
#' @export
generateResultFileNames <- function(numberOfCores, folderName, fileName, separator = "-", extension = ".csv") {
  allResultsFileNames <- sapply(
    X = 1:numberOfCores, function(x, folderName, fileName) {
      return(file.path(folderName, paste0(fileName, separator, x, extension)))
    },
    folderName = folderName,
    fileName = fileName,
    USE.NAMES = FALSE
  )
  return(allResultsFileNames)
}

#' @title loadSimulationWithUpdatedPaths
#' @param simulationSet simulation set containing path to simulation file and pathIDs for quantities to be loaded into simulation object
#' @param loadFromCache If `TRUE`, an already loaded pkml file will not be loaded again, but the `Simulation` object will be retrieved from cache.
#' If `FALSE`, a new `Simulation` object will be created. Default value is `FALSE`.
#' @param addToCache If `TRUE`, the loaded simulation is added to cache.
#' If `FALSE`, the returned simulation only exists locally. Default is `TRUE`.
#' @return A `Simulation` object with pathIDs updated from simulationSet
#' @export
loadSimulationWithUpdatedPaths <- function(simulationSet, loadFromCache = FALSE, addToCache = TRUE) {
  simulation <- ospsuite::loadSimulation(
    filePath = simulationSet$simulationFile,
    loadFromCache = loadFromCache,
    addToCache = addToCache
  )
  # Prevent loadSimulationWithUpdatedPaths from crashing if user did not submit any pathID
  if (!is.null(simulationSet$outputs)) {
    simulation$outputSelections$clear()
    paths <- sapply(simulationSet$outputs, function(output) {
      output$path
    })
    ospsuite::addOutputs(quantitiesOrPaths = paths, simulation = simulation)
  }

  if (is.null(simulationSet$minimumSimulationEndTime)) {
    return(simulation)
  }

  if (simulationSet$minimumSimulationEndTime > simulation$outputSchema$endTime) {
    maximalIntervalIndex <- which(sapply(simulation$outputSchema$intervals, function(x) {
      x$endTime$value
    }) == simulation$outputSchema$endTime)[1]
    simulation$outputSchema$intervals[[maximalIntervalIndex]]$endTime$setValue(value = simulationSet$minimumSimulationEndTime, unit = ospUnits$Time$min)
  }
  return(simulation)
}

#' @title loadWorkflowPopulation
#' @param simulationSet A `PopulationSimulationSet` object
#' @export
#' @import ospsuite
loadWorkflowPopulation <- function(simulationSet) {
  validateIsOfType(simulationSet, "PopulationSimulationSet")
  population <- ospsuite::loadPopulation(simulationSet$populationFile)
  simulation <- loadSimulationWithUpdatedPaths(simulationSet)

  if (!is.null(simulationSet$studyDesignFile)) {
    addStudyParameters(population, simulation, simulationSet$studyDesignFile)
  }
  return(population)
}

#' @title lastPathElement
#' @param path simulation path
#' @return last path element as character string
#' @export
#' @import ospsuite
#' @import utils
lastPathElement <- function(path) {
  pathArray <- ospsuite::toPathArray(path)
  lastElement <- utils::tail(pathArray, 1)

  return(lastElement)
}

#' @title replaceInfWithNA
#' @param data numeric vector
#' @return numeric vector
#' @keywords internal
replaceInfWithNA <- function(data) {
  infData <- is.infinite(data)
  Ninf <- sum(infData)
  if (Ninf > 0) {
    logDebug(paste0(Ninf, " values were infinite and transformed into missing values (NA)"))
  }
  data[infData] <- NA
  return(data)
}

#' @title removeMissingValues
#' @param data data.frame
#' @param dataMapping name of variable on which the missing values ar checked
#' @return filtered data.frame
#' @keywords internal
removeMissingValues <- function(data, dataMapping = NULL) {
  if(isEmpty(data)){
    return(data)
  }
  data[, dataMapping] <- replaceInfWithNA(data[, dataMapping])
  naData <- is.na(data[, dataMapping])
  Nna <- sum(naData)
  data <- data[!naData, ]

  if (Nna > 0) {
    logDebug(paste0(Nna, " values were missing (NA) from variable '", dataMapping, "' and removed from the analysis"))
  }
  return(data)
}

#' @title removeNegativeValues
#' @param data data.frame
#' @param dataMapping name of variable on which the missing values ar checked
#' @return filtered data.frame
#' @keywords internal
removeNegativeValues <- function(data, dataMapping = NULL) {
  if(isEmpty(data)){
    return(data)
  }
  negativeData <- data[, dataMapping] <= 0
  Nnegative <- sum(negativeData, na.rm = TRUE)
  data <- data[!negativeData, ]
  
  if (Nnegative > 0) {
    logDebug(paste0(Nnegative, " values from variable '", dataMapping, "' were negative and removed from the analysis"))
  }
  return(data)
}

#' @title getPKParametersInOutput
#' @param output Output object
#' @return Names of pkParameters in `output`
#' @export
#' @family workflow helpers
getPKParametersInOutput <- function(output) {
  validateIsOfType(output, "Output")
  pkParameters <- sapply(output$pkParameters, function(pkParameterInfo) {
    pkParameterInfo$pkParameter
  })
  if (isEmpty(pkParameters)) {
    return(NA)
  }
  return(pkParameters)
}

#' @title getPKParametersInOutput
#' @param output Output object
#' @return Names of pkParameters in `output`
#' @keywords internal
getPKParameterGroupsInOutput <- function(output) {
  validateIsOfType(output, "Output")
  pkParameters <- sapply(output$pkParameters, function(pkParameterInfo) {
    pkParameterInfo$group
  })
  if (isEmpty(pkParameters)) {
    return(NA)
  }
  return(pkParameters)
}

#' @title getOutputPathsInSimulationSet
#' @param simulationSet SimulationSet object or derived class
#' @return Path names of outputs in `simulationSet`
#' @export
#' @family workflow helpers
getOutputPathsInSimulationSet <- function(simulationSet) {
  validateIsOfType(simulationSet, "SimulationSet")
  return(sapply(simulationSet$outputs, function(output) {
    output$path
  }))
}

#' @title getPKParametersInSimulationSet
#' @param simulationSet SimulationSet object or derived class
#' @return Data.frame with \code{path} and \code{pkParameter} in `simulationSet`
#' @export
#' @family workflow helpers
getPKParametersInSimulationSet <- function(simulationSet) {
  validateIsOfType(simulationSet, "SimulationSet")
  pkParametersTable <- data.frame()
  for (output in simulationSet$outputs) {
    pkParametersTable <- rbind.data.frame(
      pkParametersTable,
      data.frame(
        path = output$path,
        pkParameter = getPKParametersInOutput(output),
        group = getPKParameterGroupsInOutput(output),
        stringsAsFactors = FALSE
      )
    )
  }
  return(pkParametersTable)
}

#' @title getAllowedCores
#'
#' @description Get allowed number of CPU cores for computation
#'
#' @return Allowed number of CPU cores for computation
#' @keywords internal
getAllowedCores <- function() {
  return(
    getAllowedCoresLinuxKubernetes() %||% 
      getOSPSuiteSetting(settingName = "numberOfCores")
  )
}


#' @title getAllowedCoresLinuxKubernetes
#'
#' @description
#' Relevant only when reporting engine is executed on a Linux Kubernetes cluster.
#'
#' @return Allowed number of CPU cores for computation
#' @keywords internal
getAllowedCoresLinuxKubernetes <- function() {
  cores <- tryCatch(
    {
      # get cpu allowance from files
      cfs_quota_us <- as.numeric(system("cat /sys/fs/cgroup/cpu/cpu.cfs_quota_us", intern = T))
      cfs_period_us <- as.numeric(system("cat /sys/fs/cgroup/cpu/cpu.cfs_period_us", intern = T))
      cores <- floor(cfs_quota_us / cfs_period_us)
      if (cores < 1) {
        return(NULL)
      }
      return(cores)
    },
    error = function(cond) {
      return(NULL)
    },
    warning = function(cond) {
      return(NULL)
    }
  )
  return(cores)
}

#' @title getSimulationParameterDisplayPaths
#' @param parameterPaths Paths of a parameter in simulation
#' @param simulation `Simulation` object from `ospsuite`
#' @param dictionary parameterDisplayPaths data.frame mapping user defined display names
#' @return parameterDisplayPath
#' @export
#' @family workflow helpers
getSimulationParameterDisplayPaths <- function(parameterPaths, simulation, dictionary) {
  parameterDisplayPaths <- ospsuite::getParameterDisplayPaths(parameterPaths, simulation)

  for (parameterIndex in seq_along(parameterPaths)) {
    # Get the index of parameter in dictionary if defined
    dictionaryIndex <- which(parameterPaths[parameterIndex] %in% dictionary$parameter)
    if (!isOfLength(dictionaryIndex, 0)) {
      # Since dictionaryIndex is not null, use first element
      # user should already have a warning if a parameter path is defined
      # more than once in workflow$parameterDisplayPaths
      # as.character enforces character is used instead of levels
      parameterDisplayPaths[parameterIndex] <- as.character(dictionary$displayPath[dictionaryIndex[1]])
    }
  }
  return(parameterDisplayPaths)
}

#' @title setWorkflowParameterDisplayPathsFromFile
#' @description Set mapping between parameters and their display paths in a workflow
#' to replace standard display of parameter paths.
#' @param fileName name of file that includes mapping of Parameters with their display paths
#' Names in header should include `parameter` and `displayPath`.
#' @param workflow Object of class `MeanModelWorkflow` or `PopulationWorkflow`
#' @export
#' @family workflow helpers
setWorkflowParameterDisplayPathsFromFile <- function(fileName, workflow) {
  validateIsOfType(workflow, "Workflow")
  validateIsString(fileName)
  validateIsFileExtension(fileName, "csv")
  parameterDisplayPaths <- readObservedDataFile(fileName)
  workflow$setParameterDisplayPaths(parameterDisplayPaths)
  return(invisible())
}

#' @title setWorkflowParameterDisplayPaths
#' @description Set mapping between parameters and their display paths in a workflow
#' to replace standard display of parameter paths.
#' @param parameterDisplayPaths data.frame mapping Parameters with their display paths
#' Variables of the data.frame should include `parameter` and `displayPath`.
#' @param workflow Object of class `MeanModelWorkflow` or `PopulationWorkflow`
#' @export
#' @family workflow helpers
setWorkflowParameterDisplayPaths <- function(parameterDisplayPaths, workflow) {
  validateIsOfType(workflow, "Workflow")
  workflow$setParameterDisplayPaths(parameterDisplayPaths)
  return(invisible())
}

#' @title getWorkflowParameterDisplayPaths
#' @description Get mapping between parameters and their display paths in a workflow
#' to replace standard display of parameter paths.
#' @param workflow Object of class `MeanModelWorkflow` or `PopulationWorkflow`
#' @export
#' @family workflow helpers
getWorkflowParameterDisplayPaths <- function(workflow) {
  validateIsOfType(workflow, "Workflow")
  return(workflow$getParameterDisplayPaths())
}

#' @title parseVariableToObject
#' @description Create an expression of type `objectName$variableName <- variableName`
#' @param objectName Name of the object whose field is updated
#' @param variableName Name of the variable and field of `objectName`
#' @param keepIfNull logical `objectName$variableName <- variableName \%||\% objectName$variableName`
#' @return An expression to `eval()`
#' @importFrom ospsuite.utils %||%
#' @keywords internal
parseVariableToObject <- function(objectName, variableName, keepIfNull = FALSE) {
  if (keepIfNull) {
    return(parse(text = paste0(objectName, "$", variableName, " <- ", variableName, " %||% ", objectName, "$", variableName)))
  }
  return(parse(text = paste0(objectName, "$", variableName, " <- ", variableName)))
}

#' @title parseVariableFromObject
#' @description Create an expression of type `variableName <- objectName$variableName`
#' @param objectName Name of the object whose field is updated
#' @param variableName Name of the variable and field of `objectName`
#' @param keepIfNull logical `variableName <- objectName$variableName \%||\% variableName`
#' @return An expression to `eval()`
#' @keywords internal
parseVariableFromObject <- function(objectName, variableName, keepIfNull = FALSE) {
  if (keepIfNull) {
    return(parse(text = paste0(variableName, " <- ", objectName, "$", variableName)))
  }
  return(parse(text = paste0(variableName, " <- ", objectName, "$", variableName)))
}

#' @title calculateGMFE
#' @description Calculate Geometric Mean Fold Error between `x` and `y`.
#' Strictly positive pairs of values are kept in the calculation
#' @param x x values to compare
#' @param y y values to compare
#' @return GMFE
#' @export
calculateGMFE <- function(x, y) {
  positiveValues <- (y > 0 & x > 0)
  log10Error <- log10(y[positiveValues]) - log10(x[positiveValues])
  return(10^(sum(abs(log10Error)) / length(log10Error)))
}


#' @title getObjectNameAsString
#' @description Return the name of an object as a string
#' @param object the name of which is to be returned
#' @return the name of the `object` as a string
#' @keywords internal
getObjectNameAsString <- function(object) {
  return(deparse(substitute(object)))
}

#' @title saveFigure
#' @description Save figure and catches
#' @param plotObject A `ggplot` object
#' @param fileName Name of the file in which `plotObject` is saved
#' @param simulationSetName Name of the simulation set for `PlotTask` results
#' @keywords internal
saveFigure <- function(plotObject, fileName, simulationSetName = NULL) {
  tryCatch(
    {
      ggplot2::ggsave(
        filename = fileName,
        plot = plotObject,
        width = reEnv$defaultPlotFormat$width,
        height = reEnv$defaultPlotFormat$height,
        dpi = reEnv$defaultPlotFormat$dpi,
        units = reEnv$defaultPlotFormat$units
      )
    },
    error = function(e) {
      stop(messages$ggsaveError(fileName, simulationSetName, e))
    }
  )
  return(invisible())
}
