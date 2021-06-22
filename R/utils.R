#' Shortkey checking if argument 1 is not null,
#' output argument 1 if not null, or output argument 2 otherwise
#'
#' @title \%||\%
#' @param lhs argument 1
#' @param rhs argument 2
#' @return lhs if lhs is not null, rhs otherwise
#' @description
#' Check if lhs argument is not null, output lhs if not null,
#' output rhs otherwise
`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) {
    lhs
  } else {
    rhs
  }
}

#' Shortkey checking if argument 1 is not null,
#' output the argument 2 if not null, or output argument 3 otherwise
#'
#' @title ifnotnull
#' @param inputToCheck argument 1
#' @param outputIfNotNull argument 2
#' @param outputIfNull argument 3
#' @return outputIfNotNull if inputToCheck is not null, outputIfNull otherwise
#' @description
#' Check if inputToCheck is not null, if so output outputIfNotNull,
#' otherwise, output outputIfNull
ifnotnull <- function(inputToCheck, outputIfNotNull, outputIfNull = NULL) {
  if (!is.null(inputToCheck)) {
    outputIfNotNull
  } else {
    outputIfNull
  }
}

#' Shortkey checking if arguments 1 and 2 are equal,
#' output argument 3 if equal, or output argument 4 otherwise
#'
#' @title ifEqual
#' @param x argument 1
#' @param y argument 2
#' @param outputIfEqual argument 3
#' @param outputIfNotEqual argument 4
#' @return outputIfEqual if x=y, outputIfNotEqual otherwise
#' @description
#' Check if x=y, if so output outputIfEqual,
#' otherwise, output outputIfNotEqual
ifEqual <- function(x, y, outputIfEqual, outputIfNotEqual = NULL) {
  if (x == y) {
    outputIfEqual
  } else {
    outputIfNotEqual
  }
}

#' Shortkey checking if arguments 1 is included in 2,
#' output argument 3 if included, or output argument 4 otherwise
#'
#' @title ifIncluded
#' @param x argument 1
#' @param y argument 2
#' @param outputIfIncluded argument 3
#' @param outputIfNotIncluded argument 4
#' @return outputIfIncluded if x=y, outputIfNotIncluded otherwise
#' @description
#' Check if x is in y, if so output outputIfIncluded,
#' otherwise, output outputIfNotIncluded
ifIncluded <- function(x, y, outputIfIncluded, outputIfNotIncluded = NULL) {
  if (isIncluded(x, y)) {
    outputIfIncluded
  } else {
    outputIfNotIncluded
  }
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
removeForbiddenLetters <- function(text, forbiddenLetters = "[[:punct:]]", replacement = "_") {
  text <- gsub(
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
#' @param loadFromCache logical allows load from Cache option
#' @return simulation object with pathIDs updated from simulationSet
#' @export
loadSimulationWithUpdatedPaths <- function(simulationSet, loadFromCache = FALSE) {
  sim <- ospsuite::loadSimulation(
    filePath = simulationSet$simulationFile,
    loadFromCache = loadFromCache,
    addToCache = FALSE
  )
  # Prevent loadSimulationWithUpdatedPaths from crashing if user did not submit any pathID
  if (!is.null(simulationSet$outputs)) {
    sim$outputSelections$clear()
    paths <- sapply(simulationSet$outputs, function(output) {
      output$path
    })
    ospsuite::addOutputs(quantitiesOrPaths = paths, simulation = sim)
  }
  return(sim)
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

#' @title removeInf
#' @param data numeric vector
#' @param logFolder folder where the logs are saved
#' @return numeric vector
replaceInfWithNA <- function(data, logFolder = getwd()) {
  infData <- is.infinite(data)
  Ninf <- sum(infData)
  if (Ninf > 0) {
    logWorkflow(
      message = paste0(Ninf, " values were infinite and transformed into missing values (NA)"),
      pathFolder = logFolder,
      logTypes = c(LogTypes$Debug, LogTypes$Error)
    )
  }
  data[infData] <- NA
  return(data)
}

#' @title removeMissingValues
#' @param data data.frame
#' @param dataMapping name of variable on which the missing values ar checked
#' @param logFolder folder where the logs are saved
#' @return filtered data.frame
removeMissingValues <- function(data, dataMapping = NULL, logFolder = getwd()) {
  data[, dataMapping] <- replaceInfWithNA(data[, dataMapping], logFolder)
  naData <- is.na(data[, dataMapping])
  Nna <- sum(naData)
  data <- data[!naData, ]

  if (Nna > 0) {
    logWorkflow(
      message = paste0(Nna, " values were missing (NA) from variable '", dataMapping, "' and removed from the analysis"),
      pathFolder = logFolder,
      logTypes = c(LogTypes$Debug, LogTypes$Error)
    )
  }
  return(data)
}

#' @title getPKParametersInOutput
#' @param output Output object
#' @return Names of pkParameters in `output`
#' @export
getPKParametersInOutput <- function(output) {
  validateIsOfType(output, "Output")
  pkParameters <- sapply(output$pkParameters, function(pkParameterInfo) {
    pkParameterInfo$pkParameter
  })
  if (isOfLength(pkParameters, 0)) {
    return(NA)
  }
  return(pkParameters)
}

#' @title getOutputPathsInSimulationSet
#' @param simulationSet SimulationSet object or derived class
#' @return Path names of outputs in `simulationSet`
#' @export
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
getPKParametersInSimulationSet <- function(simulationSet) {
  validateIsOfType(simulationSet, "SimulationSet")
  pkParametersTable <- data.frame()
  for (output in simulationSet$outputs) {
    pkParametersTable <- rbind.data.frame(
      pkParametersTable,
      data.frame(
        path = output$path,
        pkParameter = getPKParametersInOutput(output),
        stringsAsFactors = FALSE
      )
    )
  }
  return(pkParametersTable)
}

#' @title getAllowedCores
#' @return Allowed number of CPU cores for computation
getAllowedCores <- function() {
  cores <- tryCatch(
    {
      # get cpu allowance from files
      cfs_quota_us <- as.numeric(system("cat /sys/fs/cgroup/cpu/cpu.cfs_quota_us", intern = T))
      cfs_period_us <- as.numeric(system("cat /sys/fs/cgroup/cpu/cpu.cfs_period_us", intern = T))
      cfs_quota_us / cfs_period_us
    },
    error = function(cond) {
      return(NULL)
    },
    warning = function(cond) {
      return(NULL)
    }
  )
}

#' @title getSimulationParameterDisplayPaths
#' @param parameterPaths Paths of a parameter in simulation
#' @param simulation Simulation object
#' @param dictionary parameterDisplayPaths data.frame mapping user defined display names
#' @return parameterDisplayPath
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
getWorkflowParameterDisplayPaths <- function(workflow) {
  validateIsOfType(workflow, "Workflow")
  return(workflow$getParameterDisplayPaths())
}


formatNumerics <- function(numerics,
                           digits = NULL,
                           nsmall = NULL,
                           scientific = NULL) {
  validateIsInteger(digits, nullAllowed = TRUE)
  validateIsInteger(nsmall, nullAllowed = TRUE)
  validateIsLogical(scientific, nullAllowed = TRUE)

  digits <- digits %||% reEnv$formatNumericsDigits
  nsmall <- nsmall %||% reEnv$formatNumericsSmall
  scientific <- scientific %||% reEnv$formatNumericsScientific

  return(format(numerics, digits = digits, nsmall = nsmall, scientific = scientific))
}

#' @title parseVariableToObject
#' @description Create an expression of type `objectName$variableName <- variableName`
#' @param objectName Name of the object whose field is updated
#' @param variableName Name of the variable and field of `objectName`
#' @param keepIfNull logical `objectName$variableName <- variableName %||% objectName$variableName`
#' @return An expression to `eval()`
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
#' @param keepIfNull logical `variableName <- objectName$variableName %||% variableName`
#' @return An expression to `eval()`
parseVariableFromObject <- function(objectName, variableName, keepIfNull = FALSE) {
  if (keepIfNull) {
    return(parse(text = paste0(variableName, " <- ", objectName, "$", variableName)))
  }
  return(parse(text = paste0(variableName, " <- ", objectName, "$", variableName)))
}
