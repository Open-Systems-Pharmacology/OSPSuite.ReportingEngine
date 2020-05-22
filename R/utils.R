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
#' @description
#' #Generate a listcontaining names of CSV result files that will be output by each corein parallel computation
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

#' @title getFileExtension
#' @param filePath character string containing the name of the path or file to trim
#' @return extension character string of the trimmed filed name
#' @description
#' Get extension of a file
#' @examples
#' \dontrun{
#' pathName <- "folder/subfolder/testFile.txt"
#' getFileExtension(pathName)
#' }
#' @export
getFileExtension <- function(filePath) {
  extension <- sub(
    pattern = "^.*[.]",
    replacement = "",
    x = filePath
  )
  return(extension)
}

#' @title loadSimulationWithUpdatedPaths
#' @param simulationSet simulation set containing path to simulation file and pathIDs for quantities to be loaded into simulation object
#' @return simulation object with pathIDs updated from simulationSet
#' @export
loadSimulationWithUpdatedPaths <- function(simulationSet) {
  sim <- ospsuite::loadSimulation(
    filePath = simulationSet$simulationFile,
    loadFromCache = FALSE,
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
  
  if(!is.null(simulationSet$studyDesignFile)){
    addStudyParameters(population, simulation, simulationSet$studyDesignFile)
  }
  return(population)
}

#' @title lastPathElement
#' @param path simulation path
#' @return last path element as character string
#' @export
lastPathElement <- function(path) {
  return(sub(
    pattern = "^.*[|]",
    replacement = "",
    x = path
  ))
}
