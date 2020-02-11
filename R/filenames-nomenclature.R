defaultFileNames <- list(
  populationFile = function(populationName) {
    getDefaultFileName(populationName, suffix = "Population")
  },
  resultsFile = function(simulationName) {
    getDefaultFileName(simulationName, suffix = "Results")
  },
  popResultsFile = function(populationName, simulationName) {
    getDefaultFileName(populationName, simulationName, suffix = "Results")
  },
  pkAnalysisFile = function(simulationName) {
    getDefaultFileName(simulationName, suffix = "PK-Analyses")
  },
  reportFolder = function(name = "Report") {
    getDefaultFolderName(name)
  },
  resultsFolder = function(name = "Results") {
    getDefaultFolderName(name)
  },
  figuresFolder = function(name = "Figures") {
    getDefaultFolderName(name)
  }
)

#' @title getDefaultFileName
#' @description Add suffix and extension to create a default file name
#' for population files, results files and PK-analysis files.
#' @param ... names
#' @param suffix Suffix to be added at the end of the file name
#' @param extension file format. Default is `csv`
#' @param sep separation between names and suffix. Default is `-`
#' @return default filename adding default suffix and fileextension
getDefaultFileName <- function(..., suffix, extension = "csv", sep = "-") {
  defaultName <- paste(..., suffix, sep = sep)
  defaultFileName <- paste0(defaultName, ".", extension)
  return(defaultFileName)
}

#' @title getDefaultFolderName
#' @description Create a default folder name for reporting engine results and figures
#' @param ... names
#' @param suffix Suffix to be added at the end of the folder name.
#' Dafault `suffix` is the curent date
#' @param sep separation between names and suffix. Default is `-`
#' @return default filename adding default suffix and fileextension
getDefaultFolderName <- function(..., suffix = format(Sys.Date(), "%Y%m%d"), sep = "-") {
  defaultFolderName <- paste(..., suffix, sep = sep)
  return(defaultFolderName)
}
