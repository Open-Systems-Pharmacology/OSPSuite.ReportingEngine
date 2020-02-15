defaultFileNames <- list(
  simulationResultsFile = function(simulationName) {
    getDefaultFileName(simulationName, suffix = "Results")
  },
  populationDataFile = function(populationName) {
    getDefaultFileName(populationName, suffix = "PopulationData")
  },
  popSimulationResultsFile = function(populationName, simulationName) {
    getDefaultFileName(populationName, simulationName, suffix = "Results")
  },
  pkAnalysisResultsFile = function(simulationName) {
    getDefaultFileName(simulationName, suffix = "PKAnalysis")
  },
  sensitivityAnalysisResultsFile = function(simulationName) {
    getDefaultFileName(simulationName, suffix = "SensitivityAnalysis")
  },
  workflowFolder = function(name = "Workflow") {
    getDefaultFolderName(name, suffix = paste0("_", format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M%S")), sep = "")
  },
  inputFolder = function(name = "Inputs") {
    getDefaultFolderName(name, sep = "")
  },
  simulationResultsFolder = function(name = "SimulationResults") {
    getDefaultFolderName(name, sep = "")
  },
  pkAnalysisResultsFolder = function(name = "PKAnalysisResults") {
    getDefaultFolderName(name, sep = "")
  },
  sensitivityAnalysisResultsFolder = function(name = "SensitivityAnalysisResults") {
    getDefaultFolderName(name, sep = "")
  },
  reportFolder = function(name = "Report") {
    getDefaultFolderName(name, sep = "")
  },
  resultsFolder = function(name = "Results") {
    getDefaultFolderName(name, sep = "")
  },
  figuresFolder = function(name = "Figures") {
    getDefaultFolderName(name, sep = "")
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
getDefaultFolderName <- function(..., suffix = "", sep = "-") {
  defaultFolderName <- paste(..., suffix, sep = sep)
  return(defaultFolderName)
}
