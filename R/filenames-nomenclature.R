defaultFileNames <- list(
  simulationResultsFile = function(simulationName) {
    getDefaultFileName(simulationName, suffix = "Results")
  },
  popSimulationResultsFile = function(populationName, simulationName) {
    getDefaultFileName(populationName, simulationName, suffix = "SimulationResults")
  },
  pkAnalysisResultsFile = function(simulationName) {
    getDefaultFileName(simulationName, suffix = "PKAnalysisResults")
  },
  sensitivityAnalysisResultsFile = function(simulationName) {
    getDefaultFileName(simulationName, suffix = "SensitivityAnalysisResults")
  },
  workflowFolder = function(name = "Workflow") {
    getDefaultFolderName(name, suffix = paste0("_", format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M%S")), sep = "")
  },
  inputFolder = function(name = NULL) {
    getDefaultFolderName(name, suffix="Inputs", sep = ifnotnull(name,"-",""))
  },
  simulationResultsFolder = function(name = NULL) {
    getDefaultFolderName(name, suffix =  "SimulationResults", sep = ifnotnull(name,"-",""))
  },
  pkAnalysisResultsFolder = function(name = NULL) {
    getDefaultFolderName(name, suffix = "PKAnalysisResults" , sep = ifnotnull(name,"-",""))
  },
  sensitivityAnalysisResultsFolder = function(name = NULL ) {
    getDefaultFolderName(name, suffix = "SensitivityAnalysisResults" , sep = ifnotnull(name,"-",""))
  },
  reportFolder = function(name = NULL) {
    getDefaultFolderName(name, suffix = "Report" , sep = ifnotnull(name,"-",""))
  },
  resultsFolder = function(name = NULL) {
    getDefaultFolderName(name, suffix = "Results", sep = ifnotnull(name,"-",""))
  },
  figuresFolder = function(name = NULL) {
    getDefaultFolderName(name, suffix = "Figures" , sep = ifnotnull(name,"-",""))
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
