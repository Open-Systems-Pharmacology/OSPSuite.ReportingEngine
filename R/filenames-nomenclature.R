startTime <- function() {
  return(format(Sys.time(), "%H%M%S"))
}
startDate <- function() {
  return(format(Sys.Date(), "%Y%m%d"))
}

defaultFileNames <- list(
  simulationResultsFile = function(simulationSetName) {
    getDefaultFileName(simulationSetName, suffix = "SimulationResults")
  },
  pkAnalysisResultsFile = function(simulationSetName) {
    getDefaultFileName(simulationSetName, suffix = "PKAnalysisResults")
  },
  sensitivityAnalysisResultsFile = function(simulationSetName) {
    getDefaultFileName(simulationSetName, suffix = "SensitivityAnalysisResults")
  },
  workflowFolderPath = function(name = "Workflow", folder = getwd()) {
    file.path(folder, getDefaultFolderName(name, suffix = paste0("_", startDate(), "_", startTime()), sep = ""))
  },
  inputFolder = function(name = NULL) {
    getDefaultFolderName(name, suffix = "Inputs", sep = ifnotnull(name, "-", ""))
  },
  simulationResultsFolder = function(name = NULL) {
    getDefaultFolderName(name, suffix = "SimulationResults", sep = ifnotnull(name, "-", ""))
  },
  pkAnalysisResultsFolder = function(name = NULL) {
    getDefaultFolderName(name, suffix = "PKAnalysisResults", sep = ifnotnull(name, "-", ""))
  },
  sensitivityAnalysisResultsFolder = function(name = NULL) {
    getDefaultFolderName(name, suffix = "SensitivityAnalysisResults", sep = ifnotnull(name, "-", ""))
  },
  popSensitivityResultsIndexFile = function(name = NULL) {
    getDefaultFileName(name, suffix = "popSensitivityResultsIndex")
  },
  reportName = function(name = "Report") {
    getDefaultFolderName(name, suffix = NULL, sep = "")
  },
  resultsFolder = function(name = NULL) {
    getDefaultFolderName(name, suffix = "Results", sep = ifnotnull(name, "-", ""))
  },
  figuresFolder = function(name = NULL) {
    getDefaultFolderName(name, suffix = "Figures", sep = ifnotnull(name, "-", ""))
  },
  logInfoFile = function(name = "info") {
    getDefaultFileName("log", suffix = name, extension = "txt", sep = "-")
  },
  logErrorFile = function(name = "error") {
    getDefaultFileName("log", suffix = name, extension = "txt", sep = "-")
  },
  logDebugFile = function(name = "debug") {
    getDefaultFileName("log", suffix = name, extension = "txt", sep = "-")
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

defaultWorkflowTitles <- list(
  "plotGoF" = "Time profiles and residual plots",
  "plotPKParameters" = "PK parameters",
  "plotMassBalance" = "Mass Balance",
  "plotAbsorption" = "Absorption",
  "plotSensitivity" = "Sensitivity Analysis",
  "plotDemography" = "Demography"
)

defaultWorkflowAppendices <- list(
  "plotGoF" = "appendix-time-profile.md",
  "plotPKParameters" = "appendix-pk-parameters.md",
  "plotMassBalance" = "appendix-mass-balance.md",
  "plotAbsorption" = "appendix-absorption.md",
  "plotSensitivity" = "appendix-sensitivity-analysis.md",
  "plotDemography" = "appendix-demography.md"
)

defaultWorkflowMessages <- list(
  "simulate" = "Perform simulation task",
  "calculatePKParameters" = "Calculate PK parameters task",
  "sensitivityAnalysis" = "Perform sensitivity analysis task",
  "plotGoF" = "Plot Time profiles and Residuals task",
  "plotPKParameters" = "Plot PK parameters task",
  "plotMassBalance" = "Plot Mass Balance task",
  "plotAbsorption" = "Plot Absorption task",
  "plotSensitivity" = "Plot Sensitivity task",
  "plotDemography" = "Plot Demography task"
)

defaultTaskOutputFolders <- list(
  "simulate" = "SimulationResults",
  "calculatePKParameters" = "PKAnalysisResults",
  "sensitivityAnalysis" = "SensitivityResults",
  "plotGoF" = "TimeProfiles",
  "plotPKParameters" = "PKAnalysis",
  "plotMassBalance" = "MassBalance",
  "plotAbsorption" = "Absorption",
  "plotSensitivity" = "Sensitivity",
  "plotDemography" = "Demography"
)

