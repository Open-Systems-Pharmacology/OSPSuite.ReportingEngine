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
  pkRatioResultsFile = function(simulationSetName) {
    getDefaultFileName(simulationSetName, suffix = "PKRatioResults")
  },
  sensitivityAnalysisResultsFile = function(simulationSetName) {
    getDefaultFileName(simulationSetName, suffix = "SensitivityAnalysisResults")
  },
  workflowFolderPath = function(name = "Workflow", folder = getwd()) {
    file.path(folder, getDefaultFolderName(name, suffix = paste0("_", startDate(), "_", startTime()), sep = ""))
  },
  inputFolder = function(name = NULL) {
    getDefaultFolderName(name, suffix = "Inputs", sep = ifNotNull(name, "-", ""))
  },
  simulationResultsFolder = function(name = NULL) {
    getDefaultFolderName(name, suffix = "SimulationResults", sep = ifNotNull(name, "-", ""))
  },
  pkAnalysisResultsFolder = function(name = NULL) {
    getDefaultFolderName(name, suffix = "PKAnalysisResults", sep = ifNotNull(name, "-", ""))
  },
  sensitivityAnalysisResultsFolder = function(name = NULL) {
    getDefaultFolderName(name, suffix = "SensitivityAnalysisResults", sep = ifNotNull(name, "-", ""))
  },
  popSensitivityResultsIndexFile = function(name = NULL) {
    getDefaultFileName(name, suffix = "popSensitivityResultsIndex")
  },
  reportName = function(name = "Report") {
    getDefaultFolderName(name, suffix = NULL, sep = "")
  },
  resultsFolder = function(name = NULL) {
    getDefaultFolderName(name, suffix = "Results", sep = ifNotNull(name, "-", ""))
  },
  figuresFolder = function(name = NULL) {
    getDefaultFolderName(name, suffix = "Figures", sep = ifNotNull(name, "-", ""))
  },
  logInfoFile = function(name = "info") {
    getDefaultFileName("log", suffix = name, extension = "txt", sep = "-")
  },
  logErrorFile = function(name = "error") {
    getDefaultFileName("log", suffix = name, extension = "txt", sep = "-")
  },
  logDebugFile = function(name = "debug") {
    getDefaultFileName("log", suffix = name, extension = "txt", sep = "-")
  },
  resultID = function(...) {
    paste(removeForbiddenLetters(c(...)), collapse = "_")
  }
)

#' @title getDefaultFileName
#' @description Add suffix and extension to create a default file name
#' for population files, results files and PK-analysis files.
#' @param ... names
#' @param suffix Suffix to be added at the end of the file name
#' @param extension file format. Default is `csv`
#' @param sep separation between names and suffix. Default is `-`
#' @return default filename adding default suffix and file extension
#' @keywords internal
getDefaultFileName <- function(..., suffix, extension = "csv", sep = "-") {
  defaultName <- paste(..., suffix, sep = sep)
  defaultFileName <- paste0(defaultName, ".", extension)
  return(defaultFileName)
}

#' @title getDefaultFolderName
#' @description Create a default folder name for reporting engine results and figures
#' @param ... names
#' @param suffix Suffix to be added at the end of the folder name.
#' Default `suffix` is the current date
#' @param sep separation between names and suffix. Default is `-`
#' @return default filename adding default suffix and file extension
#' @keywords internal
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

defaultWorkflowReferences <- list(
  "plotGoF" = "time-profiles",
  "plotPKParameters" = "pk-parameters",
  "plotMassBalance" = "mass-balance",
  "plotAbsorption" = "absorption",
  "plotSensitivity" = "sensitivity-analysis",
  "plotDemography" = "demography"
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
  "simulate" = "Simulation task",
  "calculatePKParameters" = "Calculate PK Parameters task",
  "sensitivityAnalysis" = "Sensitivity Analysis task",
  "plotGoF" = "Plot Time profiles and Residuals task",
  "plotPKParameters" = "Plot PK Parameters task",
  "plotMassBalance" = "Plot Mass Balance task",
  "plotAbsorption" = "Plot Absorption task",
  "plotSensitivity" = "Plot Sensitivity task",
  "plotDemography" = "Plot Demography task",
  "plotTimeProfiles" = "Plot Time Profiles task",
  "plotGOFMerged" = "Plot Merged Goodness of Fit task",
  "plotComparisonTimeProfiles" = "Plot Comparison Time Profiles task",
  "plotPKRatio" = "Plot PK Ratio task",
  "plotDDIRatio" = "Plot DDI Ratio task"
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
