#' @title getCompoundsList
#' @description Function to return a list of the compounds in a simulation.
#' @description Input is a simulation ("simulation") loaded from a PKML file using ospsuite::loadSimulation()
#' @description Output is a vector ("uniqueCompounds") of strings of unique compounds
#' @export
getCompoundsList <- function(simulation){
  moleculePaths <- ospsuite::getAllMoleculesMatching(paths = "**" , container = simulation)
  compoundsInPaths <- NULL
  for (env in moleculePaths){
    str <- ospsuite::toPathArray(env$path)
    compoundsInPaths <- c(compoundsInPaths,tail(str, 1))
  }
  uniqueCompounds <- unique(compoundsInPaths)
  return(uniqueCompounds)
}

#' @title getPathsForMoleculeAmount
#' @description Input is a simulation ("simulation") loaded from a PKML file using ospsuite::loadSimulation() and the name of a compound ("compound") in that simulation
#' @description Output is a list ("pth") of environments pointing to amounts of the compound in the model containers
#' @export
getPathsForMoleculeAmount = function(simulation,compound){
  pth <- ospsuite::getAllMoleculesMatching(paths = paste0("**|",compound) , container = simulation)
  return(pth)
}

#' @title pathStringsVector
#' @description Input is a list envList of environments that is output by the ospsuite::getAllXXXMatching() functions
#' @description Output is a list of the paths in each environment in the input list
#' @export
envList2PathStringsVector = function(envList){
  #envList2PathStringsVector = function(envList,removeSimulationName = FALSE){
  pathStringsVector = NULL


  for (eL in envList){
    pathStringsVector <- c(pathStringsVector,eL$path)
  }

  return(pathStringsVector)
}
