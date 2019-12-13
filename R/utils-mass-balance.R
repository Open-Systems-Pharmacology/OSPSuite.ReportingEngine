#' @export
getCompoundsList <- function(simulation){
  moleculePaths <- getAllMoleculesMatching(paths = "**" , container = simulation)
  compoundsInPaths <- NULL
  for (env in moleculePaths){
    str <- toPathArray(env$path)
    compoundsInPaths <- c(compoundsInPaths,tail(str, 1))
  }
  uniqueCompounds <- unique(compoundsInPaths)
  return(uniqueCompounds)
}

#' @export
getPathsForMoleculeAmount = function(simulation,compound){
  pth <- getAllMoleculesMatching(paths = paste0("**|",compound) , container = simulation)
  return(pth)
}
