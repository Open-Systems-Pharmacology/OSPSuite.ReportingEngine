rm(list=ls())
library(ospsuite)
library(ospsuite.reportingengine)


wbsim <- loadSimulation("./data/individualPksimSim.pkml")




#pth <- getAllPathStrings(wbsim,"**|Organism|**|smarties")


getCompoundsList <- function(simulation){
  moleculePaths <- getAllMoleculesMatching(paths = "**" , container = wbsim)
  compoundsInPaths <- NULL
  for (env in moleculePaths){
    str <- toPathArray(env$path)
    compoundsInPaths <- c(compoundsInPaths,tail(str, 1))
  }
  uniqueCompounds <- unique(compoundsInPaths)
  return(uniqueCompounds)
}

compounds <- getCompoundsList(wbsim)


pth=list()
for (cmp in compounds){

  pth[[cmp]] <- getAllMoleculesMatching(paths = paste0("**|",cmp) , container = wbsim)

}




# print(wbsim$outputSelections)
#
# pth <- getAllMoleculesMatching(paths = "**" , container = wbsim)
#
# addOutputs(quantitiesOrPaths = pth[1], simulation = wbsim)
#
# print(wbsim$outputSelections)

#What if we already have an observer?
#Need separate list of paths for mass balance that excludes observers and only includes amounts in moles
