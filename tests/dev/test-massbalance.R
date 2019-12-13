rm(list=ls())
library(ospsuite)
library(ospsuite.reportingengine)


wbsim <- loadSimulation("./data/individualPksimSim.pkml")

compounds <- getCompoundsList(wbsim)
pths <- getPathsForMoleculeAmount(simulation = wbsim,compounds[1])


print(wbsim$outputSelections)
addOutputs(quantitiesOrPaths = pths[c(131,132)], simulation = wbsim)
print(wbsim$outputSelections)

res <- ospsuite::runSimulation(wbsim)
df<-getOutputValues(simulationResults = res,quantitiesOrPaths = res$allQuantityPaths , individualIds = 0 )


# #Are molecule amounts the intersection of getAllContainersMatching and getALlMoleculesMatching ???
# #NO - some parameters in containers, use getALlMoleculesMatching for mass balance, but you need to exclude Applications
#
# mn <- getAllMoleculesMatching(paths = "**|smarties" , container = wbsim)
# cn <- getAllContainersMatching(paths = "**|smarties" , container = wbsim)
#
# cn_strs <- NULL
# for (pp in cn){
#   cn_strs <- c(cn_strs,pp$path)
# }
#
#
# mn_strs <- NULL
# for (pp in mn){
#   mn_strs <- c(mn_strs,pp$path)
# }





# pth=list()
# for (cmp in compounds){
#
#   pth[[cmp]] <- getAllMoleculesMatching(paths = paste0("**|",cmp) , container = wbsim)
#
# }




# print(wbsim$outputSelections)
#
# pth <- getAllMoleculesMatching(paths = "**" , container = wbsim)
#
# addOutputs(quantitiesOrPaths = pth[1], simulation = wbsim)
#
# print(wbsim$outputSelections)

#What if we already have an observer?
#Need separate list of paths for mass balance that excludes observers and only includes amounts in moles
