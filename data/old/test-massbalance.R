rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)


wbsim <- loadSimulation("./data/individualPksimSim.pkml", loadFromCache = FALSE)

compounds <- getCompoundsList(wbsim)
n <- 1
pths <- getPathsForMoleculeAmount(simulation = wbsim, compounds[n])

pthsVec <- envList2PathStringsVector(pths)

print(wbsim$outputSelections)
addOutputs(quantitiesOrPaths = pths, simulation = wbsim)
print(wbsim$outputSelections)


# mn <- getAllMoleculesMatching(paths = paste0("**|",compounds[n]) , container = wbsim)
# mn_strs <- NULL
# for (pp in mn){
#   mn_strs <- c(mn_strs,pp$path)
# }

res <- ospsuite::runSimulation(wbsim)
resList <- getOutputValues(simulationResults = res, quantitiesOrPaths = res$allQuantityPaths, individualIds = 0)
resDF <- getOutputValuesTLF(simulationResults = res, quantitiesOrPaths = res$allQuantityPaths, individualIds = 0, population = loadPopulation(csvPopulationFile = "./data/popData.csv")) # using this placeholder population CSV file for now...need to make optional the population input to getOutputValuesTLF

# write.csv(resDF$data,file = "./tests/dev/massBalanceTestCSV.csv" )
mx <- read.csv(file = "tests/dev/massBalanceTestCSV.csv", check.names = FALSE)
mx1 <- mx[, 7:ncol(mx)]

amtBalance <- rep(0, nrow(resDF$data))
for (t in seq(1:nrow(resDF$data))) {
  # amtBalance[t] <- sum(mx1[t,]) # sum(resDF$data[t,pthsVec])
  amtBalance[t] <- sum(resDF$data[t, pthsVec])
}


wrdBalance <- rep(0, nrow(resDF$data))
for (t in seq(1:nrow(resDF$data))) {
  # amtBalance[t] <- sum(mx1[t,]) # sum(resDF$data[t,pthsVec])
  wrdBalance[t] <- sum(resDF$data[t, colnames(mx1)])
}


dff <- data.frame(amt = sort(c(pthsVec, "zzz", "zzz")), wrd = sort(colnames(mx1)))


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

# What if we already have an observer?
# Need separate list of paths for mass balance that excludes observers and only includes amounts in moles
