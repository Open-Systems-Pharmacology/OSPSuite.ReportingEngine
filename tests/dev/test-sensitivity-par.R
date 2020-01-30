rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)
library(Rmpi)

#INPUTS ARE FOLDER OR PATH TO SIMULATION, LIST OF PARAMETERS (OPTIONAL,DEFAULT NULL) AND NUMBER OF CORES (OPTIONAL, DEFAULT 1), LIST OF OUTPUTS (OPTIONAL??), LIST OF PK PARAMS (OPTIONAL DEFAULT NULL)
simFilePath <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/individualPksimSim.pkml"
#simFilePath <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/simpleMobiEventSim_nonzeroinitial.pkml"
numberOfCores <- 4
resultsFileFolder <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/tests/dev/"
resultsFileName <- "SAResults"
allParameters <- NULL

sim <- loadSimulation(simFilePath,loadFromCache = FALSE)

if (is.null(allParameters)){
  allParameters <- ospsuite::potentialVariableParameterPathsFor(simulation =  sim)
}


totalNumberParameters <- length(allParameters)
numberOfCores <- min(numberOfCores,totalNumberParameters) #in case there are more cores specified in numberOfCores that there are parameters, this ensures at least one parameter per spawned core

if (totalNumberParameters == 0){
  stop("No variable parameters found for sensitivity analysis.")
}


if (numberOfCores > 1) {
  #Parallelizing among a total of min(numberOfCores,totalNumberParameters) cores
  seqVec <- (1+( (1:totalNumberParameters) %% numberOfCores)) #Create a vector, of length totalNumberParameters, consisting of a repeating sequence of integers from 1 to numberOfCores
  sortVec <- sort(seqVec) #Sort seqVec to obtain an concatenated array of repeated integers, with the repeated integers ranging from from 1 to numberOfCores.  These are the core numbers to which each parameter will be assigned.
  listSplitParameters <- split(x = allParameters, sortVec) #Split the parameters of the model according to sortVec
  #listSplitParameters <- split(x = allParameters, sort(1+( (1:length(totalNumberParameters)) %% numberOfCores))  )
  mpi.spawn.Rslaves(nslaves = numberOfCores)
  mpi.bcast.cmd(library("ospsuite"))
  mpi.bcast.cmd(library("ospsuite.reportingengine"))

  mpi.bcast.Robj2slave(obj = simFilePath)
  mpi.bcast.Robj2slave(obj = listSplitParameters)
  mpi.bcast.Robj2slave(obj = resultsFileFolder)
#Send individual ID and pop file and command to update sim here.

  allResultsFileNames<-sapply(X = 1:numberOfCores ,function(x,resultsFileFolder,resultsFileName){
    return(paste0(resultsFileFolder,resultsFileName,"_",x,".csv"))},
    resultsFileFolder = resultsFileFolder,
    resultsFileName = resultsFileName,
    USE.NAMES = FALSE)

  mpi.bcast.Robj2slave(obj = allResultsFileNames)
#######
  mpi.remote.exec(ospsuite.reportingengine::analyzeSensitivity(simFilePath = simFilePath,
                                                               perturbationParameterNamesVector = listSplitParameters[[mpi.comm.rank()]],
                                                               totalSensitivityThreshold = 1,
                                                               resultsFilePath = allResultsFileNames[mpi.comm.rank()]),
                                                               numberOfCoresToUse = 1)


  mpi.close.Rslaves()


  combinedSensitivityAnalysisResults <- ospsuite::importSensitivityAnalysisResultsFromCSV(simulation = sim,filePaths = allResultsFileNames)
  ospsuite::exportSensitivityAnalysisResultsToCSV(results = combinedSensitivityAnalysisResults,
                                                  filePath = paste0(resultsFileFolder,resultsFileName,".csv"))

} else {
  #No parallelization
  ospsuite.reportingengine::analyzeSensitivity(simFilePath = simFilePath,
                                               perturbationParameterNamesVector = allParameters,
                                               totalSensitivityThreshold = 1,
                                               resultsFilePath = paste0(resultsFileFolder,resultsFileName,".csv"))
}







#sort(1+(1:length(dd) %% (length(dd)+10))

#print("...done")


#Do we need this?
#numParametersPerCore <- ceiling(totalNumberParameters / numberOfCores) #Do we need this?


#For a sensitivity analysis, need to specify:
# - parameters to perturb (eg clearance rate)
# - outputs of interest (eg interstitial liver concentration of drug)
# - pk parameter functions to be applied to outputs of interest

#The output will be a list of objects that have at least the following fields:
# PKParameterSensitivity:
#   Parameter path: Organism-blockA-mol1init
# PK-Parameter: AUC
# Output path: Organism|blockB|mol1|twice_mol1_blockB
# Value: 1.00001383813318

#For parallelization, need to split the list of parameters to perturb among the N cores.
#Can we use the split function?

#Finally, need to export the results

#x = c("paul","john","george","ringo","freddie","mick")
# split(x, 1:length(x) %% 2 )




