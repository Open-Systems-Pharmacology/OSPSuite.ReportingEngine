rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)
library(Rmpi)


performSensitivityAnalysis <- function(simFileName,
                                       simFileFolder,
                                       paramRange=NULL,
                                       pkParameterName = "AUC",
                                       totalSensitivityThreshold = 1){
  sim<-loadSimulation(paste0(simFileFolder,simFileName))
  allParameters <- ospsuite::getAllParametersMatching(paths = "**" , container = sim)
  outputSelections <- sim$outputSelections

  if (is.null(paramRange)){
    parameters <- allParameters
  }
  else{
    parameters <- allParameters[ paramRange ]
  }

  sensitivityAnalysis <- SensitivityAnalysis$new(simulation = sim,
                                                 parameters = parameters )

  sensitivityAnalysisRunOptions <- SensitivityAnalysisRunOptions$new(showProgress = FALSE)

  results <- runSensitivityAnalysis(sensitivityAnalysis = sensitivityAnalysis,
                                    sensitivityAnalysisRunOptions = sensitivityAnalysisRunOptions)

  pkSensitivities <- list()
  for (output in outputSelections$allOutputs) {
    pkSensitivities <- results$allPKParameterSensitivitiesFor(
      pkParameterName = "AUC",
      outputPath = output$path,
      totalSensitivityThreshold = totalSensitivityThreshold
    )
  }
  print(pkSensitivities)
}


#simFileName <- "individualPksimSim.pkml"
simFileName   <- "simpleMobiEventSim.pkml"
simFileFolder <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/"

sim<-loadSimulation(paste0(simFileFolder,simFileName))
#sim<-loadSimulation("./data/simpleMobiEventSim.pkml")

allParameters <- ospsuite::getAllParametersMatching(paths = "**" , container = sim)
totalNumberParameters <- length(allParameters)

numberOfCores <- 1
numParametersPerCore <- ceiling(totalNumberParameters/numberOfCores)

paramIndices <- list()
if (numberOfCores>1){
  for (n in 1:(numberOfCores-1)){
    paramIndices[[n]] <- c( ((n-1)*numParametersPerCore)+1 , (n*numParametersPerCore))
  }
  paramIndices[[numberOfCores]] <- c((((numberOfCores-1))*numParametersPerCore)+1,totalNumberParameters)
  mpi.spawn.Rslaves(nslaves = numberOfCores)
  mpi.bcast.cmd(library('ospsuite'))
  mpi.bcast.cmd(library('ospsuite.reportingengine'))
  mpi.bcast.Robj2slave(obj = simFileName)
  mpi.bcast.Robj2slave(obj = simFileFolder)
  mpi.bcast.Robj2slave(obj = paramIndices)
  mpi.bcast.cmd(performSensitivityAnalysis(simFileName,simFileFolder,paramIndices[[mpi.comm.rank()]]))
  mpi.close.Rslaves()
} else
{
  paramIndices[[1]] <- c(1,totalNumberParameters)
  resa <- performSensitivityAnalysis(simFileName,simFileFolder)
}










