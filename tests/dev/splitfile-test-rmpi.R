library('Rmpi')




if(!(mpi.comm.size() == 0)){
  mpi.close.Rslaves()
}

theNumberOfSlaves <- 5
theFolderName <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/"
theFileName   <- "popData.csv"
theNumberOfCommentLines <- 2

#start 2 R workers (slaves) instances (once per WORKFLOW or once per Task?)
mpi.spawn.Rslaves(nslaves = theNumberOfSlaves)

library('ospsuite')
library('ospsuite.reportingengine')

tempPopDataFiles <- splitPopDataFile(fileName = theFileName,
                                     folderName = theFolderName,
                                     numberOfSlaves = theNumberOfSlaves,
                                     numberOfCommentLines = theNumberOfCommentLines)



#load ospsuite and ospsuite.reportingengine libs on the slaves
mpi.bcast.cmd(library('ospsuite'))
mpi.bcast.cmd(library('ospsuite.reportingengine'))


runpop <- function(popDataFileName,popDataFolderName){

  popDataFilePath <- paste0(popDataFolderName,popDataFileName)
  simfile <- c("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/simpleMobiEventSim.pkml")
  sim <-loadSimulation(simfile,addToCache = FALSE,loadFromCache = FALSE)
  LL <- getEnum(simulationFilePath = simfile)
  print(popDataFilePath)

  popsim.OutputList <- c(LL$Organism$blockA$mol1$Concentration$path)
  op <- getAllQuantitiesMatching(paths = popsim.OutputList, container = sim )
  pop<-loadPopulation(popDataFilePath)
  addOutputs(op,simulation = sim)

  res<-runSimulation(sim,population = pop)
  print(res$count)

  exportResultsToCSV(res,paste0(popDataFolderName,"results",mpi.comm.rank(),".csv"))


}

mpi.bcast.Robj2slave(obj = runpop)
mpi.bcast.Robj2slave(obj = tempPopDataFiles)
mpi.bcast.Robj2slave(obj = theFolderName)

mpi.remote.exec( runpop( popDataFileName = paste0(tempPopDataFiles[mpi.comm.rank()]), popDataFolderName = theFolderName  ) ) #mpi.remote.exec and not mpi.bcast.cmd so we ensure that this process has finished before next process begins

removeTempPopFiles(folderName = theFolderName, fileNamesVec= tempPopDataFiles)

resultsList <- list()
for (n in 1:theNumberOfSlaves){
  resultsList[[n]] <- read.csv(file = paste0(theFolderName,"results",n,".csv"),check.names=FALSE)
}

resultsDf <- do.call("rbind", resultsList)
write.csv(x = resultsDf,file = paste0(theFolderName,"results.csv"),row.names = FALSE)

# for (n in 1:theNumberOfSlaves){
#   file.remove(paste0(theFolderName,"results",n,".csv"))
# }

# tempPopDataFiles <- splitPopDataFile(fileName = fileName,
#                         folderName = folderName,
#                         numberOfSlaves = numberOfSlaves,
#                         numberOfCommentLines = 2)

# nprnt <-function(){
#   print("here's the function cast!")
# }
# mpi.bcast.Rfun2slave(comm=1)

# mpi.bcast.Robj2slave(obj = tempPopDataFiles)
#mpi.bcast.Robj2slave(all = TRUE)
#mpi.bcast.cmd( fls1 <- fls )
#mpi.remote.exec( nprnt() )

# mpi.bcast.cmd(simfile <- c("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/simpleMobiEventSim.pkml"))
# mpi.bcast.cmd(sim <-loadSimulation(simfile,addToCache = FALSE,loadFromCache = FALSE))
# mpi.bcast.cmd(LL <- getEnum(simulationFilePath = simfile))

#mpi.bcast.cmd(print(LL))
# mpi.bcast.cmd( print(tempPopDataFiles[mpi.comm.rank()]) )

mpi.close.Rslaves()


# tempPopDataFiles <- splitPopDataFile(fileName = fileName,
#                                      folderName = folderName,
#                                      numberOfSlaves = numberOfSlaves,
#                                      numberOfCommentLines = 2)

#removeTempPopFiles(folderName = folderName, fileNamesVec= tempPopDataFiles)


#
# # #set simulation file on the slaves
# mpi.bcast.cmd(simfile <- c("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/simpleMobiEventSim.pkml"))
# mpi.bcast.cmd(sim <-loadSimulation(simfile,addToCache = FALSE,loadFromCache = FALSE))
# mpi.bcast.cmd(LL <- getEnum(simulationFilePath = simfile))
# mpi.bcast.cmd(popsim.OutputList <- c(LL$Organism$blockA$mol1$Concentration$path))
# mpi.bcast.cmd(op <- getAllQuantitiesMatching(paths = popsim.OutputList, container = sim ))
#
# # #set C:/Temp/humans-Population#I.csv on slave #I (#I=1,2) (sub-population-table, assuming the split before!)
# mpi.bcast.cmd(popsim.popFile <-paste("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/popData",mpi.comm.rank(),'.csv',sep = ''))
#
# mpi.bcast.cmd( print("ge1") )
# mpi.bcast.cmd( rank = 0 , cmd = print(mpi.comm.rank()) )
# #mpi.bcast.cmd( fls1 <- fls )
#
# #mpi.bcast.Rfun2slave(comm=1)
#
# #mpi.bcast.cmd( print(fls[1]) )
#
# #mpi.bcast.cmd(popsim.popFile <-paste0(folderName,fls[mpi.comm.rank()]))
#
# #mpi.bcast.cmd(popsim.popFile <-paste0(folderName,fls[mpi.comm.rank()]))
#
# #mpi.bcast.cmd(print(popsim.popFile))
#
#
# mpi.bcast.cmd(pop<-loadPopulation(popsim.popFile))
# mpi.bcast.cmd(print(pop$allCovariateNames))
#
# mpi.bcast.cmd(addOutputs(op,simulation = sim))
#
#
# mpi.bcast.cmd(print(sim$outputSelections$allOutputs[[1]]))
# #simprint <-  mpi.remote.exec(sim$outputSelections$allOutputs[[1]])
# #print(simprint)
#
#
# mpi.bcast.cmd(res<-runSimulation(sim,population = pop))
#
# # resNum <-  mpi.remote.exec(res$count)
# # print(resNum)
#
# mpi.bcast.cmd(print(res$count))
# mpi.bcast.cmd(exportResultsToCSV(res,paste("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/results",mpi.comm.rank(),".csv",sep = '')))
# #
# #
#
# #mpi.bcast.cmd(popsim.sourceFile<-c("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/individualPksimSim.pkml"))
#
#
#
# # # #set output on the slaves
# # #mpi.bcast.cmd(popsim.OutputList<-c(LL$Organism$blockA$mol1$Concentration$path))
# #
# # # #set C:/Temp/humans-Population#I.csv on slave #I (#I=1,2) (sub-population-table, assuming the split before!)
# # mpi.bcast.cmd(popsim.popFile <-paste("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/popData",mpi.comm.rank(),'.csv',sep = ''))
# # #
# # # #execute R-code for running population simulation on the slaves
# #
# # mpi.bcast.cmd(pop<-loadPopulation(popsim.popFile))
# # mpi.bcast.cmd(print(paste("cheers",mpi.comm.rank())))
# # mpi.bcast.cmd(addOutputs(popsim.OutputList,simulation = sim))
# # mpi.bcast.cmd(res<-runSimulation(sim,population = pop))
# # #
# # # #export results on slave #I to c:/Temp/result#I.csv (I=1,2)
# # #mpi.bcast.cmd(exportResultsToCSV(res,paste("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/results",mpi.comm.rank(),".csv",sep = '')))
# #
# # mpi.bcast.cmd(dF <- data.frame("name" = c("Joe","John","Mike"),"age" = c(3,4,5) ))
# # mpi.bcast.cmd( write.csv(x = dF , paste0("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/dFfile",mpi.comm.rank(),".csv")) )
# #
#
#
#
# #close R slaves
# mpi.close.Rslaves()

#removeTempPopFiles(folderName = folderName, fileNamesVec= fls)

#close MPI (at the end of the workflow)
#mpi.exit()







# #getall quantities
# #add them up using rmpi
#
# library(Rmpi)
#
# ObjThng <- R6::R6Class(
#   "ObjThng",
#   public = list(
#     par1 = NULL,
#
#     initialize = function(){
#       print("new one")
#     },
#
#     func1 = function(){
#       print("here's function 1")
#     }
#
#   )
# )
#
# frnd <- ObjThng$new()
#
# #frnd <- ObjThng$new()
#
#
# if(!(mpi.comm.size() == 0)){
#   mpi.close.Rslaves()
# }
#
# mpi.spawn.Rslaves(nslaves=2)
# # prt <- function(a){
# #   return(a)
# # }
#
# print("start broadcast")
# mpi.bcast.cmd(x<-mpi.bcast.Robj())
# x<-c("This is it")
# mpi.bcast.Robj(x)
# res <-  mpi.remote.exec(x,simplify = TRUE)
# print("end broadcast")
#
#
# #trr <- mpi.remote.exec(cmd = prt , 4,simplify = TRUE , ret= TRUE)
# #print(trr)
# #mpi.remote.exec(rnorm,4)
# #mpi.remote.exec( mpi.comm.rank())
# #print(mpi.comm.size())
# #print(mpi.comm.rank())
# mpi.close.Rslaves()
#
#
#
#
# # library(Rmpi)
# # mpi.spawn.Rslaves(nslaves=3)
# # mpi.bcast.cmd(x<-mpi.bcast.Robj())
# # x<-c("This is a test.")
# # #2. Send x to each slave
# # mpi.bcast.Robj(x)
# # #3. Print x in each slave
# # mpi.remote.exec(x)
