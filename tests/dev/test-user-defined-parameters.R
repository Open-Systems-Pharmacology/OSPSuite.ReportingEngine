rm(list = ls())
devtools::load_all(".")
mmm <- MeanModel$new(modelFilePath = "./data/sim2compounds.pkml", simulationOutputsFolder = "./data", modelDisplayName = "twoCompound")

mmm$simulateMeanModel(saveSimulation = TRUE)
mmm$calculateMassBalance(saveMassBalance = TRUE)



##### USER DEFINED PK PARAMETERS#####
getMyMin <- function(x = NULL, y) {
  return(min(y))
}

getMyMax <- function(x = NULL, y) {
  return(max(y))
}
###################################

udPKFunctionArray <- c(
  UserDefinedPKFunction$new(pKParameterName = "myMin", pKFunction = getMyMin, pKParameterUnit = "umol"), ### Manually enter units?
  UserDefinedPKFunction$new(pKParameterName = "myMax", pKFunction = getMyMax, pKParameterUnit = "umol") ### Manually enter units?
)


mmm$calculateMeanModelPKParameters(userDefinedPKFunctions = udPKFunctionArray, savePKAnalysis = TRUE)
