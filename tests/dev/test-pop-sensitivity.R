rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)
devtools::load_all("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine")

inputFolderName <- "C:/Users/ahamadeh/Dropbox/rproject/workflow"
simulationFileName <- "individualPksimSim"
populationFileName <- "popData"
resultsFolderName <- "C:/Users/ahamadeh/Dropbox/rproject/workflow/res20200212"
resultsFileName <- "popSimRes"
numberOfCores <- 1

simFilePath <- file.path(inputFolderName, paste0(simulationFileName, ".pkml"))
popDataFilePath <- file.path(inputFolderName, paste0(populationFileName, ".csv"))
simResultsFilePath <- file.path(resultsFolderName, paste0(resultsFileName, ".csv"))
# dir.create(resultsFolderName)
# ospsuite.reportingengine::simulateModel(
#   simFilePath = simFilePath,
#   popDataFilePath = popDataFilePath,
#   resultsFilePath = simResultsFilePath)

pkParameterResultsFilePath <- "C:/Users/ahamadeh/Dropbox/rproject/workflow/res20200212/pkRes.csv"
# pkGeneratedResultFileNames <- calculatePKParameters(
#      simulationFilePath =  simFilePath,
#      simulationResultFilePaths = simResultsFilePath,
#      pkParameterResultsFilePath = pkParameterResultsFilePath)

sensResultsFileName <- "sensRes"
# sensGeneratedResultFileNames <- analyzeSensitivity(
#  simFilePath = simFilePath,
#  resultsFileFolder = resultsFolderName,
#  resultsFileName = sensResultsFileName,
#  numberOfCores = 1)


getPKResultsDataFrame <- function(pkParameterResultsFilePath) {
  pkResultsDataFrame <- read.csv(pkParameterResultsFilePath, encoding = "UTF-8", check.names = FALSE)
  colnames(pkResultsDataFrame) <- c("IndividualId", "QuantityPath", "Parameter", "Value", "Unit")
  # pkResultsDataFrame$IndividualId <- as.factor(pkResultsDataFrame$IndividualId)
  pkResultsDataFrame$QuantityPath <- as.factor(pkResultsDataFrame$QuantityPath)
  pkResultsDataFrame$Parameter <- as.factor(pkResultsDataFrame$Parameter)
  pkResultsDataFrame$Unit <- as.factor(pkResultsDataFrame$Unit)
  return(pkResultsDataFrame)
}

getQuantileIndividualIds <- function(dataframe, quantileVec = c(0.05, 0.5, 0.95)) {
  rowNums <- NULL

  for (n in 1:length(quantileVec)) {
    rowNums[n] <- which.min(abs(dataframe$Value - quantile(dataframe$Value, quantileVec[n])))
  }
  ids <- as.numeric(dataframe$IndividualId[rowNums])
  return(ids)
}

df <- getPKResultsDataFrame(pkParameterResultsFilePath)
outputs <- levels(df$QuantityPath)
pkParameters <- levels(df$Parameter)

dat <- df[ df["QuantityPath"] == outputs[1] & df["Parameter"] == pkParameters[1], ]

ids <- getQuantileIndividualIds(dat)
analyzeSensitivity(simFilePath,
  parametersToPerturb = NULL,
  popFilePath = popDataFilePath,
  individualID = ids,
  numberOfCores = 1,
  resultsFileFolder = resultsFolderName,
  resultsFileName = "sensitivityAnalysisResults"
)




# dat$IndividualId[c(85,38,66)]

# print(dat$IndividualId[ which.min(abs(dat$Value - quantile(dat$Value,quantileVec[1]))) ])
# print(dat$IndividualId[ which.min(abs(dat$Value - quantile(dat$Value,quantileVec[2]))) ])
# print(dat$IndividualId[ which.min(abs(dat$Value - quantile(dat$Value,quantileVec[3]))) ])

# ids <- c(dat$IndividualId[ which.min(abs(dat$Value - quantile(dat$Value,quantileVec[1]))) ],
# dat$IndividualId[ which.min(abs(dat$Value - quantile(dat$Value,quantileVec[2])))] ,
# dat$IndividualId[ which.min(abs(dat$Value - quantile(dat$Value,quantileVec[3])))] )

# print(ids)
# getIndividualIdsQuantile <-

# q50<-quantile(dat$Value,0.5)
# dat <- df[ df$QuantityPath == outputs[2]  ,]
# dat <- df[df[c("QuantityPath","Parameter")]==c(outputs[1],pkParameters[1]),]


# Report structure will be as following:
#   Population 1
#      Output 1
#         PK-Parameter 1
#         PK-Parameter 2
#                :
#         PK-Parameter N
#      Output 2
#         PK-Parameter 1
#         PK-Parameter 2
#                :
#         PK-Parameter N
