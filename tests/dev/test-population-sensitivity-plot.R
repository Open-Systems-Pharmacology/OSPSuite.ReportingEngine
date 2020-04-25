rm(list = ls())
library(ospsuite)
#library(ospsuite.reportingengine)
devtools::load_all("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/")
tree <- getSimulationTree("./tests/dev/individualPksimSim.pkml")
ps <- PopulationSimulationSet$new(
  simulationSetName = "nonparpopsim",
  simulationFile = "./tests/dev/individualPksimSim.pkml",
  populationFile = "./tests/dev/popData_short.csv",
  pathID = tree$Organism$VenousBlood$Plasma$smarties$Concentration$path
)
pwf <- PopulationWorkflow$new(simulationSets = list(ps), workflowFolder = "./allParSA/")
pwf$simulatePopulation$settings$showProgress <- FALSE
pwf$simulatePopulation$activate()
pwf$populationPKParameters$activate()
pwf$populationSensitivityAnalysis$activate()
pwf$populationSensitivityAnalysis$settings$showProgress <- TRUE
pwf$populationSensitivityAnalysis$settings$variableParameterPaths <- tree$Organism$Heart$Volume$path
pwf$populationSensitivityAnalysis$settings$pkParameterSelection <- c("C_max", "CL")
pwf$populationSensitivityAnalysis$settings$quantileVec <- c(0.05, 0.25, 0.5, 0.75, 0.95)

pwf$simulationStructures[[1]]$popSensitivityAnalysisResultsIndexFile


# plotPopSensForPkAndOutput <- function(workflow,pkParameters,quantiles,rankFilter=NULL){
#   df <- read.csv(file = workflow$simulationStructures[[1]]$popSensitivityAnalysisResultsIndexFile)
#   validateIsIncluded(pkParameters,unique(df$pkParameters))
#   validateIsInteger(rankFilter,nullAllowed = TRUE)
#   validateIsPositive(rankFilter,nullAllowed = TRUE)
#   pltList <- list()
#   for (pk in pkParameters){
#     pkDf <- df[df$pkParameters == pk,]
#     print(pkDf)
#     validateIsIncluded(quantiles,unique(pkDf$Quantile))
#     indexDf <- pkDf[pkDf$Quantile %in% quantiles,]
#     individualsDfForPKParameter <- getSensitivityDataFrameForIndividuals(indexDf,pwf$simulationStructures[[1]],pk)
#     sortedFilteredIndividualsDfForPKParameter <- sortAndFilterIndividualsDF(individualsDfForPKParameter,rankFilter)
#     pltList[pk] <- getPkParameterPopulationSensitivityPlot(data = pkDf,
#                                                            pkParameter = pk,
#                                                            output = output,
#                                                            parameterColumnName = "Parameter",
#                                                            outputColumnName = "QuantityPath",
#                                                            sensitivityColumnName = "Value",
#                                                            colorColumnName = "Quantile",
#                                                            shapeColumnName = NULL)
#   }
#   return(sortedFilteredIndividualsDfForPKParameter)
# }





plotPopSensForPkAndOutput <- function(workflow,pkParameter,output,quantiles,rankFilter=NULL){
  indexDf <- read.csv(file = workflow$simulationStructures[[1]]$popSensitivityAnalysisResultsIndexFile)
  validateIsIncluded(pkParameter,unique(indexDf$pkParameters))
  validateIsIncluded(output,unique(indexDf$Outputs))
  validateIsInteger(rankFilter,nullAllowed = TRUE)
  validateIsPositive(rankFilter,nullAllowed = TRUE)
  pkOutputIndexDf <- getPkOutputIndexDf(indexDf,pkParameter,output)
  validateIsIncluded(quantiles,unique(pkOutputIndexDf$Quantile))
  quantilePkOutputIndexDf <- pkOutputIndexDf[pkOutputIndexDf$Quantile %in% quantiles,]
  print(quantilePkOutputIndexDf)
  individualsDfForPKParameter <- getSensitivityDataFrameForIndividuals(indexDf = quantilePkOutputIndexDf,
                                                                       structureSet = pwf$simulationStructures[[1]],
                                                                       pkParameter = pkParameter,
                                                                       output = output)
  sortedFilteredIndividualsDfForPKParameter <- sortAndFilterIndividualsDF(individualsDfForPKParameter,rankFilter)
  print(sortedFilteredIndividualsDfForPKParameter)
  #   plt <- getPkParameterPopulationSensitivityPlot(data = pkDf,
  #                                                  pkParameter = pkParameter,
  #                                                  output = output,
  #                                                  parameterColumnName = "Parameter",
  #                                                  outputColumnName = "QuantityPath",
  #                                                  sensitivityColumnName = "Value",
  #                                                  colorColumnName = "Quantile",
  #                                                  shapeColumnName = NULL)
  #
  #   return(plt)
}


getPkOutputIndexDf <- function(indexDf,pkParameter,output){
  pkOutputIndexDf <- indexDf[(indexDf$pkParameters == pkParameter) & (indexDf$Outputs == output),]
  return(pkOutputIndexDf)
}

#get the dataframe corresponding to individuals occupying specified percentiles in distributions of specified pk parameters
getSensitivityDataFrameForIndividuals <- function(indexDf,structureSet,pkParameter,output){
  dfList <- list()
  for (n in 1:nrow(indexDf)){
    individualId <- indexDf$IndividualId[n]
    quantile <- indexDf$Quantile[n]
    resultsFile <- file.path(structureSet$workflowFolder,structureSet$sensitivityAnalysisResultsFolder,indexDf$Filename[n])
    # sensitivityResultsForIndividual <- ospsuite::importSensitivityAnalysisResultsFromCSV(simulation = loadSimulationWithUpdatedPaths(structureSet$simulationSet),
    #                                                                                      filePaths = resultsFile)
    #individualsDf <- sensitivityResultsForIndividual$allPKParameterSensitivitiesFor(pkParameterName = pkParameter, outputPath = output )
    #print(individualsDf)
    individualsDf <- read.csv(resultsFile,check.names = FALSE,encoding = "UTF-8")
    colnames(individualsDf) <- c("QuantityPath", "Parameter", "PKParameter", "Value")
    individualsDf <- individualsDf[(individualsDf$PKParameter == pkParameter) & (individualsDf$QuantityPath == output),]
    dfList[[n]] <- cbind(individualsDf , data.frame("Quantile" = rep(quantile,nrow(individualsDf)) , "individualId" = rep(individualId,nrow(individualsDf))))
  }
  individualsDfForPKParameter <- do.call(rbind.data.frame,dfList)
  return(individualsDfForPKParameter)
}

sortAndFilterIndividualsDF <- function(individualsDfForPKParameter,rankFilter){
  validateIsPositive(nrow(individualsDfForPKParameter))
  sortedIndividualsDfForPKParameter <- individualsDfForPKParameter[order(-abs(individualsDfForPKParameter$Value) ),]
  if (is.null(rankFilter)){
    return(sortedIndividualsDfForPKParameter)
  }
  rankFilter <- min(rankFilter,nrow(sortedIndividualsDfForPKParameter))
  sortedFilteredIndividualsDfForPKParameter <- sortedIndividualsDfForPKParameter[1:rankFilter,]
  return(sortedFilteredIndividualsDfForPKParameter)
}


getPkParameterPopulationSensitivityPlot <- function(pkDf){

  p <- ggplot2::ggplot() + ggplot2::geom_point(data = pkDf, aes(x = sens,
                                                               y = param,
                                                               color = quant,
                                                               shape = pop))
  p <- p + geom_vline(xintercept = 0)
  return(plt)
}

idf <- plotPopSensForPkAndOutput(workflow = pwf,
            pkParameter = c("C_max"),
            output = pwf$simulationStructures[[1]]$simulationSet$pathID[1],
            quantiles = c(0.25,0.5),
            rankFilter = 10)
#print(idf)
