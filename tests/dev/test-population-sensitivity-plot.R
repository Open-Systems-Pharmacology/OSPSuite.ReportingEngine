rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)
devtools::load_all(".")
load("./tests/dev/ex_03_pop/simTrees.Rdata")

simulationFile = "./tests/dev/ex_03_pop/RaltegravirSim.pkml"
populationFile = "./tests/dev/ex_03_pop/RalPop10.csv"

ps <- PopulationSimulationSet$new(
  simulationSetName = "ral",
  simulationFile = simulationFile,
  populationFile = populationFile,
  pathID = simTree1$Organism$VenousBlood$Plasma$Raltegravir$Concentration$path
)
pwf <- PopulationWorkflow$new(simulationSets = list(ps), workflowFolder = "./tests/dev/ex_03_pop")
pwf$simulatePopulation$settings$showProgress <- FALSE
pwf$simulatePopulation$activate()
pwf$populationPKParameters$activate()
pwf$populationSensitivityAnalysis$activate()
pwf$populationSensitivityAnalysis$settings$showProgress <- TRUE
#pwf$populationSensitivityAnalysis$settings$variableParameterPaths <- simTree1$Organism$Heart$Volume$path
pwf$populationSensitivityAnalysis$settings$pkParameterSelection <- c("C_max", "CL")
pwf$populationSensitivityAnalysis$settings$quantileVec <- c(0.05, 0.25, 0.5, 0.75, 0.95)

pwf$simulationStructures[[1]]$popSensitivityAnalysisResultsIndexFile
pwf$runWorkflow()

#
#
# # plotPopSensForPkAndOutput <- function(workflow,pkParameters,quantiles,rankFilter=NULL){
# #   df <- read.csv(file = workflow$simulationStructures[[1]]$popSensitivityAnalysisResultsIndexFile)
# #   validateIsIncluded(pkParameters,unique(df$pkParameters))
# #   validateIsInteger(rankFilter,nullAllowed = TRUE)
# #   validateIsPositive(rankFilter,nullAllowed = TRUE)
# #   pltList <- list()
# #   for (pk in pkParameters){
# #     pkDf <- df[df$pkParameters == pk,]
# #     print(pkDf)
# #     validateIsIncluded(quantiles,unique(pkDf$Quantile))
# #     indexDf <- pkDf[pkDf$Quantile %in% quantiles,]
# #     individualsDfForPKParameter <- getSensitivityDataFrameForIndividuals(indexDf,pwf$simulationStructures[[1]],pk)
# #     sortedFilteredIndividualsDfForPKParameter <- sortAndFilterIndividualsDF(individualsDfForPKParameter,rankFilter)
# #     pltList[pk] <- getPkParameterPopulationSensitivityPlot(data = pkDf,
# #                                                            pkParameter = pk,
# #                                                            output = output,
# #                                                            parameterColumnName = "Parameter",
# #                                                            outputColumnName = "QuantityPath",
# #                                                            sensitivityColumnName = "Value",
# #                                                            colorColumnName = "Quantile",
# #                                                            shapeColumnName = NULL)
# #   }
# #   return(sortedFilteredIndividualsDfForPKParameter)
# # }
#
#
#
#
#
# getPopSensDfForPkAndOutput <- function(workflow,pkParameter,output,quantiles,rankFilter=NULL){
#   indexDf <- read.csv(file = workflow$simulationStructures[[1]]$popSensitivityAnalysisResultsIndexFile)
#   validateIsIncluded(pkParameter,unique(indexDf$pkParameters))
#   validateIsIncluded(output,unique(indexDf$Outputs))
#   validateIsInteger(rankFilter,nullAllowed = TRUE)
#   validateIsPositive(rankFilter,nullAllowed = TRUE)
#   pkOutputIndexDf <- getPkOutputIndexDf(indexDf,pkParameter,output)
#   validateIsIncluded(quantiles,unique(pkOutputIndexDf$Quantile))
#   quantilePkOutputIndexDf <- pkOutputIndexDf[pkOutputIndexDf$Quantile %in% quantiles,]
#   individualsDfForPKParameter <- getSensitivityDataFrameForIndividuals(indexDf = quantilePkOutputIndexDf,
#                                                                        structureSet = pwf$simulationStructures[[1]],
#                                                                        pkParameter = pkParameter,
#                                                                        output = output)
#   sortedFilteredIndividualsDfForPKParameter <- sortAndFilterIndividualsDF(individualsDfForPKParameter,rankFilter)
#   return(sortedFilteredIndividualsDfForPKParameter)
# }
#
#
# getPkOutputIndexDf <- function(indexDf,pkParameter,output){
#   pkOutputIndexDf <- indexDf[(indexDf$pkParameters == pkParameter) & (indexDf$Outputs == output),]
#   return(pkOutputIndexDf)
# }
#
# #get the dataframe corresponding to individuals occupying specified percentiles in distributions of specified pk parameters
# getSensitivityDataFrameForIndividuals <- function(indexDf,structureSet,pkParameter,output){
#   dfList <- list()
#   for (n in 1:nrow(indexDf)){
#     individualId <- indexDf$IndividualId[n]
#     quantile <- indexDf$Quantile[n]
#     resultsFile <- file.path(structureSet$workflowFolder,structureSet$sensitivityAnalysisResultsFolder,indexDf$Filename[n])
#     individualsDf <- read.csv(resultsFile,check.names = FALSE,encoding = "UTF-8")
#     colnames(individualsDf) <- c("QuantityPath", "Parameter", "PKParameter", "Value")
#     individualsDf <- individualsDf[(individualsDf$PKParameter == pkParameter) & (individualsDf$QuantityPath == output),]
#     dfList[[n]] <- cbind(individualsDf , data.frame("Quantile" = rep(quantile,nrow(individualsDf)) , "individualId" = rep(individualId,nrow(individualsDf))))
#   }
#   individualsDfForPKParameter <- do.call(rbind.data.frame,dfList)
#   return(individualsDfForPKParameter)
# }
#
# sortAndFilterIndividualsDF <- function(individualsDfForPKParameter,rankFilter){
#   validateIsPositive(nrow(individualsDfForPKParameter))
#   sortedIndividualsDfForPKParameter <- individualsDfForPKParameter[order(-abs(individualsDfForPKParameter$Value) ),]
#   if (is.null(rankFilter)){
#     return(sortedIndividualsDfForPKParameter)
#   }
#   rankFilter <- min(rankFilter,length(unique(sortedIndividualsDfForPKParameter$Parameter)))
#
#   #rank Parameter column according to sorted order
#   sortedIndividualsDfForPKParameter$Parameter <- factor(x = sortedIndividualsDfForPKParameter$Parameter,levels = unique(sortedIndividualsDfForPKParameter$Parameter))
#
#   #Filter out all except top 'rankFilter' most sensitive parameters
#   sortedFilteredIndividualsDfForPKParameter <- sortedIndividualsDfForPKParameter[ as.numeric(sortedIndividualsDfForPKParameter$Parameter) %in%  1:rankFilter  ,]
#
#   #Reverse level order of Parameter column to make most sensitive parameter have highest factor level
#   sortedFilteredIndividualsDfForPKParameter$Parameter <- factor(x = sortedFilteredIndividualsDfForPKParameter$Parameter,levels = rev(unique(sortedFilteredIndividualsDfForPKParameter$Parameter)) )
#
#   return(sortedFilteredIndividualsDfForPKParameter)
# }
#
#
#
# getPkParameterPopulationSensitivityPlot <- function(data,title){
#   data[["Quantile"]] <- as.factor(data[["Quantile"]])
#   plt <- ggplot2::ggplot() + ggplot2::geom_point(data = data,
#                                                  mapping = aes_string(x = "Value", y = "Parameter", color = "Quantile", shape = NULL),
#                                                  size = 3
#                                                  ) +
#     ggplot2::ylab("Parameter") + ggplot2::xlab("Sensitivity") + ggplot2::labs(color = "Individual quantile",
#                                                                               title = title)
#
#   plt <- plt + geom_vline(xintercept = 0)
#   return(plt)
# }

structureSet = pwf$simulationStructures[[1]]
pkParameter = c("C_max")
output = pwf$simulationStructures[[1]]$simulationSet$pathID[1]
quantiles = c(0.25,0.5)
rankFilter = 10
title <- paste("Population sensitivity of",pkParameter,"of",output)

sortedFilteredIndividualsDfForPKParameter <- getPopSensDfForPkAndOutput(structureSet = structureSet,
                                                                        pkParameter = pkParameter,
                                                                        output = output,
                                                                        quantiles = quantiles,
                                                                        rankFilter = rankFilter)


plt <- getPkParameterPopulationSensitivityPlot(data = sortedFilteredIndividualsDfForPKParameter,title = title)




show(plt)
