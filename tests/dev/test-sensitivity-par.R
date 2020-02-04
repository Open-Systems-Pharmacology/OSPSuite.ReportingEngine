rm(list = ls())
library(ospsuite)
library(ospsuite.reportingengine)
library(Rmpi)
library(tictoc)


# function(populationFilePath,IndividualID,simulationFilePath){
#
#   individualParameters <- popObject$getParameterValuesForIndividual(individualId = 2)
#
#
# }



# INPUTS ARE FOLDER OR PATH TO SIMULATION, LIST OF PARAMETERS (OPTIONAL,DEFAULT NULL) AND NUMBER OF CORES (OPTIONAL, DEFAULT 1), LIST OF OUTPUTS (OPTIONAL??), LIST OF PK PARAMS (OPTIONAL DEFAULT NULL)
# simFilePath <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/simpleMobiEventSim_nonzeroinitial.pkml"
simFilePath <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/individualPksimSim.pkml"
popFilePath <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/popData.csv" # DEFAULT NULL
popObject <- loadPopulation(popFilePath)
individualParameters <- popObject$getParameterValuesForIndividual(individualId = 2) # DEFAULT NULL

numberOfCores <- 2


resultsFileFolder <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/tests/dev/"
resultsFileName <- "SAResults"

parametersToPerturb <- NULL



analyzeSensitivity <- function(simFilePath,
                               parametersToPerturb = NULL,
                               popFilePath=NULL,
                               individualID=NULL,
                               numberOfCores=NULL,
                               resultsFileFolder="getwd()",
                               resultsFileName="sensitivityAnalysisResults"){


  #If there is a population file and individualID then for each individual perform SA
  #If there is a population file and no individualID then do SA for entire population
  #If there is no population file and individualID then do SA for mean model
  #If there is no population file and no individualID then do SA for mean model.



  #Determine if SA is to be done for a single individual or more
  if (!is.null(popFilePath)){
    popObject <- loadPopulation(popFilePath)
    individualSeq <- individualID %||% seq(1,popObject$count)
    for (ind in individualSeq){
      individualParameters <-
        individualSensitivityAnalysis(simFilePath = simFilePath,
                                      parametersToPerturb = parametersToPerturb,
                                      individualParameters = popObject$getParameterValuesForIndividual(individualId = ind),
                                      numberOfCores = numberOfCores,
                                      resultsFileFolder = resultsFileFolder,
                                      resultsFileName = resultsFileName
        )
    }
  }
  else {
    individualSensitivityAnalysis(simFilePath = simFilePath,
                                  parametersToPerturb = parametersToPerturb,
                                  individualParameters = NULL,
                                  numberOfCores = numberOfCores,
                                  resultsFileFolder = resultsFileFolder,
                                  resultsFileName = resultsFileName)
  }
}



individualSensitivityAnalysis <- function(simFilePath,
                                          parametersToPerturb = NULL,
                                          individualParameters,
                                          numberOfCores = 1,
                                          resultsFileFolder = resultsFileFolder,
                                          resultsFileName = resultsFileName){
  #Load simulation to determine number of perturbation parameters
  sim <- loadSimulation(simFilePath)

  #If no perturbation parameters specified, perturb all parameters
  if (is.null(parametersToPerturb)) {
    parametersToPerturb <- ospsuite::potentialVariableParameterPathsFor(simulation = sim)
  }
  totalNumberParameters <- length(parametersToPerturb)

  #In case there are more cores specified in numberOfCores than there are parameters, ensure at least one parameter per spawned core
  numberOfCores <- min(numberOfCores, totalNumberParameters)
  if (totalNumberParameters == 0) {
    stop("No variable parameters found for sensitivity analysis.")
  }

  #Determine if SA is to be done on a single core or more
  if (numberOfCores > 1) {


    runParallelSensitivityAnalysis(simFilePath,
                                   parametersToPerturb,
                                   individualParameters,
                                   numberOfCores,
                                   resultsFileFolder,
                                   resultsFileName)







    # combinedSensitivityAnalysisResults <- ospsuite::importSensitivityAnalysisResultsFromCSV(simulation = sim, filePaths = allResultsFileNames)
    # ospsuite::exportSensitivityAnalysisResultsToCSV(
    #   results = combinedSensitivityAnalysisResults,
    #   filePath = paste0(resultsFileFolder, resultsFileName, ".csv")
    # )
  } else {
    # No parallelization
    updateSimulationIndividualParameters(simulation = sim, individualParameters)
    ospsuite.reportingengine::analyzeCoreSensitivity(
      simulation = sim,
      perturbationParameterNamesVector = parametersToPerturb,
      totalSensitivityThreshold = 1,
      resultsFilePath = paste0(resultsFileFolder, resultsFileName, ".csv")
    )
  }



}


# sort(1+(1:length(dd) %% (length(dd)+10))

# print("...done")


# Do we need this?
# numParametersPerCore <- ceiling(totalNumberParameters / numberOfCores) #Do we need this?


# For a sensitivity analysis, need to specify:
# - parameters to perturb (eg clearance rate)
# - outputs of interest (eg interstitial liver concentration of drug)
# - pk parameter functions to be applied to outputs of interest

# The output will be a list of objects that have at least the following fields:
# PKParameterSensitivity:
#   Parameter path: Organism-blockA-mol1init
# PK-Parameter: AUC
# Output path: Organism|blockB|mol1|twice_mol1_blockB
# Value: 1.00001383813318

# For parallelization, need to split the list of parameters to perturb among the N cores.
# Can we use the split function?

# Finally, need to export the results

# x = c("paul","john","george","ringo","freddie","mick")
# split(x, 1:length(x) %% 2 )
