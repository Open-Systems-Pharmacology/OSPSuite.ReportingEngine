#' @title MeanModel
#' @docType class
#' @description  MeanModel R6 class
#' @export
MeanModel <- R6::R6Class(
  "MeanModel",
  public = list(
    modelFilePath = NULL, #a path to a pkml file that will be loaded using loadSimulation()
    modelDisplayName = NULL, #a string, to be set to name of model file if not supplied
    modelOutputsList = NULL, #to be a vector of model path strings
    calculatedOutputs = NULL,
    massBalance = NULL,
    #OPTIONAL Name of the user-defined function for calculation of "non-standard" PKParameters (s. #23)
    calculatedOutputsFolder = NULL, #a string, Subfolder for calculation outputs
    #Data display name for report
    #Time raster


    loadedModel = NULL,
    simulationResults = NULL,
    sensitivityResults = NULL,


    initialize = function(modelFilePath,
                          modelDisplayName = NULL,
                          calculatedOutputsFolder = "." #default output folder is current directory
    ) {
      self$modelFilePath <- modelFilePath
      self$modelDisplayName <- modelDisplayName
      self$calculatedOutputsFolder <- calculatedOutputsFolder
      self$loadedModel <- loadSimulation(modelFilePath, loadFromCache = FALSE)
      #Active task list: (ie tasks that are to be started when workflow begins)


    },

    #TASKS
    simulateMeanModel = function(saveSimulation = FALSE,calculateMassBalance = FALSE) {


      if (calculateMassBalance) {  #If statement to add all outputs to loadedModel's output list if a mass balance is required
        compoundList <- getCompoundsList(self$loadedModel) #Get a vector of all compound names
        pathsPerCompoundList <-list()
        for (cmp in compoundList){ #For loop to get the list of paths for each compound
          pathsPerCompoundList[[cmp]]  <- getPathsForMoleculeAmount(simulation = self$loadedModel,cmp)
          addOutputs(quantitiesOrPaths = pathsPerCompoundList[[cmp]], simulation = self$loadedModel) #add outputs for current compound to loadedModel's output list
        }
      }

      #Simulate mean model (details s. #31)
      #Remarks: Mass balance plots require all model outputs, thus it's not enough to save only model outputs of interest

      # IF TIME RASTER PROVIDED THEN USE IT, ELSE USE DEFAULT SIMULATION TIMES IN PKML
      self$simulationResults <- runSimulation(self$loadedModel)
      self$modelOutputsList <-  self$simulationResults$allQuantityPaths
      self$calculatedOutputs <- getOutputValuesTLF(simulationResults = self$simulationResults, quantitiesOrPaths = self$modelOutputsList , individualIds = 0 , population =  loadPopulation(csvPopulationFile = "./data/popData.csv"))  #using this placeholder population CSV file for now...need to make optional the population input to getOutputValuesTLF

      if (calculateMassBalance) { #If statement to calculate mass balance for each compound if required
        self$massBalance <- list()

        for ( cmp in  compoundList ){ #For loop to calculate mass balance for each compound
          self$massBalance[[cmp]] <- rep(0,nrow(self$calculatedOutputs$data)) #Set up vector of zeros
          pthsVec <- envList2PathStringsVector(pathsPerCompoundList[[cmp]]) #Get string vector of all outputs for the current compound
          for (t in seq(1:nrow(self$calculatedOutputs$data))){ #For each timepoint t
            self$massBalance[[cmp]][t] <- sum(self$calculatedOutputs$data[t,pthsVec]) # Select the columns of the dataframe self$calculatedOutputs$data corresponding to the current compound and sum together the amounts of the compound in each path given in pthsVec for the timepoint t
          }
        }
        self$massBalance <- as.data.frame(self$massBalance) #Convert massBalance list to dataframe
      }


      # print(paste0(self$calculatedOutputsFolder,"/",self$modelDisplayName,"-outputs.csv"))

      if (saveSimulation){
        write.csv(self$calculatedOutputs$data,file = paste0(self$calculatedOutputsFolder,"/",self$modelDisplayName,"-outputs.csv"))

        if (calculateMassBalance){
          write.csv(self$massBalance,file = paste0(self$calculatedOutputsFolder,"/",self$modelDisplayName,"-mass-balance.csv"))
        }

      }

    },


    calculateMeanModelPkParameters = function(){
      # Calculate MeanModel PK Parameters (details s. #32 )
      # Sometimes it's required to calculate "non-standard" PK-Parameter which are not part of the basis
      # toolbox. In such a case user MUST provide a function for calculation.
    },

    plotMeanModelTimeProfiles = function(){
      # MeanModel Plots/Tables: Time Profiles (details s. #33)
    },

    plotMeanModelResiduals = function(){
      # MeanModel Plots/Tables: Residuals (details s. #33)
    },

    plotMeanModelMassBalance = function(){
      # MeanModel Plots/Tables: Mass Balance (details s. #34)
    },

    plotMeanModelAbsorption = function(){
      # MeanModel Plots/Tables: Absorption (details s. #35 )
    },

    plotMeanModelPkParameters = function(){
      # MeanModel Plots/Tables: PK Parameters (details s. #36 )
    },

    calculateMeanModelSensitivity = function(){
      # Calculate MeanModel Sensitivity (details s. #37 )
    },

    plotMeanModelSensitivity = function(){
      # Plot MeanModel Sensitivity (details s. #38 )
    }
  )



)
