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
    #OPTIONAL Name of the user-defined function for calculation of "non-standard" PKParameters (s. #23)
    calculatedOutputsFolder = NULL, #a string, Subfolder for calculation outputs
    #Data display name for report
    #Time raster

    initialize = function(modelFilePath,
                          modelDisplayName = NULL,
                          calculatedOutputsFolder = "." #default output folder is current directory
    ) {
      self$modelFilePath <- modelFilePath
      self$modelDisplayName <- modelDisplayName
      self$calculatedOutputsFolder <- calculatedOutputsFolder
      private$loadedModel <- loadSimulation(modelFilePath)
      #Active task list: (ie tasks that are to be started when workflow begins)


    },

    #TASKS
    simulateMeanModel = function(){

      #Simulate mean model (details s. #31)
      #Remarks: Mass balance plots require all model outputs, thus it's not enough to save only model outputs of interest

      # IF TIME RASTER PROVIDED THEN USE IT, ELSE USE DEFAULT SIMULATION TIMES IN PKML
      private$simulationResults <- runSimulation(private$loadedModel)
      self$modelOutputsList <-  private$simulationResults$allQuantityPaths
      self$calculatedOutputs <- getOutputValuesTLF(simulationResults = private$simulationResults, quantitiesOrPaths = self$modelOutputsList , individualIds = 0 , population =  loadPopulation(csvPopulationFile = "./data/popData.csv"))  #using this placeholder population CSV file for now...need to make optional the population input to getOutputValuesTLF

      # print(paste0(self$calculatedOutputsFolder,"/",self$modelDisplayName,"-outputs.csv"))

       write.csv(self$calculatedOutputs$data,file = paste0(self$calculatedOutputsFolder,"/",self$modelDisplayName,"-outputs.csv"))

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
  ),

  private = list(
    loadedModel = NULL,
    simulationResults = NULL,
    sensitivityResults = NULL
  )

)
