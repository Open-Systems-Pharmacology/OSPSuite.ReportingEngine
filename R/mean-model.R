#' @title MeanModel
#' @docType class
#' @description  MeanModel R6 class
#' @export
MeanModel <- R6::R6Class(
  "MeanModel",
  public = list(
    modelFilePath = NULL, #a path to a pkml file that will be loaded using loadSimulation()
    simModel = NULL, #a model previously loaded using loadSimulation, an alternative to providing modelFilePath
    modelDisplayName = NULL, #a string, to be set to name of model file if not supplied
    simulationOutputs = NULL,
    massBalance = NULL,
    PKAnalysis = NULL,
    additionalPKAnalysis = NULL,
    #OPTIONAL Name of the user-defined function for calculation of "non-standard" PKParameters (s. #23)
    simulationOutputsFolder = NULL, #a string, Subfolder for calculation outputs
    #Data display name for report
    #Time raster


    simulationResults = NULL,
    sensitivityResults = NULL,


    initialize = function(modelFilePath = NULL,
                          simModel = NULL,
                          modelDisplayName = NULL,
                          simulationOutputsFolder = "." #default output folder is current directory
    ) {

      if (!is.null(modelFilePath)){
        self$modelFilePath <- modelFilePath
      } else{

        if (!is.null(simModel)){
          self$modelFilePath <- simModel$sourceFile
        } else{
          stop("Either modelFilePath or simModel need to be provided.")
        }

      }

      self$modelDisplayName <- modelDisplayName
      self$simulationOutputsFolder <- simulationOutputsFolder



      #Active task list: (ie tasks that are to be started when workflow begins)


    },

    #TASKS
    simulateMeanModel = function(saveSimulation = FALSE) {

      loadedModel <- loadSimulation(self$modelFilePath, loadFromCache = FALSE)
      # if (calculateMassBalance) {  #If statement to add all outputs to loadedModel's output list if a mass balance is required
      #   compoundList <- getCompoundsList(loadedModel) #Get a vector of all compound names
      #   pathsPerCompoundList <-list()
      #   for (cmp in compoundList){ #For loop to get the list of paths for each compound
      #     pathsPerCompoundList[[cmp]]  <- getPathsForMoleculeAmount(simulation = loadedModel,cmp)
      #     addOutputs(quantitiesOrPaths = pathsPerCompoundList[[cmp]], simulation = loadedModel) #add outputs for current compound to loadedModel's output list
      #   }
      # }

      #Simulate mean model (details s. #31)
      #Remarks: Mass balance plots require all model outputs, thus it's not enough to save only model outputs of interest

      # IF TIME RASTER PROVIDED THEN USE IT, ELSE USE DEFAULT SIMULATION TIMES IN PKML
      self$simulationResults <- runSimulation(loadedModel)
      self$simulationOutputs <- getOutputValuesTLF(simulationResults = self$simulationResults, quantitiesOrPaths = self$modelOutputsList , individualIds = 0 , population =  loadPopulation(csvPopulationFile = "./data/popData.csv"))  #using this placeholder population CSV file for now...need to make optional the population input to getOutputValuesTLF

      # if (calculateMassBalance) { #If statement to calculate mass balance for each compound if required
      #   self$massBalance <- list()
      #
      #   for ( cmp in  compoundList ){ #For loop to calculate mass balance for each compound
      #     self$massBalance[[cmp]] <- rep(0,nrow(self$simulationOutputs$data)) #Set up vector of zeros
      #     pthsVec <- envList2PathStringsVector(pathsPerCompoundList[[cmp]]) #Get string vector of all outputs for the current compound
      #     for (t in seq(1:nrow(self$simulationOutputs$data))){ #For each timepoint t
      #       self$massBalance[[cmp]][t] <- sum(self$simulationOutputs$data[t,pthsVec]) # Select the columns of the dataframe self$simulationOutputs$data corresponding to the current compound and sum together the amounts of the compound in each path given in pthsVec for the timepoint t
      #     }
      #   }
      #   self$massBalance <- as.data.frame(self$massBalance) #Convert massBalance list to dataframe
      # }


      # print(paste0(self$simulationOutputsFolder,"/",self$modelDisplayName,"-outputs.csv"))

      if (saveSimulation){
        write.csv(self$simulationOutputs$data,file = paste0(self$simulationOutputsFolder,"/",self$modelDisplayName,"-outputs.csv"))

        # if (calculateMassBalance){
        #   write.csv(self$massBalance,file = paste0(self$simulationOutputsFolder,"/",self$modelDisplayName,"-mass-balance.csv"))
        # }

      }

    },

    calculateMassBalance = function(saveMassBalance = FALSE) {

      loadedModel <- loadSimulation(self$modelFilePath, loadFromCache = FALSE)
      #Add all outputs to loadedModel's output list
      compoundList <- getCompoundsList(loadedModel) #Get a vector of all compound names
      pathsPerCompoundList <-list()
      for (cmp in compoundList){ #For loop to get the list of paths for each compound
        pathsPerCompoundList[[cmp]]  <- getPathsForMoleculeAmount(simulation = loadedModel,cmp)
        addOutputs(quantitiesOrPaths = pathsPerCompoundList[[cmp]], simulation = loadedModel) #add outputs for current compound to loadedModel's output list
      }

      simulationResults <- runSimulation(loadedModel)
      simulationOutputs <- getOutputValuesTLF(simulationResults = simulationResults,
                                              quantitiesOrPaths = simulationResults$allQuantityPaths,
                                              individualIds = 0,
                                              population =  loadPopulation(csvPopulationFile = "./data/popData.csv")
      )  #using this placeholder population CSV file for now...need to make optional the population input to getOutputValuesTLF

      self$massBalance <- list()

      for ( cmp in compoundList ){ #For loop to calculate mass balance for each compound
        self$massBalance[[cmp]] <- rep(0,nrow(simulationOutputs$data)) #Set up vector of zeros that is the same length as the number of simulation timepoints
        pthsVec <- envList2PathStringsVector(pathsPerCompoundList[[cmp]]) #Get string vector of all outputs for the current compound
        for (t in seq(1:nrow(simulationOutputs$data))){ #For each timepoint t
          self$massBalance[[cmp]][t] <- sum(simulationOutputs$data[t,pthsVec]) # Select the columns of the dataframe self$simulationOutputs$data corresponding to the current compound and sum together the amounts of the compound in each path given in pthsVec for the timepoint t
        }
      }
      self$massBalance <- as.data.frame(self$massBalance) #Convert massBalance list to datafram

      # print(paste0(self$simulationOutputsFolder,"/",self$modelDisplayName,"-outputs.csv"))
      if (saveMassBalance){
        write.csv(self$massBalance,file = paste0(self$simulationOutputsFolder,"/",self$modelDisplayName,"-mass-balance.csv"))
      }



    },


    calculateMeanModelPKParameters = function(userDefinedPKFunctions = NULL , savePKAnalysis = FALSE){

      #userDefinedPKFunctions is an array of UserDefinedPKFunction objects

      # Calculate MeanModel PK Parameters (details s. #32 )
      # Sometimes it's required to calculate "non-standard" PK-Parameter which are not part of the basis
      # toolbox. In such a case user MUST provide a function for calculation.

      self$PKAnalysis <- calculatePKAnalyses(self$simulationResults)
      tempFileName <- paste0("./tests/tempPKresults_",as.numeric(Sys.time()),".csv")
      exportPKAnalysesToCSV(pkAnalyses = self$PKAnalysis , filePath = tempFileName)
      PKResultsDF <- read.csv(file = tempFileName, check.names = FALSE)
      file.remove(tempFileName)
      #print(PKResultsDF)
      #print(PKResultsDF[["IndividualId"]])
      #print(colnames(PKResultsDF))


      userDefinedPKResults = list()
      userDefinedPKResults[["IndividualId"]]<-NULL
      userDefinedPKResults[["Quantity Path"]]<-NULL
      userDefinedPKResults[["Parameter"]]<-NULL
      userDefinedPKResults[["Value"]]<-NULL
      userDefinedPKResults[["Unit"]]<-NULL


      for (pth in self$simulationResults$allQuantityPaths){
        #print(pth)
        pathResults<-getOutputValues(simulationResults = self$simulationResults,
                                     quantitiesOrPaths = pth,
                                     individualIds = 0)

        x <- pathResults[[pth]]$x
        y <- as.vector( pathResults[[pth]]$y )

        for (ud in userDefinedPKFunctions){
          userDefinedPKResults[["IndividualId"]]  <-c(      userDefinedPKResults[["IndividualId"]]  , 0 )
          userDefinedPKResults[["Quantity Path"]] <-c(      userDefinedPKResults[["Quantity Path"]] , pth )
          userDefinedPKResults[["Parameter"]]     <-c(      userDefinedPKResults[["Parameter"]]     , ud$pKParameterName )
          userDefinedPKResults[["Value"]]         <-c(      userDefinedPKResults[["Value"]]         , ud$pKFunction(x,y) )
          userDefinedPKResults[["Unit"]]          <-c(      userDefinedPKResults[["Unit"]]          , ud$pKParameterUnit )
        }

      }


      userDefinedPKResultsDataFrame <- as.data.frame(userDefinedPKResults)
      print(userDefinedPKResultsDataFrame)

      colnames(PKResultsDF) <- colnames(userDefinedPKResultsDataFrame)
      self$PKAnalysis <- rbind(PKResultsDF,userDefinedPKResultsDataFrame)

      if (savePKAnalysis){
        write.csv(self$PKAnalysis,file = paste0(self$simulationOutputsFolder,"/",self$modelDisplayName,"-PK-analysis.csv"))
      }

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

      loadedModel <- loadSimulation(filePath = self$modelFilePath, loadFromCache = FALSE)
      sensitivityAnalysisResults <- runSensitivityAnalysis(SensitivityAnalysis$new(simulation = loadedModel),
                                                           SensitivityAnalysisRunOptions$new(showProgress = TRUE))

      #perturb parameters, save selected outputs (as functions of time)
      #for each perturbation calculate pk parameters

      ### SensitivityAnalysisRunOptions has two parameters
      ### numberOfCoresToUse and showProgress.
      ### These could be inputs to the function
      ### runSensitivityAnalysis to simplify.


      #next calculate pk parameter for the perturbed output vectors (which are functions of time)
      for (output in loadedModel$outputSelections$allOutputs) {
        pkSensitivities <- sensitivityAnalysisResults$allPKParameterSensitivitiesFor(pkParameterName = "AUC", outputPath = output$path)
        for (pkSensitivity in pkSensitivities) {
          print(pkSensitivity)
        }
      }

    },

    plotMeanModelSensitivity = function(){
      # Plot MeanModel Sensitivity (details s. #38 )
    }
  )



)
