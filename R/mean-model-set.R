#' @title MeanModelSet
#' @description R6 class representing Reporting Engine Mean Model Set
#' @field simulationFile names of pkml file to be used for the simulation
#' @field simulationName display name of simulation
#' @field pathID path name for the simulation (e.g. `Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)`)
#' @field pathName display name for `pathID`
#' @field pathUnit display unit for `pathID`
#' @field pkParameters PK parameters function names to be calculated from the simulation (e.g. `C_max`).
#' @field pkParametersNames display names for `pkParameters`
#' @field pkParameterUnits display units for `pkParameters`
#' @field dataFilter filter to compare with observed data
#' @export
MeanModelSet <- R6::R6Class(
  "MeanModelSet",
  public = list(
    simulationFile = NULL, 
    simulationName = NULL,
    pathID = NULL,
    pathName = NULL,
    pathUnit = NULL,
    pkParameters = NULL,
    pkParameterUnits = NULL,
    dataFilter = NULL,
    
    #' @description
    #' Create a new `MeanModelSet` object.
    #' @param simulationFile names of pkml file to be used for the simulation
    #' @param simulationName display name of simulation
    #' @param pathID path name for the simulation (e.g. `Organism|PeripheralVenousBlood|Raltegravir|Plasma (Peripheral Venous Blood)`)
    #' @param pathName display name for `pathID`
    #' @param pathUnit display unit for `pathID`
    #' @param pkParameters PK parameters function names to be calculated from the simulation (e.g. `C_max`).
    #' Default value is enum `AllPKParameters`.
    #' @param pkParametersNames display names for `pkParameters`
    #' @param pkParameterUnits display units for `pkParameters`
    #' @param dataFilter filter to compare with observed data
    #' @return A new `MeanModelSet` object
    initialize = function(simulationFile, 
                          simulationName = NULL,
                          pathID,
                          pathName = NULL,
                          pathUnit = NULL,
                          pkParameters = AllPKParameters,
                          pkParametersNames = NULL,
                          pkParametersUnits = NULL,
                          dataFilter = NULL){
      
      self$simulationFile <-  simulationFile
      self$simulationName <- simulationName %||% trimFileName(simulationFile, extension = "pkml")
      
      self$pathID <- pathID
      self$pathName <- pathName %||% pathID
      self$pathUnit <- pathUnit
      
      self$pkParameters <- pkParameters
      self$pkParametersNames <- pkParametersNames %||% pkParameters
      self$pkParameterUnits <- pkParameterUnits
      
      self$dataFilter <- dataFilter
      
    }
  )
)
    