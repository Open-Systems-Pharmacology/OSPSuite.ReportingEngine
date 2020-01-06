#' @title UserDefinedPKFunction
#' @docType class
#' @description  UserDefinedPKFunction R6 class
#' @export
UserDefinedPKFunction <- R6::R6Class(
  "UserDefinedPKFunction",
  public = list(

    pKParameterName = NULL,
    pKFunction = NULL,
    pKParameterUnit = NULL,

    initialize=function(pKParameterName, pKFunction, pKParameterUnit=NULL){
      self$pKParameterName = pKParameterName
      self$pKFunction = pKFunction
      self$pKParameterUnit = pKParameterUnit
    }
  )
)
