#' @title readObservedDataFile
#' @description
#' Read observed data file with Nonmem format.
#' Can read csv files as well as
#' @param fileName name of file to be read
#' @param header logical indicating if data has a header
#' @param encoding encoding of the file
#' @return data.frame containing observed data
#' @export
readObservedDataFile <- function(fileName,
                                 header = TRUE,
                                 encoding = "UTF-8") {
  extension <- fileExtension(fileName)
  # For some cases where data was derived from Excel,
  # <U+FEFF> is included in first variable name and needs to be removed
  forbiddenCharacters <- "\ufeff" 

  if (extension %in% "csv") {
    observedData <- read.csv(fileName,
      header = header,
      check.names = FALSE,
      encoding = encoding
    )
    variableNames <- names(observedData)
    variableNames[1] <- gsub(forbiddenCharacters, "", variableNames[1])
    names(observedData) <- variableNames
    return(observedData)
  }

  observedData <- read.table(fileName,
    header = header,
    check.names = FALSE,
    encoding = encoding
  )
  variableNames <- names(observedData)
  variableNames[1] <- gsub(forbiddenCharacters, "", variableNames[1])
  names(observedData) <- variableNames
  return(observedData)
}

#' @title evalDataFilter
#' @description
#' Evaluate a data filter by converting the variable names of the data.frame
#' into names of variables to be evaluated in the filter expression.
#' @param data data.frame containing observed data
#' @param filterExpression expression
#' character string filter to be applied
#' @return vector of logicals corresponding to the evaluation of the filter
#' @export
evalDataFilter <- function(data, filterExpression) {
  variableNames <- names(data)
  expressionList <- lapply(
    variableNames,
    function(variableName) {
      parse(text = paste0(variableName, '<- data[,"', variableName, '"]'))
    }
  )

  for (dataExpression in expressionList) {
    eval(dataExpression)
  }

  return(eval(filterExpression))
}


dictionaryParameters <- list(
  ID = "ID",
  nonmenColumn = "nonmenColumn",
  timeID = "time",
  dvID = "dv",
  lloqID = "lloq"
)

getDictionaryVariable <- function(dictionary, variableID) {
  variableMapping <- dictionary[, dictionaryParameters$ID] %in% variableID
  variableName <- as.character(dictionary[variableMapping, dictionaryParameters$nonmenColumn])
  if (isOfLength(variableName, 0)) {
    return()
  }
  return(variableName)
}
