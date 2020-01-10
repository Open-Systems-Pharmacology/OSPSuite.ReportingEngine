#' @title splitPopDataFile
#' @description Function to split a population data file for parallel processing
#' @description Input is a CSV population data file
#' @description Output is vector of strings of the name of new CSV files
#' @export
splitPopDataFile <- function(fileName,folderName,numberOfSlaves,numberOfCommentLines){
  allPopData <- read.csv(file = paste0(folderName,fileName),
                         skip = numberOfCommentLines,
                         check.names = FALSE )
  numRows <- nrow(allPopData)
  numLinesPerSlave <- ceiling(numRows/numberOfSlaves)
  firstRow<- c(1,((1:(numberOfSlaves-1))*numLinesPerSlave)+1)
  lastRow <- c(((1:(numberOfSlaves-1))*numLinesPerSlave),numRows)
  newFileNamesVec <- NULL
  for (n in 1:numberOfSlaves){
    dF <- allPopData[firstRow[n]:lastRow[n],]
    newFileName <- paste0("tempPopFile",n,"_",fileName)
    newFileNamesVec <- c(newFileNamesVec,newFileName)
    write.csv( x = dF , file = paste0(folderName,newFileName) , row.names=FALSE  )
  }
  return(newFileNamesVec)
}


#' @title removeTempPopFiles
#' @description Function to remove temporary population data files previously created when splitting main population data file for parallelization.
#' @description Input is vector of strings containting names of temporary population data files, as output by function splitPopDataFile
#' @description No output.
#' @export
removeTempPopFiles <- function(folderName,fileNamesVec){
  for (n in 1:length(fileNamesVec)){
    file.remove(paste0(folderName,fileNamesVec[n]))
  }
}

# folderName <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/"
# fileName   <- "popData.csv"
# numberOfSlaves <- 4
#
# flz <- splitPopDataFile(fileName = fileName,
#                         folderName = folderName,
#                         numberOfSlaves = numberOfSlaves,
#                         numberOfCommentLines = 2)
#
# removeTempPopFiles(folderName = folderName, fileNamesVec= flz)

