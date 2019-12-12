#' @export
getAllPathStrings <- function(inputEnv){
  #Function that returns a vector of strings.  Each element of the returned vector is a path of a Quantity in the simulation inputEnv
  strs=NULL
  currentEnv <- getAllQuantitiesMatching(paths = "**",container = inputEnv)
  for (env in currentEnv){
    strs <- c(env$path,strs)
  }
  return(strs)
}

#' @export
addBranch <- function(originalString,stringToGo){
  #Function to create a multilayered list called endList with a branched structure corresponding to the structure of stringToGo that terminates with a string called 'path' that is equal to the string originalString
  if (length(stringToGo) == 0){
    #If stringToGo is empty, create a terminal list with a string called 'path' and value equal to originalString
    endList <- list()
    endList$path <- toPathString(originalString)
    return(endList)
  }
  else{
    #If stringToGo is still not empty, remove its leading element and create a sub-branch list corresponding to the structure of the remaining elements of stringToGo
    newBranch <-list()
    newBranch[[stringToGo[1]]] <- addBranch(originalString,tail(stringToGo,-1))
    return(newBranch)
  }
}

#' @export
nextStep <- function(listSoFar,originalString,stringToGo){
  #Recursive function that adds a multilayer list to listSoFar that has a branched structure representing the vector of strings stringToGo.
  if (length(stringToGo) == 0){ #If end of string vector stringToGo has been reached, create a vector called 'path' and give it the value 'originalString'.
    listSoFar$path <- toPathString(originalString)
  }
  else{ #End of branch has not been reached.
    if (is.null(listSoFar[[stringToGo[1]]])){ #If this portion of the string vector stringToGo has not been added to listToGo yet, add it using the function addBranch
      listSoFar[[stringToGo[1]]] <- addBranch(originalString,tail(stringToGo,-1))
    }
    else{ #If this portion of the string vector stringToGo has already been added to listSoFar, remove the leading element of stringToGo and recursively apply this function using the remaining elements of stringToGo.
      listSoFar[[stringToGo[1]]] <- nextStep(listSoFar[[stringToGo[1]]],originalString,tail(stringToGo,-1))
    }
  }
  return(listSoFar)
}

#' @export
getEnum <- function(simulationFilePath){
#Input: the path of the simulation file as a string.
#Output: a list with a branched structure representing the path tree of Quantities in the simulation file.  At the end of each branch is a string called 'path' that is the path of the quantity represented by the branch.
currentEnv <- loadSimulation(simulationFilePath)
allPathStrings <- getAllPathStrings(currentEnv)

pathEnumList<-list() #Initiate list to be returned as a null list.
for (str in allPathStrings){
  pathArray <-  toPathArray(str) #Convert the path string to a vector of strings, each representing a branch portion.
  pathArray <- tail(pathArray,-1) #Remove the first element of the new vector, which is the name of the simulation.
  pathEnumList <- nextStep(pathEnumList,pathArray,pathArray) #Begin recursive loop to generate branched list.
}

return(pathEnumList)
}
