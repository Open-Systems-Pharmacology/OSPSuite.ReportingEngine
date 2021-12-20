#' @title getClassAncestry
#' @description function to get extend the head of the vector of classes `clsVec` with the ancestral classes of the first element fo `clsVec`
#' @param clsVec is a vector of R6 class objects
getClassAncestry <- function(clsVec){

  #Ensure that `clsVec` is a vector of classes that can be indexed
  clsVec <- c(clsVec)

  #Get the parent class of the first element in the vector of classes `clsVec`
  nextParent <- clsVec[[1]]$get_inherit()

  #If nextParent is NULL, then the first element in the vector of classes `clsVec` has no parent class, so return `clsVec`.
  if(is.null( nextParent )){
    return(clsVec)
  }

  #If nextParent is not NULL, run this function recursively to obtain a vector of the ancestors of nextParent
  return(getClassAncestry( c(nextParent,clsVec) ))
}


#' @title getAncestralInitializerList
#' @description function to get a list of the initializers of the R6 class `classObject` and all of its ancestor classes
#' @param classObject is an R6 class object
getAncestralInitializerList <- function(classObject){

  #Build a vector of the ancestor classes of `classObject`, starting with the most base class and ending with `classObject`.
  classAncestry <- getClassAncestry(classObject)

  #Loop through the vector of ancestor classes of `classObject` (`classAncestry`) and return a list of their initializer functions
  ancestralInitializerList <- lapply(classAncestry,function(cls){ cls$public_methods$initialize })

  return(ancestralInitializerList)
}


#' @title parseFunctionBody
#' @description function to cast the commands of the function `functionToParse` as a list of semicolon-separated strings
#' @param `functionToParse` a function the body of which is to be converted to a list of semicolon-separated strings
parseFunctionBody <- function(functionToParse) {
  ospsuite.utils::validateIsIncluded(values = typeof(functionToParse),parentValues= c("closure","function"))
  #read body of functionToParse, convert to character, removing first element (curly brackets), paste together all function commands into a string (separated by `;`)
  return(paste(tail(as.character(body(functionToParse)), -1), collapse = ";"))
}


#' @title makeChildInitializer
#' @description a function to generate an initializer for a child class that executes the initializers of its ancestral classes followed by an extending initializer `extendedInitializer`
#' @param parentClass the parent class of the child class for which the initializer is to be generated.
#' @param extendedInitializer is an initializer function to be called after calling all parent classes by order of superiority
makeChildInitializer <- function(parentClass, extendedInitializer) {
  parentInitializersList <- getAncestralInitializerList(parentClass)
  return(generateInitializer(parentInitializersList,extendedInitializer))
}


#' @title generateInitializer
#' @description function that recursively builds a combined initializer based on a list of parent class initializers (`parentInitializersList`) and an extending initializer (`extendedInitializer`).
#' @param parentInitializersList is an list of parent initializers. ordered from the most to the least superior.
#' @param extendedInitializer is an initializer function to be called after calling all parent classes by order of superiority
generateInitializer <- function(parentInitializersList, extendedInitializer){

  ospsuite.utils::validateIsOfType(parentInitializersList,"list")
  sapply(parentInitializersList,function(fn){ ospsuite.utils::validateIsIncluded(values = typeof(fn),parentValues= c("closure","function")) })
  ospsuite.utils::validateIsIncluded(values = typeof(extendedInitializer),parentValues= c("closure","function"))

  #Recursive application of this function for cases in which there is multilevel inheritance, where parentInitializersList
  #Each recursion will return an `extendedInitializer` that is an amalgamation of the all but the first elements of parentInitializersList with extendedInitializer
  if (length(parentInitializersList) > 1) {
    extendedInitializer <- generateInitializer(tail(parentInitializersList, -1), extendedInitializer)
  }

  #If parentInitializersList includes only one element, amalgamate that function with extendedInitializer
  parentInitializer <- parentInitializersList[[1]]

  #Parse the body of the parent and child initializers into a series of commands (as strings) separated by semicolons (';')
  parentInitializerBody <- parseFunctionBody(parentInitializer)
  extendedInitializerBody <- parseFunctionBody(extendedInitializer)

  #amalgamate (as a string) the bodies of the parent and child classes into a new function with no input arguments
  childInitializerBody <- paste0("function(){
       eval(parse(text = '", parentInitializerBody, "' ))
       eval(parse(text = '", extendedInitializerBody, "' ))
     }")

  #create a function object based on the string `childInitializerBody``
  childInitializer <- eval(parse(text = childInitializerBody))

  # Set the arguments to the childInitializer function to be the union of the arguments of the `parentInitializer` function and the `extendedInitializer` function
  # Remove any duplicated arguments.  Arguments of the extendedInitializer overwrite arguments of the parentInitializer with the same name.
  childInitializerFormals <- c(formals(parentInitializer), formals(extendedInitializer))
  formals(childInitializer) <- childInitializerFormals[!duplicated(names(childInitializerFormals), fromLast = TRUE)]

  return(childInitializer)
}



