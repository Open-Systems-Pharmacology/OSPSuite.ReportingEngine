#' @title getClassAncestry
#' @description function to extend the head of the vector of classes `clsVec` with the ancestral classes of the first element fo `clsVec`
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
#' @description a function to generate an initializer for a child class that executes the initializer function `preSuperInitializer`, followed by the initializers of `parentClass` and its ancestral classes, followed by the function `postSuperInitializer`
#' @param preSuperInitializer is an initializer function to be called BEFORE calling all parent classes by order of superiority
#' @param parentClass the parent class of the child class for which the initializer is to be generated.
#' @param postSuperInitializer is an initializer function to be called AFTER calling all parent classes by order of superiority
makeChildInitializer <- function(parentClass = NULL, preSuperInitializer = NULL, postSuperInitializer = NULL) {
  parentInitializersList <- ospsuite.utils::ifNotNull(condition = parentClass, outputIfNotNull = getAncestralInitializerList(parentClass),outputIfNull = list(function(){}))
  return(generateInitializer(parentInitializersList = parentInitializersList,
                             preSuperInitializer = preSuperInitializer %||% function(){},
                             postSuperInitializer = postSuperInitializer  %||% function(){}))
}


#' @title generateInitializer
#' @description function that recursively builds a combined initializer based on a list of parent class initializers (`parentInitializersList`) and an extending initializer (`postSuperInitializer`).
#' @param preSuperInitializer is an initializer function to be called BEFORE calling all parent classes by order of superiority
#' @param parentInitializersList is an list of parent initializers. ordered from the most to the least superior.
#' @param postSuperInitializer is an initializer function to be called AFTER calling all parent classes by order of superiority
generateInitializer <- function(parentInitializersList, preSuperInitializer, postSuperInitializer = function(){}){

  ospsuite.utils::validateIsOfType(parentInitializersList,"list")
  sapply(parentInitializersList,function(fn){ ospsuite.utils::validateIsIncluded(values = typeof(fn),parentValues= c("closure","function")) })
  ospsuite.utils::validateIsIncluded(values = typeof(postSuperInitializer),parentValues= c("closure","function"))

  #Recursive application of this function for cases in which there is multilevel inheritance, where parentInitializersList
  #Each recursion will return an `postSuperInitializer` that is an amalgamation of the all but the first elements of parentInitializersList with postSuperInitializer
  if (length(parentInitializersList) > 1) {
    postSuperInitializer <- generateInitializer(parentInitializersList = tail(parentInitializersList, -1),
                                               preSuperInitializer = function(){},
                                               postSuperInitializer = postSuperInitializer)
  }

  #If parentInitializersList includes only one element, amalgamate that function with postSuperInitializer
  parentInitializer <- parentInitializersList[[1]]

  #Parse the body of the parent and child initializers into a series of commands (as strings) separated by semicolons (';')
  preSuperInitializerBody <- parseFunctionBody(preSuperInitializer)
  parentInitializerBody <- parseFunctionBody(parentInitializer)
  postSuperInitializerBody <- parseFunctionBody(postSuperInitializer)

  #amalgamate (as a string) the bodies of the parent and child classes into a new function with no input arguments
  childInitializerBody <- paste0("function(){
      eval(parse(text = '", preSuperInitializerBody, "' ))
      eval(parse(text = '", parentInitializerBody, "' ))
      eval(parse(text = '", postSuperInitializerBody, "' ))
     }")

  #create a function object based on the string `childInitializerBody``
  childInitializer <- eval(parse(text = childInitializerBody))
  # Set the arguments to the childInitializer function to be the union of the arguments of the `parentInitializer` function and the `postSuperInitializer` function
  # Remove any duplicated arguments.  Arguments of the postSuperInitializer overwrite arguments of the parentInitializer with the same name.
  childInitializerFormals <- c(formals(parentInitializer), formals(preSuperInitializer), formals(postSuperInitializer))
  formals(childInitializer) <- childInitializerFormals[!duplicated(names(childInitializerFormals), fromLast = TRUE)]

  return(childInitializer)
}



