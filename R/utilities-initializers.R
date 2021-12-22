#' @title parseFunctionBody
#' @description function to cast the commands of the function `functionToParse` as a list of semicolon-separated strings
#' @param `functionToParse` a function the body of which is to be converted to a list of semicolon-separated strings
#' @keywords internal
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
#' @return `childInitializer`, an initializer function
#' @keywords internal
makeChildInitializer <- function(parentClass = NULL, preSuperInitializer = NULL, postSuperInitializer = NULL) {
  parentInitializer <- ospsuite.utils::ifNotNull(condition = parentClass,
                                                 outputIfNotNull = parentClass$public_methods$initialize,
                                                 outputIfNull = function(){})
  childInitializer <- generateInitializer(preSuperInitializer = preSuperInitializer %||% function(){},
                                          parentInitializer = parentInitializer,
                                          postSuperInitializer = postSuperInitializer  %||% function(){})

  return(childInitializer)
}


#' @title generateInitializer
#' @description function that a combined initializer that is a sequence of: a `preSuperInitializer` method, a parent class initializer (`parentInitializersList`) and a `postSuperInitializer` method.
#' @param preSuperInitializer is an initializer function to be called BEFORE calling all parent classes by order of superiority
#' @param parentInitializer is the parent class initializer.
#' @param postSuperInitializer is an initializer function to be called AFTER calling all parent classes by order of superiority
#' @return `childInitializer`, an initializer function
#' @keywords internal
generateInitializer <- function(preSuperInitializer, parentInitializer, postSuperInitializer){

  ospsuite.utils::validateIsIncluded(values = typeof(preSuperInitializer), parentValues= c("closure","function"))
  ospsuite.utils::validateIsIncluded(values = typeof(parentInitializer), parentValues= c("closure","function"))
  ospsuite.utils::validateIsIncluded(values = typeof(postSuperInitializer), parentValues= c("closure","function"))

  #Parse the body of the parent and child initializers into a series of commands (as strings) separated by semicolons (';')
  preSuperInitializerBody <- parseFunctionBody(preSuperInitializer)
  parentInitializerBody <- parseFunctionBody(parentInitializer)
  postSuperInitializerBody <- parseFunctionBody(postSuperInitializer)

  # Set the arguments to the childInitializer function to be the union of the arguments of the `parentInitializer` function and the `postSuperInitializer` function
  # Remove any duplicated arguments.  Arguments of the postSuperInitializer overwrite arguments of the parentInitializer with the same name.
  childInitializerFormals <- c(formals(parentInitializer), formals(preSuperInitializer), formals(postSuperInitializer))
  childInitializerFormals <- childInitializerFormals[!duplicated(names(childInitializerFormals), fromLast = TRUE)]

  #amalgamate (as a string) the bodies of the parent and child classes into a new function with no input arguments
  childInitializerBody <- paste0("function(){
      eval(parse(text = '", preSuperInitializerBody, "' ))
      eval(parse(text = '", parentInitializerBody, "' ))
      eval(parse(text = '", postSuperInitializerBody, "' ))
     }")

  #create a function object based on the string `childInitializerBody``
  childInitializer <- eval(parse(text = childInitializerBody))
  formals(childInitializer) <- childInitializerFormals

  return(childInitializer)
}



