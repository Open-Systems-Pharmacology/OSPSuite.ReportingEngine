% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{parseVariableToObject}
\alias{parseVariableToObject}
\title{parseVariableToObject}
\usage{
parseVariableToObject(objectName, variableName, keepIfNull = FALSE)
}
\arguments{
\item{objectName}{Name of the object whose field is updated}

\item{variableName}{Name of the variable and field of \code{objectName}}

\item{keepIfNull}{logical \verb{objectName$variableName <- variableName \\\%||\\\% objectName$variableName}}
}
\value{
An expression to \code{eval()}
}
\description{
Create an expression of type \code{objectName$variableName <- variableName}
}
\keyword{internal}
