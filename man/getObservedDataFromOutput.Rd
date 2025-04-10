% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-observed-data.R
\name{getObservedDataFromOutput}
\alias{getObservedDataFromOutput}
\title{getObservedDataFromOutput}
\usage{
getObservedDataFromOutput(output, data, dataMapping, molWeight, structureSet)
}
\arguments{
\item{output}{An \code{Output} object}

\item{data}{A data.frame}

\item{dataMapping}{A list mapping the variable of data}

\item{molWeight}{Molar weight for unit conversion of dependent variable}

\item{structureSet}{A \code{SimulationStructure} object}
}
\value{
list of data and its metaData
}
\description{
Get selected observed data from an Output object
}
\keyword{internal}
