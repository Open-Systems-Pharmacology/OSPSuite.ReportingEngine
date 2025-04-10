% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-pop-pk-parameters.R
\name{getPopulationPKAnalysesFromOutput}
\alias{getPopulationPKAnalysesFromOutput}
\title{getPopulationPKAnalysesFromOutput}
\usage{
getPopulationPKAnalysesFromOutput(
  data,
  metaData,
  output,
  pkParameter,
  molWeight = NULL
)
}
\arguments{
\item{data}{data.frame of the PK Analyses across Population Simulation sets}

\item{metaData}{metaData (dimension and unit) of the PK Analyses across Population Simulation sets}

\item{output}{An \code{Output } object}

\item{pkParameter}{\code{pkParameter} from \code{Output } object}

\item{molWeight}{Molecular weight of compound (if unit conversion needed)}
}
\value{
list of data.frame and its metaData including the values of PK parameters specified by \code{pkParameter} and \code{Output} objects
}
\description{
Get the values of PK parameters specified by an \code{Output} object from a data.frame
}
\keyword{internal}
