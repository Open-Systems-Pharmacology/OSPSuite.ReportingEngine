% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-simulation-set.R
\name{loadSimulationWithUpdatedPaths}
\alias{loadSimulationWithUpdatedPaths}
\title{loadSimulationWithUpdatedPaths}
\usage{
loadSimulationWithUpdatedPaths(
  simulationSet,
  loadFromCache = FALSE,
  addToCache = TRUE
)
}
\arguments{
\item{simulationSet}{simulation set containing path to simulation file and pathIDs for quantities to be loaded into simulation object}

\item{loadFromCache}{If \code{TRUE}, an already loaded pkml file will not be loaded again, but the \code{Simulation} object will be retrieved from cache.
If \code{FALSE}, a new \code{Simulation} object will be created. Default value is \code{FALSE}.}

\item{addToCache}{If \code{TRUE}, the loaded simulation is added to cache.
If \code{FALSE}, the returned simulation only exists locally. Default is \code{TRUE}.}
}
\value{
A \code{Simulation} object with pathIDs updated from simulationSet
}
\description{
loadSimulationWithUpdatedPaths
}
