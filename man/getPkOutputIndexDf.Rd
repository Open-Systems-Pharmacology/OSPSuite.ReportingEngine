% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-sensitivity-analysis.R
\name{getPkOutputIndexDf}
\alias{getPkOutputIndexDf}
\title{getPkOutputIndexDf}
\usage{
getPkOutputIndexDf(indexDf, pkParameter, output)
}
\arguments{
\item{indexDf}{dataframe containing summary of sensitivity results}

\item{pkParameter}{name of PK parameter for which to obtain the population sensitivity results}

\item{output}{pathID of output for which to obtain the population sensitivity results}
}
\value{
pkOutputIndexDf dataframe containing index of files containing population sensitivity analysis results conducted for given output and pkParameter
}
\description{
Function to filter the population results index file for given pkParameter and output
}
\keyword{internal}
