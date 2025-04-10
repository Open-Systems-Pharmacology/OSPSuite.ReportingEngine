% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qualification-outputs.R
\name{addNewPkParameter}
\alias{addNewPkParameter}
\title{addNewPkParameter}
\usage{
addNewPkParameter(pkParameter, startTime, endTime)
}
\arguments{
\item{pkParameter}{the name of the PK parameter from the qualification \code{ConfigurationPlan}}

\item{startTime}{the starting time of the interval over which the PK parameter is calculated (from the qualification \code{ConfigurationPlan})}

\item{endTime}{the ending time of the interval over which the PK parameter is calculated (from the qualification \code{ConfigurationPlan})}
}
\value{
String \code{pkParameterName}
}
\description{
Create a PK parameter calculated between a start and end time as specified in a qualification \code{ConfigurationPlan} and return the PK parameter name
}
\keyword{internal}
