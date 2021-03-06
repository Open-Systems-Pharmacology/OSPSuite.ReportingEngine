% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-sensitivity-analysis.R
\name{analyzeSensitivity}
\alias{analyzeSensitivity}
\title{analyzeSensitivity}
\usage{
analyzeSensitivity(simulation, settings = settings, logFolder = getwd())
}
\arguments{
\item{simulation}{simulation class object}

\item{settings}{list of settings for the sensitivity analysis}

\item{logFolder}{folder where the logs are saved}
}
\value{
sensitivity analysis results
}
\description{
Run a sensitivity analysis for a single individual,
varying only the set of parameters variableParameterPaths
}
