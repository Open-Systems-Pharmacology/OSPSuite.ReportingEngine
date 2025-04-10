% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-ratio-comparison.R
\name{getPKRatioSummaryFromMCSampling}
\alias{getPKRatioSummaryFromMCSampling}
\title{getPKRatioSummaryFromMCSampling}
\usage{
getPKRatioSummaryFromMCSampling(
  pkData,
  referencePKData,
  simulationSetName,
  settings = NULL
)
}
\arguments{
\item{pkData}{A data.frame of PK Parameter values for Population to compare}

\item{referencePKData}{A data.frame of PK Parameter values for reference Population}

\item{simulationSetName}{Name of simulation set}

\item{settings}{A list of task settings}
}
\value{
A data.frame of the PK Parameter ratios summary statistics
}
\description{
Get PK Parameter Ratio Measure From Monte Carlo Sampling
}
\keyword{internal}
