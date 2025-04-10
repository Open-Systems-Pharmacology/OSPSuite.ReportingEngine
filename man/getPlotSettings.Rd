% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-qualification.R
\name{getPlotSettings}
\alias{getPlotSettings}
\title{getPlotSettings}
\usage{
getPlotSettings(plotSettingsFromConfigurationPlot)
}
\arguments{
\item{plotSettingsFromConfigurationPlot}{is a field from the \code{configurationPlan$plots} list}
}
\value{
\code{plotSettings}, a list of settings for each of the X and Y axis.  Each list contains the unit, dimensions, and scaling type for each axes and option to plot grid lines.
}
\description{
Read plot settings from configuration plan.
}
\keyword{internal}
