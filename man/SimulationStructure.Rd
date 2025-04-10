% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation-structure.R
\name{SimulationStructure}
\alias{SimulationStructure}
\title{SimulationStructure}
\description{
R6 class representing that stores file structure (file names and paths) relating to a particular instance of a simulationSet object
}
\keyword{internal}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{workflowFolder}}{root folder storing workflow results}

\item{\code{simulationSet}}{the set for which the SimulationStructure object stores paths input and results}

\item{\code{simulationResultsFolder}}{path to folder storing results of simulations}

\item{\code{pkAnalysisResultsFolder}}{path to folder storing results of pk analyses}

\item{\code{sensitivityAnalysisResultsFolder}}{path to folder storing results of sensitivity analyses}

\item{\code{simulationResultFileNames}}{vector of names of CSV files storing simulation results}

\item{\code{pkAnalysisResultsFileNames}}{vector of names of CSV files storing results of pk analyses}

\item{\code{pkRatioResultsFileNames}}{vector of names of CSV files storing results of pk ratios}

\item{\code{sensitivityAnalysisResultsFileNames}}{vector of names of CSV files storing results of sensitivity analyses}

\item{\code{popSensitivityAnalysisResultsIndexFile}}{path to file containing index of population sensitivity analysis results}

\item{\code{parameterDisplayPaths}}{data.frame mapping parameters to user-defined display paths}

\item{\code{simulationSetDescriptor}}{Descriptor of simulation sets indicated in reports}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-SimulationStructure-new}{\code{SimulationStructure$new()}}
\item \href{#method-SimulationStructure-clone}{\code{SimulationStructure$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SimulationStructure-new"></a>}}
\if{latex}{\out{\hypertarget{method-SimulationStructure-new}{}}}
\subsection{Method \code{new()}}{
Create a new \code{SimulationStructure} object.
Build and store names of potential subfolders to hold simulation results, pkAnalysis results and sensitivityAnalysis results.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationStructure$new(simulationSet, workflowFolder = getwd())}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{simulationSet}}{A \code{SimulationSet} or \code{PopulationSimulationSet} object}

\item{\code{workflowFolder}}{output folder of the workflow}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SimulationStructure-clone"></a>}}
\if{latex}{\out{\hypertarget{method-SimulationStructure-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationStructure$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
