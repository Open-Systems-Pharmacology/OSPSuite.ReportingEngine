% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation-settings.R
\name{SimulationSettings}
\alias{SimulationSettings}
\title{SimulationSettings}
\description{
R6 class for Population Simulation Settings
}
\keyword{internal}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{numberOfCores}}{is the number of cores to use for simulation}

\item{\code{showProgress}}{is a logical field  TRUE shows progress of simulation.}

\item{\code{maxSimulationsPerCore}}{Scale factor used in a parallel simulation.  The product of this scale factor and the number of allowable cores (allowedCores) sets the maximum number of simulations that may be run on one core.}

\item{\code{allowedCores}}{is the number of cores assigned to the user session.}

\item{\code{mcRepetitions}}{is the number of repetitions when performing a Monte Carlo Simulation}

\item{\code{mcRandomSeed}}{is the Random Seed Number when performing a Monte Carlo Simulation
which allows repeatability of the simulations}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-SimulationSettings-new}{\code{SimulationSettings$new()}}
\item \href{#method-SimulationSettings-clone}{\code{SimulationSettings$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SimulationSettings-new"></a>}}
\if{latex}{\out{\hypertarget{method-SimulationSettings-new}{}}}
\subsection{Method \code{new()}}{
Create a \code{SimulationSettings} object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationSettings$new(
  numberOfCores = NULL,
  showProgress = TRUE,
  maxSimulationsPerCore = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{numberOfCores}}{number of cores for parallel computation}

\item{\code{showProgress}}{logical indicating if simulation progress is printed on console}

\item{\code{maxSimulationsPerCore}}{Scale factor used in a parallel simulation.  The product of this scale factor and the number of allowable cores (allowedCores) sets the maximum number of simulations that may be run on one core.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{SimulationSettings} object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SimulationSettings-clone"></a>}}
\if{latex}{\out{\hypertarget{method-SimulationSettings-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationSettings$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
