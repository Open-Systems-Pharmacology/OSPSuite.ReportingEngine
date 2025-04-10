% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/population-plot-task.R
\name{PopulationPlotTask}
\alias{PopulationPlotTask}
\title{PopulationPlotTask}
\description{
R6 class for PopulationPlotTask settings
}
\seealso{
Other workflow tasks: 
\code{\link{GofPlotTask}},
\code{\link{PlotTask}},
\code{\link{PopulationSensitivityAnalysisTask}},
\code{\link{QualificationTask}},
\code{\link{SensitivityAnalysisTask}},
\code{\link{Task}},
\code{\link{addUserDefinedTask}()},
\code{\link{loadCalculatePKParametersTask}()},
\code{\link{loadCalculateSensitivityTask}()},
\code{\link{loadGOFMergedTask}()},
\code{\link{loadPlotAbsorptionTask}()},
\code{\link{loadPlotDDIRatioTask}()},
\code{\link{loadPlotDemographyTask}()},
\code{\link{loadPlotMassBalanceTask}()},
\code{\link{loadPlotPKParametersTask}()},
\code{\link{loadPlotPKRatioTask}()},
\code{\link{loadPlotSensitivityTask}()},
\code{\link{loadPlotTimeProfilesAndResidualsTask}()},
\code{\link{loadQualificationComparisonTimeProfileTask}()},
\code{\link{loadQualificationTimeProfilesTask}()},
\code{\link{loadSimulateTask}()}
}
\concept{workflow tasks}
\section{Super classes}{
\code{ospsuite.reportingengine::Task} -> \code{ospsuite.reportingengine::PlotTask} -> \code{PopulationPlotTask}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{workflowType}}{Type of population workflow}

\item{\code{xParameters}}{list of parameter paths to be plotted along x-axis}

\item{\code{yParameters}}{list of parameter paths to be plotted along y-axis}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PopulationPlotTask-new}{\code{PopulationPlotTask$new()}}
\item \href{#method-PopulationPlotTask-saveResults}{\code{PopulationPlotTask$saveResults()}}
\item \href{#method-PopulationPlotTask-runTask}{\code{PopulationPlotTask$runTask()}}
\item \href{#method-PopulationPlotTask-clone}{\code{PopulationPlotTask$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="ospsuite.reportingengine" data-topic="Task" data-id="activate"><a href='../../ospsuite.reportingengine/html/Task.html#method-Task-activate'><code>ospsuite.reportingengine::Task$activate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ospsuite.reportingengine" data-topic="Task" data-id="getAbsolutePath"><a href='../../ospsuite.reportingengine/html/Task.html#method-Task-getAbsolutePath'><code>ospsuite.reportingengine::Task$getAbsolutePath()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ospsuite.reportingengine" data-topic="Task" data-id="getInputs"><a href='../../ospsuite.reportingengine/html/Task.html#method-Task-getInputs'><code>ospsuite.reportingengine::Task$getInputs()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ospsuite.reportingengine" data-topic="Task" data-id="getRelativePath"><a href='../../ospsuite.reportingengine/html/Task.html#method-Task-getRelativePath'><code>ospsuite.reportingengine::Task$getRelativePath()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ospsuite.reportingengine" data-topic="Task" data-id="inactivate"><a href='../../ospsuite.reportingengine/html/Task.html#method-Task-inactivate'><code>ospsuite.reportingengine::Task$inactivate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ospsuite.reportingengine" data-topic="Task" data-id="print"><a href='../../ospsuite.reportingengine/html/Task.html#method-Task-print'><code>ospsuite.reportingengine::Task$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ospsuite.reportingengine" data-topic="Task" data-id="validateInput"><a href='../../ospsuite.reportingengine/html/Task.html#method-Task-validateInput'><code>ospsuite.reportingengine::Task$validateInput()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ospsuite.reportingengine" data-topic="Task" data-id="validateStructureSetInput"><a href='../../ospsuite.reportingengine/html/Task.html#method-Task-validateStructureSetInput'><code>ospsuite.reportingengine::Task$validateStructureSetInput()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PopulationPlotTask-new"></a>}}
\if{latex}{\out{\hypertarget{method-PopulationPlotTask-new}{}}}
\subsection{Method \code{new()}}{
Create a \code{PopulationPlotTask} object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PopulationPlotTask$new(
  workflowType = PopulationWorkflowTypes$parallelComparison,
  xParameters = NULL,
  yParameters = NULL,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{workflowType}}{Type of population workflow. Use enum \code{PopulationWorkflowTypes} to get list of workflow types.}

\item{\code{xParameters}}{list of parameter paths to be plotted along x-axis}

\item{\code{yParameters}}{list of parameter paths to be plotted along y-axis}

\item{\code{...}}{input parameters inherited from \code{PlotTask} R6 class}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{PopulationPlotTask} object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PopulationPlotTask-saveResults"></a>}}
\if{latex}{\out{\hypertarget{method-PopulationPlotTask-saveResults}{}}}
\subsection{Method \code{saveResults()}}{
Save the task results
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PopulationPlotTask$saveResults(taskResults)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{taskResults}}{list of \code{TaskResults} objects}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PopulationPlotTask-runTask"></a>}}
\if{latex}{\out{\hypertarget{method-PopulationPlotTask-runTask}{}}}
\subsection{Method \code{runTask()}}{
Run task and save its output
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PopulationPlotTask$runTask(structureSets)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{structureSets}}{list of \code{SimulationStructure} R6 class}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PopulationPlotTask-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PopulationPlotTask-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PopulationPlotTask$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
