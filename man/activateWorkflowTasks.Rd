% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-task.R
\name{activateWorkflowTasks}
\alias{activateWorkflowTasks}
\title{activateWorkflowTasks}
\usage{
activateWorkflowTasks(workflow, tasks = workflow$getAllTasks())
}
\arguments{
\item{workflow}{A \code{Workflow} object}

\item{tasks}{names of the tasks to activate
Default activates all tasks of the workflow using workflow method \code{workflow$getAllTasks()}}
}
\description{
activates a series of \code{Tasks} from a \code{Workflow}
}
\examples{
\dontrun{

# A workflow object needs to be created first
myWorkflow <- MeanModelWorkflow$new(workflowFolder, simulationSets)

# Use enum helper to get task names
activateWorkflowTasks(
  workflow = myWorkflow,
  tasks = StandardSimulationTasks$simulate
)

# Default will activate every task of workflow
activateWorkflowTasks(workflow = myWorkflow)
}

}
\seealso{
Other workflow helpers: 
\code{\link{addStudyParameters}()},
\code{\link{addXParametersForDemographyPlot}()},
\code{\link{addXParametersForPKParametersPlot}()},
\code{\link{addYParametersForDemographyPlot}()},
\code{\link{addYParametersForPKParametersPlot}()},
\code{\link{checkTaskInputsExist}()},
\code{\link{createWorkflowFromExcelInput}()},
\code{\link{getMeanSensitivityAnalysisResultsFileNames}()},
\code{\link{getOutputPathsInSimulationSet}()},
\code{\link{getPKAnalysisResultsFileNames}()},
\code{\link{getPKParameterGroupsInOutput}()},
\code{\link{getPKParametersInOutput}()},
\code{\link{getPKParametersInSimulationSet}()},
\code{\link{getPopulationPKData}()},
\code{\link{getPopulationPKMetaData}()},
\code{\link{getPopulationSensitivityAnalysisResultsFileNames}()},
\code{\link{getSimulationDescriptor}()},
\code{\link{getSimulationParameterDisplayPaths}()},
\code{\link{getSimulationResultFileNames}()},
\code{\link{getTaskInputs}()},
\code{\link{getWorkflowParameterDisplayPaths}()},
\code{\link{getWorkflowReferencePopulationName}()},
\code{\link{getXParametersForDemographyPlot}()},
\code{\link{getXParametersForPKParametersPlot}()},
\code{\link{getYParametersForDemographyPlot}()},
\code{\link{getYParametersForPKParametersPlot}()},
\code{\link{inactivateWorkflowTasks}()},
\code{\link{setSimulationDescriptor}()},
\code{\link{setWorkflowParameterDisplayPaths}()},
\code{\link{setWorkflowParameterDisplayPathsFromFile}()},
\code{\link{setXParametersForDemographyPlot}()},
\code{\link{setXParametersForPKParametersPlot}()},
\code{\link{setYParametersForDemographyPlot}()},
\code{\link{setYParametersForPKParametersPlot}()}
}
\concept{workflow helpers}
