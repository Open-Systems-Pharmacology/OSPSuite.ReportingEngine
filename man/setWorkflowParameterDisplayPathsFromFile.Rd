% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-workflow.R
\name{setWorkflowParameterDisplayPathsFromFile}
\alias{setWorkflowParameterDisplayPathsFromFile}
\title{setWorkflowParameterDisplayPathsFromFile}
\usage{
setWorkflowParameterDisplayPathsFromFile(fileName, workflow)
}
\arguments{
\item{fileName}{name of file that includes mapping of Parameters with their display paths
Names in header should include \code{parameter} and \code{displayPath}.}

\item{workflow}{Object of class \code{MeanModelWorkflow} or \code{PopulationWorkflow}}
}
\description{
Set mapping between parameters and their display paths in a workflow
to replace standard display of parameter paths.
}
\seealso{
Other workflow helpers: 
\code{\link{activateWorkflowTasks}()},
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
\code{\link{setXParametersForDemographyPlot}()},
\code{\link{setXParametersForPKParametersPlot}()},
\code{\link{setYParametersForDemographyPlot}()},
\code{\link{setYParametersForPKParametersPlot}()}
}
\concept{workflow helpers}
