#' @title updateSimulationTimesFromConfigurationPlan
#' @description Read simulation run times from `ConfigurationPlan` and update them accordingly in simulation object
#' @param simulation path of the output folder created or used by the Workflow.
#' @param configurationPlan The configuration plan of a Qualification workflow read from json file.
updateSimulationTimesFromConfigurationPlan <- function(simulation,
                                                       configurationPlan){

}


#' @title getOutputsFromConfigurationPlan
#' @description Get a list of outputs from simulation and from `ConfigurationPlan`
#' @param simulation path of the output folder created or used by the Workflow.
#' @param configurationPlan The configuration plan of a Qualification workflow read from json file.
#' @return A list of `outputs`
getOutputsFromConfigurationPlan <- function(simulation,
                                            configurationPlan){

  outputs <- lapply(simulation$outputSelections$allOutputs, function(output) {
    Output$new(output$path)
  })

  outputsDDI <- getDDIOutputs(simulation = simulation,
                              configurationPlan = configurationPlan)

  outputs <- c(outputs,outputsDDI)

  return(outputs)
}




#' @title getDDIOutputsDataframe
#' @description Get a dataframe relating project, simulation, output, pk parameter, start time, end time for each DDI plot component
#' @param configurationPlan The configuration plan of a Qualification workflow read from json file.
#' @return A dataframe containing data for generating DDI plots
getDDIOutputsDataframe <- function(configurationPlan){
  ddiOutputsDataframe <- NULL
  for (plot in configurationPlan$plots$DDIRatioPlots){
    pkParameters <- toPathArray(plot$PKParameter)
    for (group in plot$Groups){
      for (ddiRatio in group$DDIRatios){
        outputPath <- ddiRatio$Output
        for (simulationType in c("SimulationControl","SimulationDDI")){
          plotComponent <- ddiRatio[[simulationType]]
          df <- data.frame(
            project = plotComponent$Project,
            simulation = plotComponent$Simulation,
            outputPath = outputPath,
            startTime = toBaseUnit(quantityOrDimension = ospDimensions$Time,
                                   values = plotComponent$StartTime,
                                   unit = plotComponent$TimeUnit),
            endTime = toBaseUnit(quantityOrDimension = ospDimensions$Time,
                                 values = plotComponent$EndTime,
                                 unit = plotComponent$TimeUnit),
            pkParameter = pkParameters)
          ddiOutputsDataframe <- rbind.data.frame(ddiOutputsDataframe,df)
        }
      }
    }
  }
  return(ddiOutputsDataframe)
}

